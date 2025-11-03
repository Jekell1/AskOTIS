"""Supervisor for multi-shard symbol reference embedding backfill.

Features:
  * Launches N shard processes of `backfill_symbol_ref_embeddings.py`.
  * Assigns each shard unique snapshot + checkpoint files.
  * Auto-restarts shards that exit non‑zero (configurable max retries / backoff).
  * Periodically aggregates shard progress (reads snapshot JSON) and prints a combined view.
  * Detects vector quota exhaustion marker ('.vector_quota_exceeded') and aborts remaining shards cleanly.
  * Graceful Ctrl+C handling (terminates children, prints final partial summary).

Usage Example:
    python ops/supervise_symbol_ref_backfill.py \
        --shards 4 --embed-batch 64 --batch 256 --dimension 1536 \
        --snapshot-every-batches 20 --poll-interval 60

Pass-through extra args to shard script with -- extra flags:
    python ops/supervise_symbol_ref_backfill.py --shards 4 -- --stop-percent 100

Environment:
  Set SYMBOL_REF_VECTOR_DIM automatically when --dimension provided (if not already set).

Exit Codes:
  0 = all shards completed successfully
  2 = aborted due to vector quota marker
  3 = exceeded restart budget for one or more shards
  130 = interrupted by user

"""
from __future__ import annotations
import argparse, subprocess, time, json, signal, os, sys, pathlib, shutil, threading
from typing import Dict, List, Optional

SCRIPT = 'backfill_symbol_ref_embeddings.py'
VECTOR_QUOTA_MARKER = '.vector_quota_exceeded'

class ShardState:
    def __init__(self, index:int):
        self.index = index
        self.process: Optional[subprocess.Popen] = None
        self.restarts = 0
        self.last_start = 0.0
        self.done = False
        self.exit_code: Optional[int] = None
        self.snapshot_path: Optional[pathlib.Path] = None
        self.checkpoint_path: Optional[pathlib.Path] = None

def build_args(base_cmd: List[str], shard_index:int, args) -> List[str]:
    shard_args = [
        '--hex-shards', str(args.shards),
        '--hex-shard-index', str(shard_index),
        '--embed-batch', str(args.embed_batch),
        '--batch', str(args.batch),
        '--snapshot-every-batches', str(args.snapshot_every_batches),
        '--snapshot-file', f'.symbol_ref_backfill_shard{shard_index}.json',
        '--checkpoint-file', f'.symbol_ref_backfill_checkpoint_shard{shard_index}.json'
    ]
    if args.stop_percent is not None:
        shard_args += ['--stop-percent', str(args.stop_percent)]
    # Append any passthrough args after '--'
    full = base_cmd + shard_args + args.extra
    return full

def read_snapshot(path: pathlib.Path):
    if not path.is_file():
        return None
    try:
        return json.loads(path.read_text(encoding='utf-8'))
    except Exception:
        return None

def spawn_shard(state: ShardState, args, env) -> None:
    cmd = [sys.executable, SCRIPT]
    full_cmd = build_args(cmd, state.index, args)
    log_path = pathlib.Path(f'.symbol_ref_backfill_shard{state.index}.log')
    log_fp = open(log_path, 'a', encoding='utf-8')
    log_fp.write(f"\n=== (re)start shard {state.index} at {time.strftime('%Y-%m-%d %H:%M:%S')} CMD={' '.join(full_cmd)} ===\n")
    log_fp.flush()
    state.last_start = time.time()
    state.snapshot_path = pathlib.Path(f'.symbol_ref_backfill_shard{state.index}.json')
    state.checkpoint_path = pathlib.Path(f'.symbol_ref_backfill_checkpoint_shard{state.index}.json')
    state.process = subprocess.Popen(full_cmd, stdout=log_fp, stderr=log_fp, env=env)

def aggregate(shards: List[ShardState]):
    total_baseline = 0
    total_embedded = 0
    per = []
    for s in shards:
        snap = read_snapshot(s.snapshot_path) if s.snapshot_path else None
        if snap and 'baseline_missing' in snap:
            baseline = snap.get('baseline_missing') or 0
            embedded = snap.get('embedded_so_far') or 0
            total_baseline += baseline
            total_embedded += embedded
            per.append((s.index, embedded, baseline))
    pct = (total_embedded/total_baseline*100) if total_baseline else 0.0
    return pct, total_embedded, total_baseline, per

def kill_all(shards: List[ShardState]):
    for s in shards:
        if s.process and s.process.poll() is None:
            try:
                s.process.terminate()
            except Exception:
                pass
    # Give a grace period then force kill
    time.sleep(2)
    for s in shards:
        if s.process and s.process.poll() is None:
            try:
                s.process.kill()
            except Exception:
                pass

def main():
    ap = argparse.ArgumentParser(description='Supervisor for symbol ref embedding shard backfill')
    ap.add_argument('--shards', type=int, default=4, help='Number of hex shards (1-16)')
    ap.add_argument('--embed-batch', type=int, default=64, help='Embedding batch size passed to shard script')
    ap.add_argument('--batch', type=int, default=256, help='Scan batch size passed to shard script')
    ap.add_argument('--dimension', type=int, default=1536, help='Vector dimension (sets SYMBOL_REF_VECTOR_DIM if unset)')
    ap.add_argument('--snapshot-every-batches', type=int, default=20, dest='snapshot_every_batches')
    ap.add_argument('--poll-interval', type=int, default=60, help='Seconds between aggregate progress prints')
    ap.add_argument('--max-restarts', type=int, default=5, help='Maximum auto restarts per shard')
    ap.add_argument('--restart-delay-base', type=float, default=10.0, help='Base seconds before restart; multiplied by restart count')
    ap.add_argument('--stop-percent', type=float, default=None, help='Optional shard stop-percent to pass through')
    ap.add_argument('--grace-after-finish', type=int, default=30, help='Seconds to continue monitoring after all shards done (in case of late writes)')
    ap.add_argument('--run-diagnose', action='store_true', help='After shards finish, run diagnose_symbol_ref_gaps.py (or provided script) to print final missing vector summary.')
    ap.add_argument('--diagnose-script', default='diagnose_symbol_ref_gaps.py', help='Path to diagnostic script that outputs JSON with missing_docs.')
    ap.add_argument('extra', nargs=argparse.REMAINDER, help="Extra args after '--' passed to shard script")
    args = ap.parse_args()

    if args.shards < 1 or args.shards > 16:
        ap.error('--shards must be between 1 and 16')

    # Clean '--' prefix in extra if present
    if args.extra and args.extra[0] == '--':
        args.extra = args.extra[1:]

    env = os.environ.copy()
    if 'SYMBOL_REF_VECTOR_DIM' not in env:
        env['SYMBOL_REF_VECTOR_DIM'] = str(args.dimension)

    shards: List[ShardState] = [ShardState(i) for i in range(args.shards)]
    quota_abort = False
    interrupted = False
    start_time = time.time()

    # Launch all shards initially
    for s in shards:
        spawn_shard(s, args, env)

    try:
        last_print = 0.0
        while True:
            time.sleep(1)
            # Quota detection
            if pathlib.Path(VECTOR_QUOTA_MARKER).is_file():
                quota_abort = True
                print('[SUP] Vector quota marker detected; aborting all shards.')
                break
            all_done = True
            for s in shards:
                if s.done:
                    continue
                proc = s.process
                if proc is None:
                    all_done = False
                    continue
                rc = proc.poll()
                if rc is None:
                    all_done = False
                    continue
                # Process exited
                s.exit_code = rc
                if rc == 0:
                    s.done = True
                    print(f"[SUP][shard {s.index}] completed successfully.")
                else:
                    if s.restarts >= args.max_restarts:
                        print(f"[SUP][shard {s.index}] exceeded max restarts (exit={rc}). Will not restart.")
                        s.done = True  # mark as terminal failure
                    else:
                        s.restarts += 1
                        delay = args.restart_delay_base * s.restarts
                        print(f"[SUP][shard {s.index}] exited code={rc}; scheduling restart #{s.restarts} in {delay:.1f}s")
                        def delayed_restart(state: ShardState, wait: float):
                            time.sleep(wait)
                            if pathlib.Path(VECTOR_QUOTA_MARKER).is_file():
                                return
                            if state.done:  # may have been marked done elsewhere
                                return
                            spawn_shard(state, args, env)
                        threading.Thread(target=delayed_restart, args=(s, delay), daemon=True).start()
                        all_done = False
            now = time.time()
            if now - last_print >= args.poll_interval:
                last_print = now
                pct, embedded, baseline, per = aggregate(shards)
                shard_summ = ', '.join([f"{idx}:{(e/b*100 if b else 0):.1f}%" for idx,e,b in per]) if per else 'no-snapshots'
                elapsed = now - start_time
                print(f"[SUP][agg] elapsed={elapsed/60:.1f}m shards_done={sum(1 for s in shards if s.done)}/{len(shards)} pct={pct:.4f}% embedded={embedded} baseline={baseline} per=[{shard_summ}]")
            if all_done:
                # Allow a short grace to catch late snapshot flushes
                grace_start = time.time()
                while time.time() - grace_start < args.grace_after_finish:
                    time.sleep(1)
                break
        # end main loop
    except KeyboardInterrupt:
        interrupted = True
        print('\n[SUP] Interrupt received; terminating shards...')
    finally:
        kill_all(shards)

    # Final aggregate
    pct, embedded, baseline, per = aggregate(shards)
    print(f"[SUP][final] pct={pct:.4f}% embedded={embedded} baseline={baseline} shard_status={[ (s.index, s.exit_code, s.restarts) for s in shards ]}")

    # Optional post-run diagnosis
    if args.run_diagnose:
        diag_path = pathlib.Path(args.diagnose_script)
        if not diag_path.is_file():
            print(f"[SUP][diag] Skipping diagnosis; script not found: {diag_path}")
        else:
            print(f"[SUP][diag] Running diagnosis script: {diag_path}")
            try:
                res = subprocess.run([sys.executable, str(diag_path)], capture_output=True, text=True, timeout=900)
                if res.returncode != 0:
                    print(f"[SUP][diag][WARN] Diagnosis script exited {res.returncode}. stderr={res.stderr.strip()[:300]}")
                else:
                    out = res.stdout.strip()
                    # Attempt JSON parse
                    try:
                        data = json.loads(out)
                        miss = data.get('missing_docs')
                        total = data.get('total_docs')
                        pct_miss = data.get('percent_missing')
                        print(f"[SUP][diag] Final verify: missing={miss} / {total} ({pct_miss}%)")
                        if miss == 0:
                            print("[SUP][diag] FULL COVERAGE ACHIEVED ✅")
                        else:
                            print("[SUP][diag] Incomplete coverage detected; consider a targeted non-force backfill for residual items.")
                    except Exception:
                        # Non-JSON output; print a snippet for manual review
                        print("[SUP][diag][WARN] Could not parse JSON from diagnosis output. Raw snippet:\n" + out[:500])
            except subprocess.TimeoutExpired:
                print("[SUP][diag][WARN] Diagnosis script timed out.")

    if interrupted:
        sys.exit(130)
    if quota_abort:
        sys.exit(2)
    # Any shard failed with non-zero and exhausted restarts?
    failed = [s for s in shards if (not s.done) or (s.exit_code not in (0, None) and s.restarts >= args.max_restarts)]
    if failed:
        sys.exit(3)
    sys.exit(0)

if __name__ == '__main__':
    main()
