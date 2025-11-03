"""Convenience launcher for parallel symbol ref embedding backfill shards.

Spawns N processes (shards) splitting ref_id space by first hex char ranges.

Usage:
  python launch_symbol_ref_shards.py --shards 8 --embed-batch 128 --text-max-len 384

Stops each shard automatically when its local baseline is complete. Supports resuming
because each shard run has its own checkpoint file (appended with shard index).

You can safely Ctrl+C; child processes will receive termination signal.
"""
from __future__ import annotations
import argparse, subprocess, sys, os, time

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--shards',type=int,default=4)
    ap.add_argument('--embed-batch',type=int,default=128)
    ap.add_argument('--text-max-len',type=int,default=384)
    ap.add_argument('--batch',type=int,default=512)
    ap.add_argument('--grow-batch-after',type=int,default=5)
    ap.add_argument('--grow-factor',type=float,default=2.0)
    ap.add_argument('--max-batch',type=int,default=1536)
    ap.add_argument('--snapshot-every-batches',type=int,default=10)
    ap.add_argument('--stop-percent',type=float,default=100.0)
    ap.add_argument('--force',action='store_true',help='Force re-embed all docs in each shard (not only missing)')
    ap.add_argument('--dry-run',action='store_true')
    ap.add_argument('--no-wait',action='store_true',help='Launch shards then exit without waiting for completion')
    args=ap.parse_args()

    if not (1 <= args.shards <= 16):
        print('Shards must be 1..16'); sys.exit(1)

    procs=[]
    print(f"Launching {args.shards} shard processes...")
    base_cmd=[sys.executable,'backfill_symbol_ref_embeddings.py',
              f"--batch",str(args.batch),
              f"--embed-batch",str(args.embed_batch),
              f"--text-max-len",str(args.text_max_len),
              f"--grow-batch-after",str(args.grow_batch_after),
              f"--grow-factor",str(args.grow_factor),
              f"--max-batch",str(args.max_batch),
              f"--snapshot-every-batches",str(args.snapshot_every_batches),
              f"--stop-percent",str(args.stop_percent),
              '--hex-shards',str(args.shards)]
    if args.force:
        base_cmd.append('--force')

    for i in range(args.shards):
        cmd=base_cmd + ['--hex-shard-index',str(i), '--checkpoint-file', f'.symbol_ref_backfill_checkpoint_{i}.json', '--snapshot-file', f'.symbol_ref_backfill_progress_{i}.json']
        print('CMD', ' '.join(cmd))
        if args.dry_run: continue
        p=subprocess.Popen(cmd)
        procs.append(p)
        time.sleep(0.5)  # small stagger

    if args.dry_run:
        print('Dry run only; no processes started.')
        return

    if args.no_wait:
        print('Launched shard processes (no-wait mode). PIDs: ' + ', '.join(str(p.pid) for p in procs))
        return

    try:
        while procs:
            alive=[]
            for p in procs:
                ret=p.poll()
                if ret is None:
                    alive.append(p)
            procs=alive
            if procs:
                time.sleep(5)
        print('All shard processes completed.')
    except KeyboardInterrupt:
        print('\nTermination requested; sending SIGTERM to children...')
        for p in procs:
            try: p.terminate()
            except Exception: pass
        for p in procs:
            try: p.wait(timeout=5)
            except Exception: pass
        print('Exited.')

if __name__=='__main__':
    main()
