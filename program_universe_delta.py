"""Compute deltas across program id universes.
Inputs (generated previously):
  - meta_program_ids.json
  - paragraph_program_ids.json
  - filesystem_program_ids.json
Outputs summary + delta JSON file.
"""
import json, argparse, sys


def load(name,path):
    try:
        return set(json.load(open(path,'r',encoding='utf-8')))
    except FileNotFoundError:
        print(f"Missing {path}", file=sys.stderr); return set()


def main():
    ap=argparse.ArgumentParser(description='Program universe delta report')
    ap.add_argument('--out-json',default='program_universe_delta.json')
    ap.add_argument('--meta-file',default='meta_program_ids.json')
    ap.add_argument('--paragraph-file',default='paragraph_program_ids.json')
    ap.add_argument('--filesystem-file',default='filesystem_program_ids.json')
    args=ap.parse_args()
    meta=load('meta',args.meta_file)
    paras=load('paragraphs',args.paragraph_file)
    fs=load('filesystem',args.filesystem_file)

    in_fs_only=sorted(fs - meta - paras)
    in_paras_only=sorted(paras - meta)
    in_meta_only=sorted(meta - paras - fs)
    common_all=sorted(meta & paras & fs)

    summary={
        'counts':{
            'meta': len(meta),
            'paragraphs': len(paras),
            'filesystem': len(fs),
            'common_all': len(common_all),
            'in_fs_only': len(in_fs_only),
            'in_paragraphs_only': len(in_paras_only),
            'in_meta_only': len(in_meta_only)
        },
        'samples':{
            'fs_only': in_fs_only[:20],
            'paras_only': in_paras_only[:20],
            'meta_only': in_meta_only[:20]
        }
    }
    print('Summary:')
    for k,v in summary['counts'].items():
        print(f"  {k}: {v}")
    json.dump({'summary':summary,'fs_only':in_fs_only,'paras_only':in_paras_only,'meta_only':in_meta_only,'common_all':common_all}, open(args.out_json,'w'), indent=2)
    print('Wrote', args.out_json)

if __name__=='__main__':
    main()
