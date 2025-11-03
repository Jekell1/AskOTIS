#!/usr/bin/env python
"""Thin wrapper re-exporting root assert_vector_coverage CI check for convenience under diagnostics/.
"""
from assert_vector_coverage import main as _main

if __name__=='__main__':
    _main()
