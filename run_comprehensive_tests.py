"""Convenience runner for the comprehensive test suite.

Usage:
  python run_comprehensive_tests.py
"""
import unittest, sys

if __name__=='__main__':
    suite=unittest.defaultTestLoader.discover('tests')
    runner=unittest.TextTestRunner(verbosity=2)
    result=runner.run(suite)
    if not result.wasSuccessful():
        sys.exit(1)