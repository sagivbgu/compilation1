#!/bin/bash
python clean_tests.py
python for_gil_compile_all_tests.py $1
python for_gil_run_our_bins.py $1
python for_gil_run_scheme_on_tests.py $1.scm
python for_gil_compare_outputs.py