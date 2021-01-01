import os
import subprocess
import difflib
from sys import argv

def make_cmd(fn):
    cmd = "cp inputs/{0}.scm .. && cd .. && make {0} && mv {0} compiler_tests/executables && mv {0}.s compiler_tests/bin && rm {0}*".format(fn)
    return cmd

def run_compiler(filename):
    fname = filename[:-4]
    os.system(make_cmd(fname))
    os.system('./executables/{0} > ./outputs/{0}'.format(fname))

def run_chez(filename):
    os.system("chezscheme9.5 --quiet < ./inputs/{} >& ./expected_outputs/{}".format(filename, filename[:-4]))

def compare(filename):
    output_name = filename[:-4]
    with open('./expected_outputs/{}'.format(output_name), 'r') as expected_output:
        with open('./outputs/{}'.format(output_name), 'r') as real_output:
            diff = difflib.unified_diff(
                real_output.readlines(),
                expected_output.readlines(),
                fromfile="real_output",
                tofile='expected_output'
            )
    return diff

def print_test_results(report):
    for item in report.items():
        print(item[0])
        print(item[1].strip() + "\n")

if __name__ == "__main__":
    if (len(argv) > 1):
        input_files = ['{}.scm'.format(argv[1])]
    else:
        input_files = os.listdir('./inputs')
    report = {}
    tests_total = 0
    tests_passed = 0
    for test in input_files:
        tests_total += 1
        run_compiler(test)
        run_chez(test)
        line_diff = []
        for line in compare(test):
            line_diff.append(line)
        if line_diff == []:
            report[test] = "passed"
            tests_passed += 1
        else:
            report[test] = "failed: \n" + " ".join(line_diff)
    if tests_passed != tests_total:
        print("passed {}/{} tests".format(tests_passed,tests_total))
        print_test_results(report)
    else:
        print("passed all tests!")