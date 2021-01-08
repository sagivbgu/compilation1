import os
import subprocess
import difflib
from sys import argv

TEST_PASS = "passed"
TEST_FAIL = "failed"
TEST_SECOND_ORDER_PASS = "2nd-order passed"

def make_cmd(fn):
    cmd = "cp inputs/{0}.scm .. && cd .. && make {0} && mv {0} compiler_tests/executables && mv {0}.s compiler_tests/bin && rm {0}*".format(fn)
    return cmd

def run_compiler(filename):
    fname = filename[:-4]
    os.system(make_cmd(fname))
    os.system('./executables/{0} > ./outputs/{0}'.format(fname))

def run_chez(filename):
    os.system("chezscheme9.5 --quiet < ./inputs/{} >& ./expected_outputs/{}".format(filename, filename[:-4]))

def run_chez_on_compiler_output(filename):
    os.system("chezscheme9.5 --quiet < ./outputs/{0} >& ./outputs/rerun_{0} && mv ./outputs/rerun_{0} ./outputs/{0}".format(filename[:-4]))

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
    tests = [test for test in report.keys()]
    tests.sort()

    for test in tests:
        print(test)
        print(report[test].strip() + "\n")

def run_tests(input_files):
    report = {}
    for test in input_files:
        run_compiler(test)
        run_chez(test)
        line_diff = list(compare(test))
        if line_diff == []:
            report[test] = TEST_PASS
        else:
            report[test] = TEST_FAIL + ": \n" + " ".join(line_diff)
    return report

def rerun_failed_tests(report):
    for (test, description) in report.items():
        if description.startswith(TEST_FAIL):
            run_chez_on_compiler_output(test)
            line_diff = list(compare(test))
            if line_diff == []:
                report[test] = report[test].replace(TEST_FAIL, TEST_SECOND_ORDER_PASS)

if __name__ == "__main__":
    if (len(argv) > 1):
        input_files = ['{}.scm'.format(argv[1])]
    else:
        input_files = os.listdir('./inputs')
    
    
    report = run_tests(input_files)
    rerun_failed_tests(report)
    
    tests_total = len(input_files)
    tests_passed = len([(test, description) for (test, description) in report.items() if description.startswith(TEST_PASS)])
    tests_second_order_passed = len([(test, description) for (test, description) in report.items() if description.startswith(TEST_SECOND_ORDER_PASS)])
    tests_failed = len([(test, description) for (test, description) in report.items() if description.startswith(TEST_FAIL)])

    if tests_passed != tests_total:
        print("passed: {}/{} tests".format(tests_passed,tests_total))
        print("passed (2nd order): {}/{} tests".format(tests_second_order_passed,tests_total))
        print("failed: {}/{} tests\n".format(tests_failed,tests_total))
        print_test_results(report)
    else:
        print("passed all tests!")