import os
import difflib

def compare(filename):
    try:
        with open('expected_outputs/{}'.format(filename), 'r') as expected_output:
            with open('outputs/{}'.format(filename), 'r') as real_output:
                diff = difflib.unified_diff(
                    real_output.readlines(),
                    expected_output.readlines(),
                    fromfile="real_output",
                    tofile='expected_output'
                )
    except IOError:
        diff = ["Problem with file: " + filename]
    return diff

tests = os.listdir("outputs")
report = {}
tests_total = 0
tests_passed = 0

def print_test_results(report):
    tests = [test for test in report.keys()]
    tests.sort()

    for test in tests:
        print(test)
        print(report[test].strip() + "\n")

for test in tests:
        tests_total += 1
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