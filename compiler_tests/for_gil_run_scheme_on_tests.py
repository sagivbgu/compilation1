import os

inputs = os.listdir("inputs")
for f in inputs:
	os.system("chezscheme9.5 --quiet < inputs/{0} > expected_outputs/{1} 2>&1".format(f, f[:-4]))