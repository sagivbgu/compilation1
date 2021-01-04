import os
from sys import argv

if len(argv) != 1:
	bins = argv[1:]
else:
	bins = os.listdir('executables')
for f in bins:
	print("Running {0}".format(f))
	os.system("./executables/{0} > outputs/{0}".format(f))
	print("Finished {0}".format(f))