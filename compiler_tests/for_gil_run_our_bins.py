import os
from sys import argv

if len(argv) != 1:
	bins = argv[1:]
else:
	bins = os.listdir('executables')
for f in bins:
	os.system("./executables/{0} > outputs/{0}".format(f))