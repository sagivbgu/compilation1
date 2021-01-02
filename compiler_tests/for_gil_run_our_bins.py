import os

bins = os.listdir('executables')
for f in bins:
	os.system("./executables/{0} > outputs/{0}".format(f))