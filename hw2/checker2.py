#!/bin/python
import os
import sys

for i in range(10000):
    test = os.popen("./a.out").read().rstrip()
    ftest = "100 1\n" + test
    prefix = "echo \"" + ftest + "\" | "
    # print("On test:\n" + test)
    one = os.popen(prefix + "./myparser").read().rstrip()
    two = os.popen(prefix + "./vparser").read().rstrip()
    
    if one.count('\n') != two.count('\n'):
        print("Error \n" + test)
        sys.exit()
    else:
        print("Ok:" + str(i))
