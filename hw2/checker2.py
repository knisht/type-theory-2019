#!/bin/python
import os
import sys
import signal

class TimeoutException(Exception):
    pass

def timeout_handler(signum, frame):
    raise TimeoutException

signal.signal(signal.SIGALRM, timeout_handler)

for i in range(1000000):
    test = os.popen("./a.out").read().rstrip()
    ftest = "20 1\n" + test
    prefix = "echo \"" + ftest + "\" | "
    # print("On test:\n" + test)
    one = ""
    two = ""
    signal.alarm(5)
    try:
        one = os.popen(prefix + "./parser").read().rstrip()
        two = os.popen(prefix + "./").read().rstrip()
        if one.count('\n') != two.count('\n'):
            print("Error: \n" + test)
            sys.exit()
        else:
            print("Ok:" + str(i))
    except TimeoutException:
        print("___________TIMEOUT___________")
        continue
    else:
        signal.alarm(0)
