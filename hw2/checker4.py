#!/bin/python

import sys
import os

while True:
    ans = os.popen("./checker3.py").read().rstrip()
    print(ans)

