#!/bin/python
import os
import sys

onename = "myparser"
twoname = ""

with open("test") as f : contents = f.read().rstrip()

prefix = "cat test | "
one = os.popen(prefix + "./" + onename).read().rstrip()
two = os.popen(prefix + "./" + twoname).read().rstrip()

if (one.count('\n') == two.count('\n')):
    print("eq")
else:
    print("no")
