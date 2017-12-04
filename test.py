"""
small testing system for the compiler. It should be noted that this is not the
best testing system, and should probably eventually actually check that these
outputs are correct?
"""

import sys
import os
import re
from subprocess import call


def fail(name, exit_code):
    """
    fail is to be called when a compile that was intended to succeed, didn't
    """
    print("failure to compile '", name, "': ", exit_code, file=sys.stderr)
    sys.exit(1)


def succeed(name):
    """
    succeed is to be called when a compile that was intended to fail, didn't
    """
    print("accidentally compiled '", name, "' successfully", file=sys.stderr)
    sys.exit(1)


if call(["cargo", "build"]) != 0:
    sys.exit(1)

print("\nrunning succeed tests")


def main():
    """
    main function, to get pylint off my back about naming ;)
    """
    tests_succeed = "./language/tests-succeed/"
    tests_fail = './language/tests-fail/'

    for file in os.listdir(path=tests_succeed):
        if re.match(".*\\.cf", file):
            print("compiling ", file)
            res = call(["cargo", "run", "-q", "--", tests_succeed + file])
            if res != 0:
                fail(file, res)
        else:
            print("weird file found in tests-succeed: ", file, file=sys.stderr)

    for file in os.listdir(path=tests_fail):
        if re.match(".*\\.cf", file):
            print("compiling ", file, end="\n    ")
            res = call(["cargo", "run", "-q", "--",
                        "--no-run", tests_fail + file])
            if res == 0:
                succeed(file)
        else:
            print("weird file found in tests-fail: ", file, file=sys.stderr)


main()
