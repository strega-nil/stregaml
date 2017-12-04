from subprocess import call
RES = call(["./_build/default/cafe/cafe.exe", "language/scratch.cf"])
if RES != 0:
    print("failure to compile : ", RES)
