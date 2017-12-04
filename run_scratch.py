from subprocess import call
RES = call(["./_build/default/cafec/main.exe", "language/scratch.cf"])
if RES != 0:
    print("failure to compile : ", RES)
