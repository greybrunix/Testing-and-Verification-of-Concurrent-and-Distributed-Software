import re
import sys
import os
import subprocess

def run_harmony(name):
    pid = os.fork()
    if pid ==  0:
        subprocess.run(["harmony", name])
    else:
        os.waitpid(pid, 0)
        name = name[:-3] + "hco"
        run_graph(name)

def run_graph(name):
    f = open(name, "r")
#    print(f.read())
    

name = sys.argv[1] 
pattern_hny = r'\.hny$'
pattern_hco = r'\.hco$'
if re.search(pattern_hny, name):
    run_harmony(name)
elif re.search(pattern_hco, name):
    run_graph(name)
