import re
import csv
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


#name = sys.argv[1] 
#pattern_hny = r'\.hny$'
#pattern_hco = r'\.hco$'
#if re.search(pattern_hny, name):
#    run_harmony(name)
#elif re.search(pattern_hco, name):
#    run_graph(name)

pattern_send = r'\"send\[\[(.*?)\]'
pattern_receive = r'receive\[(\d+)\]'
pattern = f'{pattern_send}|{pattern_receive}'

with open("../graph/fixe_biblioteca.hco", "r") as f:
    data = f.read()

pattern_send = r'"method": "send\((\d+), (\d+), \\"(\w+)\\"\)",.*?"id".*?"(\d+)"'
pattern_receive = r'"method": "receive\((\d+)\)",.*?"id".*?"(\d+)"'
pattern = f'{pattern_send}|{pattern_receive}'

matches = re.findall(pattern, data)

f.close

seen = []
for elem in matches[:]:
    if elem in seen:
        matches.remove(elem)
    else:
        seen.append(elem)


data = [['type', 'src', 'dst', 'msg', 'id']]

for elem in matches:
    tmp = []
    if elem[0] != '':
        tmp.append('send')
        tmp.append(elem[0]) # src
        tmp.append(elem[1]) # dst
        tmp.append(elem[2]) # msg
        tmp.append(elem[3]) # id
    else:
        tmp.append('receive')
        tmp.append('') # src
        tmp.append(elem[4]) # dst
        tmp.append('') # msg
        tmp.append(elem[5]) # id
    data.append(tmp)

with open('data.csv', 'w+') as f:

    csvwriter = csv.writer(f)
    csvwriter.writerows(data)
