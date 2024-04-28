import re
import csv
import sys
import os
import subprocess

def run_harmony(name, web):
    pid = os.fork()
    if pid ==  0:
        if web:
            subprocess.run(["harmony", name])
        else:
            subprocess.run(["harmony", name, "--noweb"])
    else:
        os.waitpid(pid, 0)
        name = name[:-3] + "hco"
        generate_csv(name)


def generate_csv(name):

    with open(name, "r") as f:
        data = f.read()

    pattern_send = r'"method": "send\((\d+), (\d+), \\"(.*?)\\"\)",.*?"id".*?"(\d+)"'
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

name = sys.argv[1] 
web = ''
if (len(sys.argv) > 2):
    web = sys.argv[2]
pattern_hny = r'\.hny$'
pattern_hco = r'\.hco$'
if re.search(pattern_hny, name):
    if web == '--noweb':
        run_harmony(name, 0)
    else:
        run_harmony(name, 1)
elif re.search(pattern_hco, name):
    generate_csv(name)
