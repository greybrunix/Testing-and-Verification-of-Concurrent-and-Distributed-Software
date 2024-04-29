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

    pattern_send = r'\"method\": \"send\((\d+), (\d+), \\\"(.*?)\\\"\)\",.*?\"id\".*?(\d+).*?\"sp\":\s\d+}'
    pattern_receive = r'\"method\": \"receive\((\d+)\)\",.*?\"id\".*?\"(\d+)\"'
    
    res = [['type', 'src', 'dst', 'msg', 'id']]
    seen = []
    for line in data.split('\n'):
        matchSend = re.match(pattern_send, line)
        matchReceive = re.match(pattern_receive, line) 
        if matchSend and matchSend.group() not in seen:
            tmp = ['send', 
                   matchSend.group(1), 
                   matchSend.group(2),
                   matchSend.group(3),
                   matchSend.group(4)
                  ]
            res.append(tmp)
            seen.append(matchSend.group())
        elif matchReceive and matchReceive.group() not in seen:
            tmp = ['receive',
                   '',                     # src
                   matchReceive.group(1),  # dst
                   '',                     # msg
                   matchReceive.group(2)   # id
                    ]
            res.append(tmp)
            seen.append(matchReceive.group())

    f.close

    with open('data.csv', 'w+') as f:
        csvwriter = csv.writer(f)
        csvwriter.writerows(res)

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
