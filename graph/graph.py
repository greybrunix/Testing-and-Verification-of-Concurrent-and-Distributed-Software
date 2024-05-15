import re
import json
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

    pattern_send = r'\"method\": \"send.*?\n\s*],\n\s*\"atomic\".*?\n\s*\"push\".*?\n\s*\"pc\".*?[\s\S]*?\"StoreVar msg\".*?\n.*\n.*\n.*\n\s*\"local\":.*?(\d+).*?(\d+).*?\"payload\".*?\"value\":.*?\"value\":\s\"(\w+)\".*?(\d+).*?'
    pattern_send_bags = r'\"method\": \"send.*?\n\s*],\n\s*\"pop\".*?\n\s*\"push\".*?\n\s*\"pc\".*?\n\s*.*?\n\n\s*.*?{\n\s*\"code\": \"Frame send1.*?[\s\S]*?\"StoreVar msg\",\n\s*\"explain\": \"pop value \({ \\\"dst\\\": (\d+), \\\"id\\\": (\d+), \\\"payload\\\": (\w+), \\\"src\\\": (\d+)'
    pattern_receive = r'\"method\": \"receive.*?\n\s*],\n\s*\"atomic\".*?\n\s*\"push\".*?\n\s*\"pc\".*?[\s\S]*?\"StoreVar msg\".*?\n.*?\n.*?\n.*?\n\s*\"local\":.*?(\d+).*?(\d+).*?\"payload\".*?\"value\":.*?\"value\": \"(\w+)\".*?(\d+).*?'
    pattern_receive_bags = r'\"method\": \"receive.*?\n\s*],\n\s*\"local\".*?\n\s*\"pop\".*?\n\s*\"push\".*?[\s\S]*?\"StoreVar msg\".*?\n.*?\n.*?\n.*?\n\s*\"local\":.*?(\d+).*?(\d+).*?\"payload\".*?\"value\":.*?\"value\": \"(\w+)\".*?(\d+).*?'
    
    pattern = f'{pattern_send}|{pattern_send_bags}|{pattern_receive}|{pattern_receive_bags}'
    matches = re.findall(pattern, data);

    res = {}
    i = 0
    for match in matches:
        print(match)
        res[f'{i}'] = {}
        if match[0] != "":
            res[f'{i}']['type'] = 'send'
            res[f'{i}']['src'] = match[3]
            res[f'{i}']['dst'] = match[0]
            res[f'{i}']['msg'] = match[2]
            res[f'{i}']['id'] = match[1]
        elif match[4] != "":
            res[f'{i}']['type'] = 'send'
            res[f'{i}']['src'] = match[7]
            res[f'{i}']['dst'] = match[4]
            res[f'{i}']['msg'] = match[6]
            res[f'{i}']['id'] = match[5]
        elif match[8] != "":
            res[f'{i}']['type'] = 'receive'
            #res[f'{i}']['src'] = match[11]
            res[f'{i}']['dst'] = match[8]
            #res[f'{i}']['msg'] = match[10]
            res[f'{i}']['id'] = match[9]
        elif match[12] != "":
            res[f'{i}']['type'] = 'receive'
            #res[f'{i}']['src'] = match[11]
            res[f'{i}']['dst'] = match[12]
            #res[f'{i}']['msg'] = match[10]
            res[f'{i}']['id'] = match[13]
        i += 1

    f.close

    print(res)

    with open('data.json', 'w+', encoding='utf-8') as f:
        json.dump(res, f, ensure_ascii=False, indent=4)

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
