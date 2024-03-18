import re
import sys
import os
import subprocess
import networkx as nx
import matplotlib.pyplot as plt

def run_harmony(name):
    pid = os.fork()
    if pid ==  0:
        subprocess.run(["harmony", name])
    else:
        os.waitpid(pid, 0)
        name = name[:-3] + "hco"
        run_graph(name)

def run_graph(nt, events):
    # (src, dst, msg, ts, td)

    G = nx.DiGraph()

    lasts = []
    lasts_cont = []
    for i in range(nt):
        node = f'{i}0'
        G.add_node(node, pos=(i, 0))
        lasts.append(node)
        lasts_cont.append(0)

    time = 0
    for ev in events:
        time = ev[3]
        lasts_cont[ev[0]] += 1
        src = f'{ev[0]}{lasts_cont[ev[0]]}'
        G.add_node(src, pos=(ev[0], time))
        G.add_edge(lasts[ev[0]], src)
        time = ev[4]
        lasts_cont[ev[1]] += 1
        dst = f'{ev[1]}{lasts_cont[ev[1]]}'
        G.add_node(dst, pos=(ev[1], time))
        G.add_edge(lasts[ev[1]], dst)
        G.add_edge(src, dst, label=ev[2])
    
    for i in range(nt):
        node = f'{i}n'
        G.add_node(node, pos=(i, time+0.5))
        G.add_edge(lasts[i], node)

    # Draw the graph
    pos = nx.get_node_attributes(G, 'pos')
    nx.draw(G, pos, with_labels=True, arrows=True)
    edge_labels = nx.get_edge_attributes(G, 'label')
    nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)
    plt.savefig("teste.png")
    plt.show()

threads = [(0, 1, "ola", 0.2, 0.5),
           (1, 2, "adeus", 0.8, 1),
           (2, 1, "hola", 1.2, 1.5)
        ]

#name = sys.argv[1] 
#pattern_hny = r'\.hny$'
#pattern_hco = r'\.hco$'
#if re.search(pattern_hny, name):
#    run_harmony(name)
#elif re.search(pattern_hco, name):
#    run_graph(name)
run_graph(3, threads)
