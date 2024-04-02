import re
import json
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


def dfs(edges, node, trace):
    while True:
        if not edges[node]:
            if not trace:
                return []
            else:
                prev_node, sym = trace.pop()
                #edges[prev_node].append((node, sym))
                node = prev_node
        else:
            neighbour, sym = edges[node].pop()
            trace.append((node, sym))
            if neighbour in final:
                trace.append((neighbour, -1))
                return trace
            else:
                node = neighbour
    return trace


def run_graph(nt, events):
    # (send, src, dst, msg, ts, td)
    # (receive, dst, ts, td)

    G = nx.DiGraph()
    nodelist = []
    lasts = []
    lasts_cont = []

    for i in range(1, nt+1):
        node = f'{i}0'
        G.add_node(node, pos=(i, 0))
        lasts.append(node)
        lasts_cont.append(0)
        plt.text(i-0.12, -0.1, f'Process:{i}', fontsize=12)
    
    # Time in the future will determine the y coordinate of each node
    time = 0
    for ev in events[:]:
        # Time source and dest
        #ts = ev[3]
        #td = ev[4]
        ts = time
        td = time + 1
       
        if ev[0] == 'send' and ('receive', ev[1]) in events:
            events.remove(('receive', ev[1]))
            # Update last node in each thread
            lasts_cont[int(ev[1])-1] += 1
            lasts_cont[int(ev[2])-1] += 1
        
            # Create node label
            src = f'{ev[1]}{lasts_cont[int(ev[1])-1]}'
            dst = f'{ev[2]}{lasts_cont[int(ev[2])-1]}'
        
            # Add nodes to the list of visible nodes
            nodelist.append(src)
            nodelist.append(dst)

            # Add src and dst nodes to graph
            G.add_node(src, pos=(int(ev[1]), ts))
            G.add_node(dst, pos=(int(ev[2]), td))
        
            # Add edge between src and dst, edge to expand each process
            G.add_edge(lasts[int(ev[1])-1], src)
            G.add_edge(lasts[int(ev[2])-1], dst)
            G.add_edge(src, dst, label=ev[3])

        # Update global time
        time = td
    
    for i in range(1, nt+1):
        node = f'{i}n'
        G.add_node(node, pos=(i, time+1))
        G.add_edge(lasts[i-1], node)        

    # Draw the graph
    pos = nx.get_node_attributes(G, 'pos')
    node_attributes = {node: {'visible': False} if node not in nodelist else {'visible': True} for node in G.nodes()}
    nx.set_node_attributes(G, node_attributes)

    visible_nodes = [node for node, attrs in G.nodes(data=True) if attrs['visible']]
    
    # Draw only the visible nodes
    G_visible = G.subgraph(visible_nodes)

    # Draw all nodes without labels
    nx.draw(G_visible, pos, with_labels=False, arrows=True)

    # Draw labels for visible edges
    edge_labels = nx.get_edge_attributes(G_visible, 'label')
    nx.draw_networkx_edge_labels(G_visible, pos, edge_labels=edge_labels)

    # Draw edges with invisible nodes
    invisible_edges = [(u, v) for u, v in G.edges() if u not in visible_nodes or v not in visible_nodes]
    nx.draw_networkx_edges(G, pos, edgelist=invisible_edges, arrows=False)

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

with open("../lib/algo.hfa", "r") as f:
    data = json.load(f)

symbols = data.get("symbols")
symbols_values = [
    tuple(item['value'][i]['value'] for i in range(len(item['value'])))
    for item in data['symbols']
]

nt = max([int(value) for tup in symbols_values 
          for value in tup[1:] 
          if value.isdigit()]
         )

initial = data.get("initial")
final = [item["idx"] for item in data['nodes'] if item['type'] == 'final']
edges = {item['src']: [] for item in data['edges']}

for item in data['edges']:
    src = item['src']
    dst = item['dst']
    sym = int(item['sym'])
    edges[src].append((dst, sym))

node = initial
visited = set()
trace = dfs(edges, node, trace)
events = []

for (n, t) in trace[:-1]:
    events.append(symbols_values[t])

run_graph(nt, events)
