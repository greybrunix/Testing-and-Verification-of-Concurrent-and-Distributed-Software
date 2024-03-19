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
    nodelist = []
    lasts = []
    lasts_cont = []

    for i in range(nt):
        node = f'{i}0'
        G.add_node(node, pos=(i, 0))
        lasts.append(node)
        lasts_cont.append(0)
        plt.text(i-0.12, -0.1, f'Process:{i}', fontsize=12)
    
    # Time in the future will determine the y coordinate of each node
    time = 0
    for ev in events:
        # Time source and dest
        ts = ev[3]
        td = ev[4]
        
        # Update last node in each thread
        lasts_cont[ev[0]] += 1
        lasts_cont[ev[1]] += 1
        
        # Create node label
        src = f'{ev[0]}{lasts_cont[ev[0]]}'
        dst = f'{ev[1]}{lasts_cont[ev[1]]}'
        
        # Add nodes to the list of visible nodes
        nodelist.append(src)
        nodelist.append(dst)

        # Add src and dst nodes to graph
        G.add_node(src, pos=(ev[0], ts))
        G.add_node(dst, pos=(ev[1], td))
        
        # Add edge between src and dst, edge to expand each process
        G.add_edge(lasts[ev[0]], src)
        G.add_edge(lasts[ev[1]], dst)
        G.add_edge(src, dst, label=ev[2])

        # Update global time
        time = td
    
    for i in range(nt):
        node = f'{i}n'
        G.add_node(node, pos=(i, time+0.5))
        G.add_edge(lasts[i], node)        

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
run_graph(3, threads)
