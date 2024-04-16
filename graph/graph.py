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
    # (send, src, dst, msg, id)
    # (receive, dst, id)

    G = nx.DiGraph()
    figsize = (20, 20)
    plt.figure(figsize=figsize)
    nodelist = []
    node_no_label = []
    node_no_receive = []
    node_self = []
    lasts = []
    lasts_cont = []

    for i in range(0, nt):
        node = f'{i}0'
        G.add_node(node, pos=(i, 0))
        lasts.append(node)
        lasts_cont.append(0)
        plt.text(i-0.08, -0.35, f'Process:{i}', fontsize=12)
    
    time = 0
    for ev in events[:]:
        # Time source and dest
        ts = time
        td = time + 1
       
        if ev[0] == 'send' and ('receive', ev[2], ev[4]) in events:
            
            receive = ('receive', ev[2], ev[4]) 
            index = events.index(receive)
            events.remove(receive)

            # Update last node in each thread
            lasts_cont[int(ev[1])] += 1
            lasts_cont[int(ev[2])] += 1
        
            # Create node label
            src = f'{ev[1]}{lasts_cont[int(ev[1])]}'
            dst = f'{ev[2]}{lasts_cont[int(ev[2])]}'
        
            # Add nodes to the list of visible nodes
            nodelist.append(src)
            nodelist.append(dst)

            # Add src and dst nodes to graph
            G.add_node(src, pos=(int(ev[1]), ts))
            G.add_node(dst, pos=(int(ev[2]), 1+index))
        
            # Add edge between src and dst, edge to expand each process
            G.add_edge(lasts[int(ev[1])], src)
            G.add_edge(lasts[int(ev[2])], dst)
            G.add_edge(src, dst, label=ev[3])
        elif ev[0] == 'send':
            # Update last node in each thread
            lasts_cont[int(ev[1])] += 1
            lasts_cont[int(ev[2])] += 1

            # Create node label
            src = f'{ev[1]}{lasts_cont[int(ev[1])]}'
            dst = f'{ev[2]}{lasts_cont[int(ev[2])]}'
        
            # Add nodes to the list of visible nodes
            nodelist.append(src)
            node_no_receive.append(src)
            node_no_receive.append(dst)

            if src == dst:
                node_self.append(src)
                # Add src and dst nodes to graph
                G.add_node(src, pos=(int(ev[1]), ts))
                G.add_node(dst, pos=(int(ev[2]), td-(td-ts)/2))
            else:
                # Add src and dst nodes to graph
                G.add_node(src, pos=(int(ev[1]), ts))
                G.add_node(dst, pos=(int(ev[2])-0.5, td-(td-ts)/2))
        
            # Add edge between src and dst, edge to expand each process
            G.add_edge(lasts[int(ev[1])], src)
            G.add_edge(src, dst, label=ev[3]) 

        # Update global time
        time = td
    
    for i in range(0, nt):
        node = f'{i}n'
        G.add_node(node, pos=(i, time+1))
        G.add_edge(lasts[i], node)        

    # Draw the graph
    pos = nx.get_node_attributes(G, 'pos')
    node_attributes = {node: {'visible': False, 'arrow': True} if (node not in nodelist and node in node_no_receive) else {'visible': False, 'arrow': False} if node not in nodelist else {'visible': True, 'arrow': False} if not (node in nodelist and node in node_no_receive) else {'visible': True, 'arrow': True} for node in G.nodes()}
    nx.set_node_attributes(G, node_attributes)

    visible_nodes = [node for node, attrs in G.nodes(data=True) if attrs['visible'] and node not in node_self]
    visible_arrows = [node for node, attrs in G.nodes(data=True) if attrs['arrow'] and node not in node_self]
    red_nodes = [node for node, attrs in G.nodes(data=True) if node in node_self]

    # Draw only the visible nodes
    G_visible = G.subgraph(visible_nodes)

    # Draw only invisible nodes with arrows
    G_arrows = G.subgraph(visible_arrows)

    G_red = G.subgraph(red_nodes)
    
    # Draw all nodes without labels
    nx.draw(G_visible, pos, with_labels=False, arrows=True)

    nx.draw_networkx_nodes(G_red, pos, node_color="#ff0000")

    # Draw labels for visible edges
    edge_labels = nx.get_edge_attributes(G_visible, 'label')
    for edge, label in edge_labels.items():
        nx.draw_networkx_edge_labels(G_visible, pos, edge_labels={edge: label}, label_pos=0.9, verticalalignment='bottom', font_size=10)

    arrow_labels = nx.get_edge_attributes(G_arrows, 'label')
    for edge, label in arrow_labels.items():
        nx.draw_networkx_edge_labels(G_arrows, pos, edge_labels={edge: label}, label_pos=0.8, verticalalignment='bottom', font_size=10)

    red_labels = nx.get_edge_attributes(G_red, 'label')
    for edge, label in red_labels.items():
        nx.draw_networkx_edge_labels(G_red, pos, edge_labels={edge: label}, label_pos=0.8, verticalalignment='top', font_size=10)

    # Draw edges with invisible nodes
    invisible_edges = [(u, v) for u, v in G.edges() if u not in visible_nodes and v not in visible_nodes and u not in red_nodes and v not in red_nodes]
    invisible_edges = set(invisible_edges)
    nx.draw_networkx_edges(G, pos, edgelist=invisible_edges, arrows=False)

    visible_edges = [(u, v) for u, v in G.edges() if u in visible_arrows and v in visible_arrows and u not in red_nodes and v not in red_nodes]
    visible_edges = set(visible_edges)
    nx.draw_networkx_edges(G_arrows, pos, edgelist=visible_edges, arrows=True, node_size=1)

    red_edges = [(u, v) for u, v in G.edges() if u in red_nodes and v in red_nodes]
    nx.draw_networkx_edges(G_red, pos, edgelist=red_edges, arrows=True, connectionstyle='arc3, rad=0.01', width=0)

    plt.savefig("teste.png")
    plt.show()


#name = sys.argv[1] 
#pattern_hny = r'\.hny$'
#pattern_hco = r'\.hco$'
#if re.search(pattern_hny, name):
#    run_harmony(name)
#elif re.search(pattern_hco, name):
#    run_graph(name)

'''
f = open("../lib/algo.hco", "r")
pattern_send = r'\"send\[\[(.*?)\]'
pattern_receive = r'receive\[(\d+)\]'
pattern = f'{pattern_send}|{pattern_receive}'
msgs = re.findall(pattern, f.read())

seen = []
for elem in msgs:
    if elem in seen:
        msgs.remove(elem)
        seen.remove(elem)
    else:
        seen.append(elem)

f.close
'''

# (send, src, dst, msg, id)
# (receive, dst, id)

nt = 2
msgs = [('send', 0, 1, 'False', 0),
        ('receive', 1, 0),
        ('send', 0, 1, 'True', 1),
        ('send', 0, 1, 'False', 2),
        ('receive', 1, 2),
        ('receive', 1, 1), 
        ('send', 0, 1, 'True', 3),
        ('send', 0, 0, 'False', 4)
        ]

run_graph(nt, msgs)
