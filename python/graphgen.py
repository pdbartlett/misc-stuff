import csv
import os
import sys

nodes = {}
edges = {}

with open(sys.argv[1]) as f:
    for row in csv.reader(f):
        stripped = [entry.strip() for entry in row]
        nodes[stripped[0]] = nodes.get(stripped[0], '') or stripped[1]
        nodes[stripped[2]] = nodes.get(stripped[2], '') or stripped[3]
        edges[stripped[4]] = (stripped[0], stripped[2])

print('graph', os.path.splitext(sys.argv[1])[0], '{')
print('layout=neato; overlap=false; splines=true; ratio=0.707')
print('node [ shape=egg fontname=Noteworthy penwidth=0 ]')
print('edge [ penwidth=2 ]')
c = 0
ids = {}
for n, col in nodes.items():
    id = 'node%d' % (c, )
    ids[n] = id
    c += 1
    bri = sum(list(int(col[i:i+2], 16) for i in (1, 3, 5)))
    fc = 'white' if bri < 420 else 'black'
    print('%s [ label=<<b>%s</b>> style=filled fillcolor="%s" fontcolor=%s fontsize=20 ]' % (id, n, col, fc))
for ch, p in edges.items():
    id = 'node%d' % (c, )
    ids[ch] = id
    c += 1
    print('%s [ label=<<b>%s</b>> shape=none margin=0 width=0 height=0 fontsize=12 ]' % (id, ch))
    print('%s -- %s [ color="%s" ]' % (ids[p[0]], ids[ch], nodes[p[0]]))
    print('%s -- %s [ color="%s" ]' % (ids[p[1]], ids[ch], nodes[p[1]]))
print('}')
