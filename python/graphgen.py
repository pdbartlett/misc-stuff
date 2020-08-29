import csv
import itertools
import os
import sys

colours = {}
parents = {}

with open(sys.argv[1]) as f:
    for row in csv.reader(f):
        stripped = [entry.strip() for entry in row]
        colours[stripped[0]] = colours.get(stripped[0], '') or stripped[1]
        colours[stripped[2]] = colours.get(stripped[2], '') or stripped[3]
        parents[stripped[4]] = (stripped[0], stripped[2])

print('graph', os.path.splitext(os.path.basename(sys.argv[1]))[0], '{')
print('layout=sfdp; overlap=false; splines=true; ratio=0.707')
print('node [ shape=egg fontname=Noteworthy penwidth=0 ]')
print('edge [ penwidth=2 ]')
ids = {}
c = itertools.count()
for n, col in colours.items():
    id = 'node' + str(next(c))
    ids[n] = id
    bri = sum(list(int(col[i:i+2], 16) for i in (1, 3, 5)))
    fc = 'white' if bri < 420 else 'black'
    print('%s [ label=<<b>%s</b>> style=filled fillcolor="%s" fontcolor=%s fontsize=20 ]' % (id, n, col, fc))
for ch, p in parents.items():
    id = 'node' + str(next(c))
    ids[ch] = id
    print('%s [ label=<<b>%s</b>> shape=none margin=0 width=0 height=0 fontsize=12 ]' % (id, ch))
    print('%s -- %s [ color="%s" ]' % (ids[p[0]], ids[ch], colours[p[0]]))
    print('%s -- %s [ color="%s" ]' % (ids[p[1]], ids[ch], colours[p[1]]))
print('}')
