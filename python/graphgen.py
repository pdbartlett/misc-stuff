import csv
import itertools
import os
import sys

def main(args):
    ids = {}
    colours = {}
    parents = {}
    c = itertools.count()
    for fpath in args:
        with open(sys.argv[1]) as f:
            for row in csv.reader(f):
                stripped = [entry.strip() for entry in row]
                ids[stripped[0]] = ids.get(stripped[0], '') or 'node' + str(next(c))
                colours[stripped[0]] = colours.get(stripped[0], '') or stripped[1]
                ids[stripped[2]] = ids.get(stripped[2], '') or 'node' + str(next(c))
                colours[stripped[2]] = colours.get(stripped[2], '') or stripped[3]
                ids[stripped[4]] = 'node' + str(next(c))
                parents[stripped[4]] = (stripped[0], stripped[2])

    print('graph', os.path.splitext(os.path.basename(sys.argv[1]))[0], '{')
    print('layout=sfdp; overlap=false; splines=true; ratio=0.707')
    print('node [ shape=egg fontname=Noteworthy penwidth=0 ]')
    print('edge [ penwidth=2 ]')
    for n, col in colours.items():
        bri = sum(list(int(col[i:i+2], 16) for i in (1, 3, 5)))
        fc = 'white' if bri < 420 else 'black'
        print('%s [ label=<<b>%s</b>> style=filled fillcolor="%s" fontcolor=%s fontsize=20 ]' % (ids[n], n, col, fc))
    for ch, p in parents.items():
        print('%s [ label=<<b>%s</b>> shape=none margin=0 width=0 height=0 fontsize=12 ]' % (ids[ch], ch))
        print('%s -- %s [ color="%s" ]' % (ids[p[0]], ids[ch], colours[p[0]]))
        print('%s -- %s [ color="%s" ]' % (ids[p[1]], ids[ch], colours[p[1]]))
    print('}')

if __name__ == "__main__":
    main(sys.argv[1:])
