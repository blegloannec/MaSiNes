#!/usr/bin/python
import sys

def printHeader():
    print '<?xml version="1.0"?>'
    print '<signalMachine>'
    print '<simulation iterations="25" time_max="0" />'
    print '<output width="500" height="400" zoom_h="25." zoom_v="25." trans_h="4." />'

def genStates():
    stylea = ['stroke:cyan','stroke:blue']
    styleb = ['stroke:orange','stroke:red']
    styles = ['stroke:grey','stroke:black']
    stylec = ['',';stroke-width:1.5']
    #scom = ';stroke-dasharray:1,1;stroke-width:0.5'
    #swall = 'stroke:black;stroke-width:0.5'
    #swall2 = swall+';stroke-width:0.5;'
    for i in range(2):
        print '<meta_signal id="b%d" speed="0" style="%s"></meta_signal>' % (i,styleb[i])
        print '<meta_signal id="s%d" speed="-1" style="%s"></meta_signal>' % (i,styles[i])
        for j in range(2):
            print '<meta_signal id="a%dc%d" speed="1" style="%s%s"></meta_signal>' % (i,j,stylea[i],stylec[j])


def genRules():
    for a in range(2):
        for b in range(2):
            for c in range(2):
                for s in range(2):
                    print '<rule><in idref="a%dc%d" /><in idref="b%d" /><in idref="s%d" /><out idref="a%dc%d" /><out idref="b%d" /><out idref="s%d" /></rule>' % (a,c,b,s,a,(a*b*c) or (a*b*s) or (c*s),b,(a*b)^(c^s))

def int2bin(n):
    b = []
    while n>0:
        b.append(n%2)
        n /= 2
    return b
                    
def genInit(a,b):
    ba = int2bin(a)
    for p in range(len(ba)):
        print '<start idref="a%dc0" pos="%d" />' % (ba[p],p)
    bb = int2bin(b)
    for p in range(len(bb)):
        print '<start idref="b%d" pos="%d" />' % (bb[p],p+len(ba))
        print '<start idref="s0" pos="%d" />' % (2*p+1+len(ba))
    for p in range(len(ba)):
        print '<start idref="b0" pos="%d" />' % (p+len(ba)+len(bb))
        print '<start idref="s0" pos="%d" />' % ((p+1+len(ba))+len(bb))
    
def printFooter():
    print '</signalMachine>'


## MAIN
def main():
    if len(sys.argv)!=3:
        print >> sys.stderr, 'usage:', sys.argv[0], 'a b > out.xml'
        sys.exit(1)
    printHeader()
    genStates()
    genRules()
    genInit(int(sys.argv[1]),int(sys.argv[2]))
    printFooter()


main()
