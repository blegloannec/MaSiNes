#!/usr/bin/python
import sys

def printHeader():
    print '<?xml version="1.0"?>'
    print '<signalMachine>'

def genStates():
    style0 = 'stroke:yellow'
    style1 = 'stroke:black'
    scell = ";stroke-width:1.5"
    scom = ';stroke-dasharray:1,1;stroke-width:0.5'
    swall = 'stroke:black;stroke-width:0.5'
    swall2 = swall+';stroke-width:0.5;'
    print '<meta_signal id="s0" speed="0" style="%s%s"></meta_signal>' % (style0,scell)
    print '<meta_signal id="s0l" speed="-1" style="%s%s"></meta_signal>' % (style0,scom)
    print '<meta_signal id="s0r" speed="1" style="%s%s"></meta_signal>' % (style0,scom)
    print '<meta_signal id="s1" speed="0" style="%s%s"></meta_signal>' % (style1,scell)
    print '<meta_signal id="s1l" speed="-1" style="%s%s"></meta_signal>' % (style1,scom)
    print '<meta_signal id="s1r" speed="1" style="%s%s"></meta_signal>' % (style1,scom)
    print '<meta_signal id="wl" speed="-1" style="%s"></meta_signal>' % (swall)
    print '<meta_signal id="wr" speed="1" style="%s"></meta_signal>' % (swall)
    print '<meta_signal id="wL" speed="-2" style="%s"></meta_signal>' % (swall)
    print '<meta_signal id="wR" speed="2" style="%s"></meta_signal>' % (swall)
    print '<meta_signal id="wLL" speed="-2" style="%s"></meta_signal>' % (swall2)
    print '<meta_signal id="wRR" speed="2" style="%s"></meta_signal>' % (swall2)
    print '<meta_signal id="w0" speed="0" style="%s"></meta_signal>' % (swall2)

def swapos(pos):
    return ('l','r')[pos=='l']

def genRules(n):
    for p in ['l','r']:
        print '<rule><in idref="w%c" /><in idref="w%c" /><out idref="s0" /><out idref="w%c" /></rule>' % (p.upper(),p,p)
        print '<rule><in idref="w%s" /><in idref="w%c" /><out idref="w0" /><out idref="w%c" /></rule>' % (2*p.upper(),p,p)
        print '<rule><in idref="w%c" /><in idref="s0" /><out idref="s0" /><out idref="w%c" /><out idref="w%s" /><out idref="s0%c" /></rule>' % (p,p,2*p.upper(),swapos(p))
        print '<rule><in idref="w%c" /><in idref="w0" /><out idref="w%c" /><out idref="w%s" /><out idref="s0%c" /></rule>' % (p,p,p.upper(),swapos(p))
    for i in range(8):
        print '<rule><in idref="s%ir" /><in idref="s%i" /><in idref="s%il" /><out idref="s%il" /><out idref="s%i" /><out idref="s%ir" /></rule>' % ((i>>2)&1,(i>>1)&1,i&1,n&1,n&1,n&1)
        n >>= 1

def genInit(init):
    size = len(init)
    print '<start idref="wl" pos="0" />'
    print '<start idref="wLL" pos="0" />'
    print '<start idref="wl" pos="-1/4" />'
    print '<start idref="wr" pos="%i" />' % (size-1)
    print '<start idref="wRR" pos="%i" />' % (size-1)
    print '<start idref="wr" pos="%i/4" />' % (4*(size-1)+1)
    for i in range(size):
        for p in ['l','','r']:
            if not ((i==0 and p=='l') or (i==size-1 and p=='r')):
                print '<start idref="s%c%s" pos="%i" />' % (init[i],p,i)
    
def printFooter():
    print '</signalMachine>'

## MAIN
def main():
    if len(sys.argv)>2:
        printHeader()
        genStates()
        genRules(int(sys.argv[1]))
        genInit(sys.argv[2])
        printFooter()
    else:
        print 'usage : python eca2xml.py num init > out.xml'

main()
