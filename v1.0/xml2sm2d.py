#!/usr/bin/python
import sys
from xml.dom.minidom import parse

style = ''

def parseSignals(dom):
    sigs = dom.getElementsByTagName('meta_signal')
    print len(sigs)
    for s in sigs:
        c = s.getElementsByTagName('color')
        if len(c)>0:
            c0 = int(c[0].getAttribute('rgb'))
            b = c0%256
            c0 = c0>>8
            g = c0%256
            r = c0>>8
            print "%s %s %s rgb(%d,%d,%d)%s" %(s.getAttribute('id'), s.getAttribute('speedx'), s.getAttribute('speedy'), r, g, b, style)
        else:
            c = s.getAttribute('style')
            if c == '':
                c = 'black'
            print "%s %s %s %s%s" %(s.getAttribute('id'), s.getAttribute('speedx'), s.getAttribute('speedy'), c, style)

def parseCollisions(dom):
    cols = dom.getElementsByTagName('rule')
    print len(cols)
    for c in cols:
        ins = c.getElementsByTagName('in')
        outs = c.getElementsByTagName('out')
        print len(ins), len(outs),
        for i in ins:
            print i.getAttribute('idref'),
        for o in outs:
            print o.getAttribute('idref'),  
        print
        

def parseInit(dom):
    starts = dom.getElementsByTagName('start')
    print len(starts)
    for s in starts:
        print s.getAttribute('posx'), s.getAttribute('posy'), s.getAttribute('idref')

def main():
    global style
    if len(sys.argv)>1:
        dom = parse(sys.argv[1])
        if len(sys.argv)>2:
            style = sys.argv[2]
        parseSignals(dom)
        parseCollisions(dom)
        parseInit(dom)
    else:
        print 'usage : python xml2sm3d.py in.xml [SVG style] > out.sm3d'

main()
