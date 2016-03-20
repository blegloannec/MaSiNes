#!/usr/bin/python
import sys
from xml.dom.minidom import parse

style = ''

def parseSignals(dom):
    sigs = dom.getElementsByTagName('meta_signal')
    print len(sigs)
    for s in sigs:
        c = s.getElementsByTagName('color')
        col = ''
        if len(c)>0:
            c0 = int(c[0].getAttribute('rgb'))
            b = c0%256
            c0 = c0>>8
            g = c0%256
            r = c0>>8
            col = 'stroke:rgb(%d,%d,%d);' % (r, g, b)
        sty = s.getAttribute('style')
        if sty=='' and col=='':
            sty = 'stroke:black'
        print "%s %s %s%s%s" %(s.getAttribute('id'), s.getAttribute('speed'), col, sty, style)

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
        print s.getAttribute('pos'), s.getAttribute('idref')

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
        print 'usage : python xml2sm.py in.xml [SVG style] > out.sm'

main()
