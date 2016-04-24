#!/usr/bin/python
import sys
from xml.dom.minidom import parse
from math import sqrt

height = 500
width = 500
e1 = [1,0,0]
e2 = [0,1,0]
factor = 0.8
dx = 50
dy = 150

def fx(x):
    return factor*x+dx
    
def fy(y):
    return height-(factor*y+dy)

def normalize(a):
    n = 0
    for i in range(3):
        n += a[i]*a[i]
    n = sqrt(n)
    for i in range(3):
        a[i] /= n

def scal(a,b):
    res = 0
    for i in range(3):
        res += a[i]*b[i]
    return res

def parseLines(dom):
    sigs = dom.getElementsByTagName('line')
    for s in sigs:
        p0 = [float(s.getAttribute('x1')), float(s.getAttribute('y1')), float(s.getAttribute('z1'))]
        p1 = [float(s.getAttribute('x2')), float(s.getAttribute('y2')), float(s.getAttribute('z2'))]
        print '<line x1="%f" y1="%f" x2="%f" y2="%f" style="%s" />' % (fx(scal(p0,e1)), fy(scal(p0,e2)), fx(scal(p1,e1)), fy(scal(p1,e2)), s.getAttribute('style'))

def readSize(dom):
    global width, height
    root = dom.getElementsByTagName('svg3d')[0]
    width = int(root.getAttribute('width'))
    height = int(root.getAttribute('height'))

def printHeader():
    print '<?xml version=\"1.0\" encoding=\"utf-8\"?>'
    print '<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"%d\" height=\"%d\">' % (width, height)

def printAxes():
    v = max(height, width)
    bx = [v,0,0]
    by = [0,v,0]
    bz = [0,0,v]
    style = 'stroke:black;stroke-dasharray:2,3;stroke-width:0.5;'
    print '<line x1="%f" y1="%f" x2="%f" y2="%f" style="%s" />' % (fx(0), fy(0), fx(scal(bx,e1)), fy(scal(bx,e2)), style)
    print '<line x1="%f" y1="%f" x2="%f" y2="%f" style="%s" />' % (fx(0), fy(0), fx(scal(by,e1)), fy(scal(by,e2)), style)
    print '<line x1="%f" y1="%f" x2="%f" y2="%f" style="%s" />' % (fx(0), fy(0), fx(scal(bz,e1)), fy(scal(bz,e2)), style)
    
def printFooter():
    print '</svg>'

def main():
    global e1, e2
    argc = len(sys.argv)
    if argc==3:
        dom = parse(sys.argv[2])
        readSize(dom)
        printHeader()
        parseLines(dom)
        printFooter()
    elif argc==9:
        for i in range(3):
            e1[i] = float(sys.argv[3+i])
            e2[i] = float(sys.argv[6+i])
        normalize(e1)
        normalize(e2)
        dom = parse(sys.argv[2])
        readSize(dom)
        printHeader()
        printAxes()
        parseLines(dom)
        printFooter()
    else:
        print >> sys.stderr, 'usages:'
        print >> sys.stderr, '  python proj_svg3d.py all in.svg3d [b1x b1y b1z b2x b2y b2z]> out.svg'
        sys.exit(1)

main()
