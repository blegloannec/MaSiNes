#!/usr/bin/python
import sys, random, math

## Parametres
maxSpeed = 3
coeffNbRules = 5
coeffOut = 1.75
coeffInit = 3
##


def genHeader():
    print '<?xml version="1.0"?>'
    print '<signalMachine>'

def genFooter():
    print '</signalMachine>'

def genSignals(n):
    for i in range(n):
        print '<meta_signal id="sig%d" speed="%d"><color rgb="%d" /></meta_signal>' % (i, random.randint(-maxSpeed,maxSpeed), random.randint(0,256*256*256-1))

def genCollisions(n):
    nc = random.randint(int(coeffNbRules*n),int(coeffNbRules*n*n))
    k = int(coeffOut*(math.log(n)+1))
    for i in range(nc):
        print '<rule>'
        nbin = 2 #random.randint(2,k)
        ins = random.sample(range(n),nbin)
        for j in ins:
            print '<in idref="sig%d" />' % j
        nbout = random.randint(0,k)
        outs = random.sample(range(n),nbout)
        for j in outs:
            print '<out idref="sig%d" />' % j
        print '</rule>'

def genInit(n):
    nb = random.randint(int(coeffInit*n/2),int(coeffInit*n))
    for i in range(nb):
        print '<start idref="sig%d" pos="%d" />' % (random.randint(0,n-1), i)


## MAIN
def main():
    random.seed()
    if len(sys.argv)>1:
        n = int(sys.argv[1])
        genHeader()
        genSignals(n)
        genCollisions(n)
        genInit(n)
        genFooter()
    else:
        print 'usage : python alea_xml.py nb_etats > out.xml'

main()
