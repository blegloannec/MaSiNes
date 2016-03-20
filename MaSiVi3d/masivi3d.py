#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, pygame, matrices, colors
from xml.dom.minidom import parse
from math import cos,sin

w,h = 0,0
linew = 1
dx,dy = 0,0
screen = None
mx,my = 0,0
l = []

## P la matrice des vecteurs de la base actuelle dans la base canonique
## 3 vecteurs en colonne
## Matrice de passage de Bcan a B (id de B a Bcan)
## Projection sur les 2 premiers vecteurs de P
P = matrices.id(3)


## INIT
def init():
    global screen,w,h,dx,dy,mx,my,clk
    pygame.init()
    # Pour fullscreen :
    #screen = pygame.display.set_mode((0,0),pygame.FULLSCREEN|pygame.HWSURFACE|pygame.DOUBLEBUF)
    #w,h = screen.get_size()
    w,h = (800,800)
    screen = pygame.display.set_mode((w,h),pygame.DOUBLEBUF)
    dx = w/2
    dy = h/2
    screen.fill(colors.white)
    pygame.display.flip()
    clk = pygame.time.Clock()
    mx, my = pygame.mouse.get_pos()
    
def reinit():
    screen.fill(colors.white)
    pygame.display.flip()

def catch_event():
    global mx,my,P
    for event in pygame.event.get():
        if event.type == pygame.QUIT or event.type == pygame.KEYDOWN:
            quit()
        elif event.type == pygame.MOUSEMOTION:
            nmx, nmy = pygame.mouse.get_pos()
            if pygame.mouse.get_pressed()[0]:
                dmy = nmx - mx
                dmx = nmy - my
                if dmx!=0 or dmy!=0:
                    a = 45.0
                    dmx = dmx/a
                    dmy = dmy/a
                    Mx = [[1,0,0],[0,cos(dmx),-sin(dmx)],[0,sin(dmx),cos(dmx)]]
                    My = [[cos(dmy),0,-sin(dmy)],[0,1,0],[sin(dmy),0,cos(dmy)]]
                    # newP = (P My Mx P^-1) P
                    P = matrices.mm_prod(P,matrices.mm_prod(My,Mx))
                    projDraw()
            mx = nmx
            my = nmy

def parseLines(dom):
    sigs = dom.getElementsByTagName('line')
    return [(float(s.getAttribute('x1')), float(s.getAttribute('y1')), float(s.getAttribute('z1')), float(s.getAttribute('x2')), float(s.getAttribute('y2')), float(s.getAttribute('z2')), colors.string2color(s.getAttribute('style'))) for s in sigs]


## DESSIN
def projection((x1,y1,z1,x2,y2,z2,c)):
    return (x1*P[0][0]+y1*P[1][0]+z1*P[2][0], x1*P[0][1]+y1*P[1][1]+z1*P[2][1], x2*P[0][0]+y2*P[1][0]+z2*P[2][0], x2*P[0][1]+y2*P[1][1]+z2*P[2][1],c)

def projectLines(l):
    return map(projection, l)

def drawLine((x1,y1,x2,y2,c)):
    r = pygame.draw.aaline(screen, c, (x1+dx,y1+dy), (x2+dx,y2+dy), linew)
    #pygame.display.update(r)
    
def drawLines(lp):
    for s in lp:
        drawLine(s)
    pygame.display.flip()

def projDraw():
    lp = projectLines(l)
    reinit()
    drawLines(lp)

## MAIN
def main():
    global l
    if len(sys.argv)!=2:
        print >> sys.stderr, 'usage:', sys.argv[0], 'diagram.svg3d'
        sys.exit(1)
    dom = parse(sys.argv[1])
    l = parseLines(dom)
    init()
    projDraw()
    while 1:
        catch_event()
        clk.tick(30)

main()
