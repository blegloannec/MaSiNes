#!/usr/bin/env python
# -*- coding: utf-8 -*-

from string import rfind

black = (0,0,0)
white = (255,255,255)
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)
gray = (127,127,127)

s2c = dict(black=black, white=white, red=red, blue=blue, green=green, gray=gray, grey=gray)

def string2color(s):
    i = rfind(s,'stroke:')
    if i>=0:
        s = s[i+7:]
        j = rfind(s,';')
        if j>=0:
            s = s[:j]
    return s2c.get(s, black)
