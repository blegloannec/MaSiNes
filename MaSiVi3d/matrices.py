#!/usr/bin/env python
# -*- coding: utf-8 -*-

def size(M):
    return len(M),len(M[0])

def const(a,h,w):
    return [[a for j in range(w)] for i in range(h)]

def zero(h,w):
    return const(0,h,w)

def id(n):
    I = zero(n,n)
    for i in range(n):
        I[i][i] = 1
    return I

def copy(M):
    h,w = size(M)
    return [[M[i][j] for j in range(w)] for i in range(h)]

def scal_prod(x,y):
    n = len(x)
    s = 0
    for i in range(n):
        s += x[i]*y[i]
    return s

def mv_prod(M,x):
    n = len(x)
    y = [0 for i in range(n)]
    for i in range(n):
        for j in range(n):
            y[i] += x[j]*M[i][j]
    return y

def mm_prod(A,B):
    N,K = size(A)
    K2,M = size(B)
    if K!=K2:
        return None
    C = [[0 for j in range(M)] for i in range(N)]
    for i in range(N):
        for j in range(M):
            for k in range(K):
                C[i][j] += A[i][k]*B[k][j]
    return C

def sm_prod(a,M):
    h,w = size(M)
    for i in range(h):
        for j in range(w):
            M[i][j] *= a

def c_swap(M,i,j):
    h = len(M)
    for k in range(h):
        M[k][i],M[k][j] = M[k][j],M[k][i]

def l_swap(M,i,j):
    w = len(M[0])
    for k in range(w):
        M[i][k],M[j][k] = M[j][k],M[i][k]

def first_non_zero(M,i):
    w = len(M[0])
    for j in range(w):
        if M[i][j]!=0:
            return j
    return None

def sline_prod(a,M,i):
    w = len(M[0])
    for j in range(w):
        M[i][j] *= a

def line_diff(a,M,i,j):
    # Mj <- Mj - a*Mi
    w = len(M[0])
    for k in range(w):
        M[j][k] -= a*M[i][k]

def inverse(M0):
    M = copy(M0)
    n = len(M)
    I = id(n)
    for i in range(n):
        j0 = first_non_zero(M,i)
        if j0>i:
            c_swap(M,i,j0)
            c_swap(I,i,j0)
        a = 1.0/M[i][i]
        sline_prod(a,M,i)
        sline_prod(a,I,i)
        for j in range(n):
            if j!=i:
                a = M[j][i]
                line_diff(a,M,i,j)
                line_diff(a,I,i,j)
    return I
