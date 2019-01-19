cimport cython
cimport numpy as np
import numpy as np


def _squared_distance(np.ndarray[double] x, np.ndarray[double] y):
    cdef double distance = 0.0
    cdef int n = x.shape[0]
    cdef int i = 0
    for i in range(n):
        distance += (x[i] - y[i])**2
    return distance


def create_adjacency_matrix(np.ndarray[double, ndim=2] S):
    n = S.shape[0]
    cdef np.ndarray[double, ndim=2] G = np.zeros((n, n), dtype=np.double)
    cdef int i, j
    for i in range(n):
        for j in range(i, n):
            if i in S[j, :] or j in S[i, :]:
                G[i, j] = 1
                G[j, i] = 1
    return G


def Mnn(np.ndarray[double, ndim=2] X, int M):
    cdef int n = X.shape[0]
    if M >= n-1:
        raise ValueError("M must be at least n-1")
    cdef int d = X.shape[1]
    cdef np.ndarray[double, ndim=2] distances = np.empty((n, n), dtype=np.double)
    cdef np.ndarray[double, ndim=2] S = np.zeros((n, M), dtype=np.double)

    cdef int i, j
    for i in range(n):
        for j in range(n):
            distances[i][j] = _squared_distance(X[i, :], X[j, :])
    
    i = 0
    for i in range(n):
        S[i] = np.argsort(distances[i, ])[1:M+1]

    return S
    
        
        


