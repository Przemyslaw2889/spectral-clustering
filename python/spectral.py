import numpy as np
import networkx as nx
import random
import pyximport
from scipy.linalg import eigh
from sklearn.cluster import KMeans
pyximport.install(setup_args={
                              "include_dirs":np.get_include()},
                  reload_support=True)

from spectral_aux import Mnn


def Laplacian_eigen(G, k):
    n = G.shape[0]
    # Finding Laplacian of a graph
    D = np.zeros((n, n), dtype=np.int)
    for i in range(n):
        D[i, i] = np.sum(G[i, :])
    L = D - G
    _, E = eigh(L, eigvals=(1, k+1))
    return E


def Mnn_graph(S):
    """
    Graph G is represented by adjacency matrix.
    In graph G vertices i and j are connected (G[i, j] == 1) if and only if
    i is in M nearest neighbours of j or j is in M nearest neighbours of i
    """
    n = S.shape[0]
    G = np.zeros((n, n), dtype=np.int)
    for i in range(n):
        for j in range(n):
            if i in S[j, :] or j in S[i, :]:
                G[i, j] = 1

    # Connecting a graph (randomly)
    G_nx = nx.from_numpy_matrix(G)
    components = list(nx.connected_components(G_nx))
    for i in range(len(components) - 1):
        vertex_1 = random.sample(components[i], 1)[0]
        vertex_2 = random.sample(components[i + 1], 1)[0]
        G[vertex_1, vertex_2] = 1
        G[vertex_2, vertex_1] = 1
    
    return G


def spectral_clustering(X, k, M):
    S = Mnn(X, M)
    G = Mnn_graph(S)
    E = Laplacian_eigen(G, k)
    kmeans = KMeans(n_clusters=k)
    clustering = kmeans.fit_predict(X)
    return clustering
