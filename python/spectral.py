import numpy as np
import networkx as nx
import random
import warnings
import pyximport
from scipy.linalg import eigh
from sklearn.cluster import KMeans
pyximport.install(setup_args={
                              "include_dirs":np.get_include()},
                  reload_support=True)

from spectral_aux import Mnn, create_adjacency_matrix


def Laplacian_eigen(G, k):
    n = G.shape[0]
    # Finding Laplacian of a graph
    D = np.zeros((n, n), dtype=np.int)
    for i in range(n):
        D[i, i] = np.sum(G[i, :])
    L = D - G
    _, E = eigh(L, eigvals=(2, k))
    return E


def Mnn_graph(S):
    """
    Graph G is represented by adjacency matrix.
    In graph G vertices i and j are connected (G[i, j] == 1) if and only if
    i is in M nearest neighbours of j or j is in M nearest neighbours of i
    """
    G = create_adjacency_matrix(S)

    # Connecting a graph (randomly)
    G_nx = nx.from_numpy_matrix(G)
    components = list(nx.connected_components(G_nx))
    for i in range(len(components) - 1):
        vertex_1 = random.sample(components[i], 1)[0]
        vertex_2 = random.sample(components[i + 1], 1)[0]
        G[vertex_1, vertex_2] = 1
        G[vertex_2, vertex_1] = 1
    
    return G


def spectral_clustering(X, k, M, verbose=False):
    n = X.shape[0]

    if M > n-1:
        print("spectral clustering: M={} was to large and it was changed to {}".format(M, n-1))
        M = n-1

    if verbose:
        print("Mnn function")
    S = Mnn(X, M)
    
    if verbose:
        print("Mnn_graph function")
    G = Mnn_graph(S)

    if verbose:
        print("Laplacian_eigen function")
    E = Laplacian_eigen(G, k)

    if verbose:
        print("Kmeans")
    kmeans = KMeans(n_clusters=k)
    clustering = kmeans.fit_predict(E)
    
    return clustering
