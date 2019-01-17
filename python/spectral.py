import numpy as np
import pyximport
pyximport.install(setup_args={
                              "include_dirs":np.get_include()},
                  reload_support=True)

from spectral_aux import Mnn


def Laplacian_eigen():
    pass


def Mnn_graph(S):
    pass


def spectral_clustering(X, k, M):
    pass

X = np.array([[1,2,3,4], [66,66,66,66], [1,2,3,4.1], [1,1,1,1]], dtype=np.double)
print(X)
print(Mnn(X, 2))
