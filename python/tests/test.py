import numpy as np
import networkx as nx


v = 7
edges = ((1, 2), (2, 3), (4,5), (5, 6), (4,6))
A = np.zeros((v, v), dtype=np.int)

for i, j in edges:
	A[i, j] = 1
	A[j, i] = 1

print(A)
G = nx.from_numpy_matrix(A)
print(nx.connected_components(G))
