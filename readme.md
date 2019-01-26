# Spectral clustering

## Algorithm

1. `Mnn(X, M)` Given matrix of observations in rows Mnn function computes matrix of m nearest neighbours S (n \times M) which has indexes of nearest neighbours in it's rows.

2. `Mnn_graph(S)` Computes an adjecency matrix of Graph G in which indexes i and j are connected if and only if i is M nearest neighbours of j or j is in M nearest neighbours of i. If this graph is not connected it is connected randomly.

3. `Laplacian_eigen(G, k)` First we compute Laplacian of matrix G, that is a matrix L = D - G where D is diagonal with degrees of matrix G. Then we compute matrix E (n \times k) which columns consist of eigenvectors corresponding to 2, 3, ... k+1 smallest eigenvalues. We return matrix E.

4. `spectral_clustering(X, k, M)` Given matrix X it calls functions in 1, 2, 3 and does kmeans algorithm on matrix E. It returns cluster labels for each row in X.

`Mnn function` as well as computing adjacency matrix are implemented in Rcpp and Cython.

## Results
We compare results on benchmark datasets contained in `benchmarks` folder. We visualize results and compute Fowlkes-Mallows, adjusted mutual information and adjusted rand scores. Code and results are shown in raport.\* files and results are saved in `results.csv` files. Simple test of algorithm with visualization are done in testy.\* files. Additionaly, we create sample datasets and test algorithm on them. 



