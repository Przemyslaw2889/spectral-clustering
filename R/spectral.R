library(Rcpp)
library(igraph)
sourceCpp(file="spectral_aux.cpp")


Mnn_graph <- function(S){
  "
    Graph G is represented by adjacency matrix.
    In graph G vertices i and j are connected (G[i, j] == 1) if and only if
    i is in M nearest neighbours of j or j is in M nearest neighbours of i
  "
  G <- get_adjacency_matrix(S)
  
  # Connecting a graph
  g_igraph <- graph_from_adjacency_matrix(G)
  components <- clusters(g_igraph)
  for(i in 2:components$no){
    vertex = which.max(components$membership == i)
    G[1, vertex] = 1
    G[vertex, 1] = 1
  }
  return(G)
}


Laplacian_eigen <- function(G, k){
  "Returns eigenenvector matrix of Laplacian of matrix G"
  diagonal <- apply(G, 1, sum)
  D <- diag(diagonal)
  L <- D - G
  n <- nrow(L)
  E <- eigen(L)$vectors[, (n-k):(n-1)]  # eigenvectors corresponding to 2, 3, ... k+1 smallest eigenvalues
  return(E)
}


spectral_clustering <- function(X, k, M){
  "X: matrix of observations we want to cluster
   k: number of clusters
   M: hyperparameter - number of nearest neighbours
  "
  
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacian_eigen(G, k)
  km <- kmeans(E, k)
  return(km$cluster)
}

