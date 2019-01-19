library(Rcpp)
library(igraph)
sourceCpp(file="spectral_aux.cpp")


Mnn_graph <- function(S){
  "
    Graph G is represented by adjacency matrix.
    In graph G vertices i and j are connected (G[i, j] == 1) if and only if
    i is in M nearest neighbours of j or j is in M nearest neighbours of i
  "
  n <- nrow(S)
  G <- matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n)
    for(j in i:n)
      if(i %in% S[j, ] || j %in% S[i, ]){
        G[i, j] = 1
        G[j, i] = 1
      }
  
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


# Testing
# G <- rbind(c(0, 1, 1, 0, 0, 0),
#            c(1, 0, 1, 0, 0, 0),
#            c(1, 1, 0, 0, 0, 0),
#            c(0, 0, 0, 0, 1, 0),
#            c(0, 0, 0, 1, 0, 1),
#            c(0, 0, 0, 0, 1, 0))
# g_igraph <- graph_from_adjacency_matrix(G)
# components <- components(g_igraph)
# components
# components$no

X <- rbind(c(1, 2, 3), c(1, 2, 4), c(100, 100, 100), c(1, 2, 5), c(1, 2, 6), c(22, 23, 24))
S = Mnn(X, 2)

Mnn_graph(S)
