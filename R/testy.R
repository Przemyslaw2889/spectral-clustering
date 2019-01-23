library(dendextend)
library(mclust)
library(genie)

source("spectral.R")


read_data <- function(benchmark, dataset){
  matrix_file_name <- paste(dataset, ".data.gz", sep="")
  labels_file_name <- paste(dataset, ".labels0.gz", sep="")
  matrix_path <- file.path("..", "benchmarks", benchmark, matrix_file_name)
  labels_path <- file.path("..", "benchmarks", benchmark, labels_file_name)
  X <- as.matrix(read.table(matrix_path))
  Y <- as.matrix(read.table(labels_path))
  return(list(X=X, Y=Y))
}


plot_data <- function(X, Y, title){
  plot(X[, 1], X[, 2], col=unlist(Y), pch=20)
  title(title)
}


test_spectral_single <- function(benchmark, dataset, M=20, k=NULL){
  data <- read_data(benchmark, dataset)
  X <- data$X
  Y <- data$Y
  if(is.null(k)){
    k = length(unique(unlist(Y)))
  }
  Y_pred <- spectral_clustering(X, k, M)
  plot_data(X, Y_pred, paste(paste(benchmark, dataset, sep="/"), ": spectral ", sep=""))
  print(paste("FM:", FM_index(Y, Y_pred), " AR:", adjustedRandIndex(Y, Y_pred), sep=" "))
  #return(Y_pred)
}


test_hclust <- function(benchmark, dataset, method="complete", k=NULL){
  data <- read_data(benchmark, dataset)
  X <- data$X
  Y <- data$Y
  if(is.null(k)){
    k = length(unique(unlist(Y)))
  }
  hc <- hclust(dist(X), method)
  Y_pred <- cutree(hc, k=k)
  plot_data(X, Y_pred, paste(paste(benchmark, dataset, sep="/"), ": hclust ", method, sep=""))
  print(paste("FM:", FM_index(Y, Y_pred), " AR:", adjustedRandIndex(Y, Y_pred), sep=" "))
  #return(Y_pred)
}


test_genie <- function(benchmark, dataset, k=NULL){
  data <- read_data(benchmark, dataset)
  X <- data$X
  Y <- data$Y
  if(is.null(k)){
    k = length(unique(unlist(Y)))
  }
  hc <- hclust2(dist(X))
  Y_pred <- cutree(hc, k=k)
  plot_data(X, Y_pred, paste(paste(benchmark, dataset, sep="/"), ": genie", sep=""))
  print(paste("FM:", FM_index(Y, Y_pred), " AR:", adjustedRandIndex(Y, Y_pred), sep=" "))
  #return(Y_pred)
}



# Przetestujemy spectral clustering na jednym ze zbiorow
test_spectral_single("graves", "dense", M=24)
test_spectral_single("sipu", "flame", M=20)
test_spectral_single("fcps", "tetra", M=5)

# hclust
test_hclust("graves", "dense")

# genie
test_genie("graves", "dense")



