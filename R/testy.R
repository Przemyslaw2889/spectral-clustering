library(dendextend)

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
  plot_data(X, Y_pred, paste(benchmark, dataset, sep="/"))
}


# Przetestujemy spectral clustering na jednym ze zbiorow
test_spectral_single("graves", "dense", M=24)
test_spectral_single("sipu", "flame", M=20)
test_spectral_single("fcps", "tetra", M=5)
