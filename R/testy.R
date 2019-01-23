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


plot_data <- function(X, Y, title=""){
  plot(X[, 1], X[, 2], col=unlist(Y), pch=20)
  title(title)
}


test_spectral_single <- function(benchmark, dataset, M=20, k=NULL, scale=FALSE){
  data <- read_data(benchmark, dataset)
  X <- data$X
  if(scale){
    X <- scale(X)
  }
  Y <- data$Y
  if(is.null(k)){
    k = length(unique(unlist(Y)))
  }
  set.seed(42)  # because kmeans in spectral clustering randomly initializes centers
  Y_pred <- spectral_clustering(X, k, M)
  plot_data(X, Y_pred, paste(paste(benchmark, dataset, sep="/"), ": spectral ", sep=""))
  print(paste("FM:", FM_index(Y, Y_pred), " AR:", adjustedRandIndex(Y, Y_pred), sep=" "))
  #return(Y_pred)
}


test_hclust <- function(benchmark, dataset, method="complete", k=NULL, scale=FALSE){
  data <- read_data(benchmark, dataset)
  X <- data$X
  if(scale){
    X <- scale(X)
  }
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


test_genie <- function(benchmark, dataset, k=NULL, scale=FALSE){
  data <- read_data(benchmark, dataset)
  X <- data$X
  if(scale){
    X <- scale(X)
  }
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


# Testing on sample datasets
test_spectral_single("graves", "dense", M=20)
test_spectral_single("graves", "dense", M=20, scale=TRUE)
test_spectral_single("sipu", "flame", M=20)
test_spectral_single("sipu", "flame", M=20, scale=TRUE)
test_spectral_single("fcps", "tetra", M=5)
test_spectral_single("fcps", "tetra", M=5, scale=TRUE)

# hclust
test_hclust("graves", "dense")
test_hclust("graves", "dense", scale=TRUE)

# genie
test_genie("graves", "dense")
test_genie("graves", "dense", scale=TRUE)

# Wygląda na to, że skalowanie działa dość różnie dla algorytmu spectral clustering. Czasami nic nie zmienia, czasami poprawia działanie, ale może się zdarzyć że je pogorszy.
# W przeciwieństwie do Pythona w ogóle nie ufam działaniu kmeans w Rze, dawał mi on dość randomowe wyniki.

# My own datasets
random_dataset <- function(mi1, mi2, sig, n){
  x <- rnorm(n, mi1[1], sig)
  y <- rnorm(n, mi1[2], sig)
  X1 <- cbind(x, y)
  Y1 <- rep(1, n)
  
  x <- rnorm(n, mi2[1], sig)
  y <- rnorm(n, mi2[2], sig)
  X2 <- cbind(x, y)
  Y2 <- rep(2, n)
  
  X <- rbind(X1, X2)
  Y <- c(Y1, Y2)
  return(list(X=X, Y=Y))
}

data <- random_dataset(c(0, 0), c(2, 2), 0.5, 50)
X <- data$X
Y <- data$Y
plot_data(X, Y)
Y_pred <- spectral_clustering(X, 2)
plot_data(X, Y_pred)
FM_index(Y, Y_pred)
adjustedRandIndex(Y, Y_pred)

get_heart <- function(n, x_change=0, y_change=0){
  t <- seq(0, 6.29, length.out=n)
  x <- 16 * sin(t)^3
  y <- 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4*t)
  return(cbind(x + x_change, y + y_change))
}

save_hearts <- function(n, n_hearts){
  X <- matrix(, nrow=0, ncol = 2)
  Y <- c()
  
  for(i in 1:n_hearts){
    X1 <- get_heart(n, 20*i, 20*i)
    Y1 <- rep(i, nrow(X1))
    
    X <- rbind(X, X1)
    Y <- c(Y, Y1)
  }
  file_name <- paste("hearts", n_hearts, sep="_")
  write.table(X, file=file.path("datasets", paste(file_name, "data", sep=".")), row.names=FALSE, col.names=FALSE)
  write.table(Y, file=file.path("datasets", paste(file_name, "labels0", sep=".")), row.names=FALSE, col.names=FALSE)
  return(list(X=X, Y=Y))
}

data <- hearts(50, 2)
X <- data$X
Y <- data$Y
plot_data(X, Y)
Y_pred <- spectral_clustering(X, 2)
plot_data(X, Y_pred)  # Calkiem niezle
FM_index(Y, Y_pred)
adjustedRandIndex(Y, Y_pred)

for(i in 2:10){
  save_hearts(40, i)
}
