source("spectral.R")


read_data <- function(benchmark, dataset){
  matrix_file_name <- paste(dataset, ".data.gz", sep="")
  labels_file_name <- paste(dataset, ".labels0.gz", sep="")
  matrix_path <- file.path("..", "benchmarks", benchmark, matrix_file_name)
  labels_path <- file.path("..", "benchmarks", benchmark, labels_file_name)
  X <- read.table(matrix_path)
  Y <- read.table(labels_path)
  return(list(X=X, Y=Y))
}

benchmark <- "graves"
dataset <- "dense"
read_data(benchmark, dataset)
