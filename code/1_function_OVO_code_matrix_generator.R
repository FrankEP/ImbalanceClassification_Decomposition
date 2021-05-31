# K <- 5  ### number of classes

OVO_matrix <- function(K){
  class_combinations <- combn(c(1:K), 2)   ### combinations of two of all classes
  
  M <- matrix(0, K, ncol(class_combinations))  ### create matrix whose elements are zero
  
  for (j in 1:ncol(class_combinations)) {
    # cat(1, "  ", j, "\n")
    M[class_combinations[1, j], j] <- 1
  }
  
  for (j in 1:ncol(class_combinations)) {
    # cat(2, "  ", j, "\n")
    M[class_combinations[2, j], j] <- -1
  }
  return(M)
}
