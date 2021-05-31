
# n <- 4
ff2n <- function(n){
  rows <- 2^n
  ncycles <-  rows
  x <- matrix(0, rows, n)
  
  for (k in 1:n) {
    settings <- c(0:1)
    ncycles <- ncycles/2
    nreps <- rows / (2*ncycles)
    # cat(ncycles, " ", nreps, "\n")
    settings <- matrix(rep(settings, nreps), nrow = nreps, byrow = TRUE)
    settings <- c(settings)
    settings <- matrix(rep(settings, ncycles), ncol = ncycles)
    x[, n-k+1] <- c(settings)
  }
  return(x)
}
# ff2n(5)

fullfact <- function(Levels){
  m <- nrow(Levels)
  n <- ncol(Levels)
  
  ssize <- base::prod(c(Levels))
  ncycles <- ssize
  cols <- max(c(m, n))
  design <- matrix(0, nrow = ssize, ncol = cols)
  
  for (i in 1:cols) {
    settings <- c(1:Levels[1, i])
    nreps <- ssize / ncycles
    ncycles <- ncycles / Levels[1, i]
    settings <- matrix(rep(settings, nreps), nrow = nreps, byrow = TRUE)
    settings <- c(settings)
    settings <- matrix(rep(settings, ncycles), ncol = ncycles)
    design[, i] <- c(settings)
  }
  
  return(design)
}
# fullfact(Levels)

designecoc <- function(K, dname){
  
  if(dname=="OVO"){
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
  }
  
  if(dname=="OVA"){
    M <- matrix(-1, nrow = K, ncol = K)
    base::diag(M) <- 1
  }
  
  if(dname=="binary complete"){
    M <- ff2n(K-1)
    M <- M[2:nrow(M), ]
    M <- cbind(0, M)
    M[M==0] <- -1
    M <- -1*M
    M <- t(M)
  }
  
  if(dname=="ternary complete"){
    Levels <- matrix(3, nrow = 1, ncol = K)
    M <- fullfact(Levels = Levels)
    M <- t(M)
    M[M==1] <- -1
    M[M==2] <- 0
    M[M==3] <- 1
    M <- clean(M)
  }
  
  if(dname=="ordinal"){
    M <- matrix(-1, nrow = K, ncol = K-1)
    for (i in 1:(K-1)) {
      M[(i+1):K, i] <- 1
    }
  }
  
  return(M)
}

# designecoc(4, dname = "binary complete")


clean <- function(M){
  L <- ncol(M)
  badcols <- rep(FALSE, L)
  for (i in 1:L) {
    if(!any(M[, i]==1)|!any(M[, i]==-1)){
      badcols[i] <- TRUE
      next
    }
    
    for (j in (i+1):L) {
      if(all(M[, i]==M[, j])|all(M[, i]==-M[, j])){
        badcols[i] <- TRUE
        next
      }
    }
  }
  M <- M[, !badcols]
  M
}

