##
## Generic GA selection ----
##

# Linear-rank selection ----
# Michalewicz (1996) Genetic Algorithms + Data Structures = Evolution Programs. p. 60

# ga_lrSelection <- function(object, 
#                            r = 2/(object@popSize*(object@popSize-1)), 
#                            q = 2/object@popSize, ...)
# {
#   if(gaControl("useRcpp"))
#     ga_lrSelection_Rcpp(object, r, q)
#   else
#     ga_lrSelection_R(object, r, q)
# }

ga_lrSelection_R <- function(object, r, q)
{
  if(missing(r)) r <- 2/(object@popSize * (object@popSize - 1))
  if(missing(q)) q <- 2/object@popSize
  rank <- (object@popSize+1) - rank(object@fitness, ties.method = "min")
  prob <- 1 + q - (rank-1)*r
  prob <- pmin(pmax(0, prob/sum(prob)), 1, na.rm = TRUE)
  sel <- sample(1:object@popSize, size = object@popSize, 
                prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

# Nonlinear-rank selection ----
# Michalewicz (1996) Genetic Algorithms + Data Structures = Evolution Programs. p. 60

# ga_nlrSelection <- function(object, q = 0.25, ...)
# {
#   if(gaControl("useRcpp"))
#     ga_nlrSelection_Rcpp(object, q)
#   else
#     ga_nlrSelection_R(object, q)
# }

ga_nlrSelection_R <- function(object, q)
{
  if(missing(q)) q <- 0.25
  rank <- (object@popSize + 1) - rank(object@fitness, ties.method = "min")
  prob <- q*(1-q)^(rank-1)
  prob <- pmin(pmax(0, prob/sum(prob)), 1, na.rm = TRUE)
  sel <- sample(1:object@popSize, size = object@popSize, 
                prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

# Proportional (roulette wheel) selection ----

# ga_rwSelection <- function(object, ...)
# {
#   if(gaControl("useRcpp"))
#     ga_rwSelection_Rcpp(object)
#   else
#     ga_rwSelection_R(object)
# }

ga_rwSelection_R <- function(object, ...)
{
  prob <- abs(object@fitness)/sum(abs(object@fitness))
  prob <- pmin(pmax(0, prob/sum(prob)), 1, na.rm = TRUE)
  sel <- sample(1:object@popSize, size = object@popSize, 
                prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

# (unbiased) Tournament selection ----

# ga_tourSelection <- function(object, k = 3, ...)
# {
#   if(gaControl("useRcpp"))
#     ga_tourSelection_Rcpp(object, k)
#   else
#     ga_tourSelection_R(object, k)
# }

ga_tourSelection_R <- function(object, k = 3, ...)
{
  sel <- rep(NA, object@popSize)
  for(i in 1:object@popSize)
  { s <- sample(1:object@popSize, size = k)
  sel[i] <- s[which.max(object@fitness[s])]
  }
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}



##
## Generic GA real-value selection ----
##


# Fitness proportional selection with fitness linear scaling ----

# gareal_lsSelection <- function(object, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_lsSelection_Rcpp(object)
#   else
#     gareal_lsSelection_R(object)
# }

gareal_lsSelection_R <- function(object)
{
  f <- object@fitness
  fmin <- min(f, na.rm = TRUE)
  if(fmin < 0) 
  { f <- f - fmin
  fmin <- min(f, na.rm = TRUE) }
  fave <- mean(f, na.rm = TRUE)
  fmax <- max(f, na.rm = TRUE)
  sfactor <- 2 # scaling factor
  eps <- sqrt(.Machine$double.eps)
  # transform f -> f' = a*f + b such that
  if(fmin > (sfactor*fave - fmax)/(sfactor-1))
  { # ave(f) = ave(f')
    # 2*ave(f') = max(f')
    delta <- fmax - fave
    a <- (sfactor - 1.0)*fave/delta
    b <- fave * (fmax - sfactor*fave)/delta 
  }
  else
  { # ave(f) = ave(f')
    # min(f') = 0
    delta <- fave - fmin
    a <- fave/delta
    b <- -1*fmin*fave/delta 
  }
  fscaled <- a*f + b
  prob <- abs(fscaled)/sum(abs(fscaled), na.rm = TRUE)
  prob[is.na(prob)] <- eps
  prob <- pmin(pmax(0.0, prob/sum(prob)), 1.0)
  sel <- sample(1:object@popSize, size = object@popSize, 
                prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}


# Fitness proportional selection with Goldberg's Sigma Truncation Scaling ----

# gareal_sigmaSelection <- function(object, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_sigmaSelection_Rcpp(object)
#   else
#     gareal_sigmaSelection_R(object)
# }

gareal_sigmaSelection_R <- function(object)
{
  popSize <- object@popSize
  mf <- mean(object@fitness, na.rm = TRUE)
  sf <- sd(object@fitness, na.rm = TRUE)
  fscaled <- pmax(object@fitness - (mf - 2*sf), 0, na.rm = TRUE)
  prob <- abs(fscaled)/sum(abs(fscaled))
  prob <- pmin(pmax(0, prob, na.rm = TRUE), 1, na.rm = TRUE)
  sel <- sample(1:popSize, size = popSize, prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}