# Single-point crossover ----

# ga_spCrossover <- function(object, parents, ...)
# {
#   if(gaControl("useRcpp"))
#     ga_spCrossover_Rcpp(object, parents)
#   else
#     ga_spCrossover_R(object, parents)
# }

ga_spCrossover_R <- function(object, parents)
{
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  if(crossOverPoint == 0)
  { children[1:2,] <- parents[2:1,]
  fitnessChildren[1:2] <- fitness[2:1] }
  else if(crossOverPoint == n) 
  { children <- parents
  fitnessChildren <- fitness }
  else 
  { children[1,] <- c(parents[1,1:crossOverPoint],
                      parents[2,(crossOverPoint+1):n])
  children[2,] <- c(parents[2,1:crossOverPoint],
                    parents[1,(crossOverPoint+1):n])
  }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}





# Whole arithmetic crossover ----

# gareal_waCrossover <- function(object, parents, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_waCrossover_Rcpp(object, parents)
#   else
#     gareal_waCrossover_R(object, parents)
# }

gareal_waCrossover_R <- function(object, parents)
{
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  a <- runif(1)
  children[1,] <- a*parents[1,] + (1-a)*parents[2,]
  children[2,] <- a*parents[2,] + (1-a)*parents[1,]
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

# Local arithmetic crossover

# gareal_laCrossover <- function(object, parents, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_laCrossover_Rcpp(object, parents)
#   else
#     gareal_laCrossover_R(object, parents)
# }

gareal_laCrossover_R <- function(object, parents)
{
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  a <- runif(n)
  children[1,] <- a*parents[1,] + (1-a)*parents[2,]
  children[2,] <- a*parents[2,] + (1-a)*parents[1,]
  out <- list(children = children, fitness = rep(as.double(NA),2))
  return(out)
}

# Blend crossover ----

# gareal_blxCrossover <- function(object, parents, a = 0.5, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_blxCrossover_Rcpp(object, parents, a)
#   else
#     gareal_blxCrossover_R(object, parents, a)
# }

gareal_blxCrossover_R <- function(object, parents, a)
{
  if(missing(a)) a <- 0.5
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  x <- apply(parents, 2, range)
  xl <- pmax(x[1,] - a*(x[2,]-x[1,]), object@lower)
  xu <- pmin(x[2,] + a*(x[2,]-x[1,]), object@upper)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  for(i in 1:n)
    children[,i] <- runif(2, xl[i], xu[i]) 
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

# Laplace crossover ----
#
# Laplace crossover(a, b), where a is the location parameter and b > 0 is 
# the scaling parameter of a Laplace distribution, which is generated as 
# described in 
# Krishnamoorthy K. (2006) Handbook of Statistical Distributions with 
#   Applications, Chapman & Hall/CRC.
#
# For smaller values of b offsprings are likely to be produced nearer to 
# parents, and for larger values of b offsprings are expected to be produced
# far from parents.
# Deep et al. (2009) suggests to use a = 0, b = 0.15 for real-valued 
# variables, and b = 0.35 for integer variables.
#
# References
#
# Deep K., Thakur M. (2007) A new crossover operator for real coded genetic
#   algorithms, Applied Mathematics and Computation, 188, 895-912.
# Deep K., Singh K.P., Kansal M.L., Mohan C. (2009) A real coded genetic
#   algorithm for solving integer and mixed integer optimization problems.
#   Applied Mathematics and Computation, 212(2), pp. 505-518.

# gareal_laplaceCrossover <- function(object, parents, a = 0, b = 0.15, ...) 
# {
#   if(gaControl("useRcpp"))
#     gareal_laplaceCrossover_Rcpp(object, parents, a, b)
#   else
#     gareal_laplaceCrossover_R(object, parents, a, b)
# }

gareal_laplaceCrossover_R <- function(object, parents, a, b) 
{
  if(missing(a)) a <- 0.00
  if(missing(b)) b <- 0.15
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  if(length(a) == 1) a <- rep(a, n)
  if(length(b) == 1) b <- rep(b, n)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  r <- runif(n)
  u <- runif(n)
  beta <- a + ifelse(r > 0.5, b*log(u), -b*log(u))
  bpar <- beta*abs(parents[1,] - parents[2,])
  children[1,] <- pmin(pmax(parents[1,] + bpar, object@lower), object@upper)
  children[2,] <- pmin(pmax(parents[2,] + bpar, object@lower), object@upper)
  out <- list(children = children, fitness = rep(NA, 2))
  return(out)
}
