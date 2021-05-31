
# Uniform random mutation ----

# gareal_raMutation <- function(object, parent, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_raMutation_Rcpp(object, parent)
#   else
#     gareal_raMutation_R(object, parent)
# }

gareal_raMutation_R <- function(object, parent)   ## randomly select one postion to mutate
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- runif(1, object@lower[j], object@upper[j])
  return(mutate)
}

# Non uniform random mutation ----

# gareal_nraMutation <- function(object, parent, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_nraMutation_Rcpp(object, parent)
#   else
#     gareal_nraMutation_R(object, parent)
# }

gareal_nraMutation_R <- function(object, parent, ...)  ## randomly select one postion to mutate
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  g <- 1 - object@iter/object@maxiter # dempening factor
  sa <- function(x) x*(1-runif(1)^g)
  j <- sample(1:n, 1)
  u <- runif(1)
  if(u < 0.5)
  { mutate[j] <- parent[j] - sa(parent[j] - object@lower[j]) }
  else
  { mutate[j] <- parent[j] + sa(object@upper[j] - parent[j]) }
  return(mutate)
}

# Random mutation around the solution ----

# gareal_rsMutation <- function(object, parent, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_rsMutation_Rcpp(object, parent)
#   else
#     gareal_rsMutation_R(object, parent)
# }

gareal_rsMutation_R <- function(object, parent)
{
  mutate <- parent <- as.vector(object@population[parent,])
  dempeningFactor <- 1 - object@iter/object@maxiter
  direction <- sample(c(-1,1),1)
  value <- (object@upper - object@lower)*0.67
  mutate <- parent + direction*value*dempeningFactor
  outside <- (mutate < object@lower | mutate > object@upper)
  for(j in which(outside))
  { mutate[j] <- runif(1, object@lower[j], object@upper[j]) }
  return(mutate)
}

# Power mutation ----
#
# Deep et al. (2009) suggests to use pow = 10 for real-valued variables, and
# pow = 4 for integer variables.
#
# References
#
# Deep K., Singh K.P., Kansal M.L., Mohan C. (2009) A real coded genetic
#   algorithm for solving integer and mixed integer optimization problems.
#   Applied Mathematics and Computation, 212(2), pp. 505-518.
# Deep K., Thakur M. (2007) A new mutation operator for real coded genetic
#  algorithms, Applied Mathematics and Computation, 193, pp. 211-230.

# gareal_powMutation <- function(object, parent, pow = 10, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_powMutation_Rcpp(object, parent, pow)
#   else
#     gareal_powMutation_R(object, parent, pow)
# }

gareal_powMutation_R <- function(object, parent, pow)
{
  if(missing(pow)) pow <- 10
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  if(length(pow) == 1) pow <- rep(pow, n)
  s <- runif(1)^pow
  t <- (parent - object@lower)/(object@upper - parent)
  r <- runif(n)
  mutate <- parent + ifelse(r < t, 
                            -s*(parent - object@lower),
                            +s*(object@upper - parent))
  return(mutate)
}