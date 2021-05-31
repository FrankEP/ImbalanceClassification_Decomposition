## 
## Real-value GA operators ----
##

# Generate a random population ----

# gareal_Population <- function(object, ...)
# {
#   if(gaControl("useRcpp"))
#     gareal_Population_Rcpp(object)
#   else
#     gareal_Population_R(object)
# }

gareal_Population_R <- function(object)
{
  lower <- object@lower
  upper <- object@upper
  nvars <- length(lower)
  population <- matrix(as.double(NA), nrow = object@popSize, ncol = nvars)
  for(j in 1:nvars) 
  { population[,j] <- runif(object@popSize, lower[j], upper[j]) }
  return(population)
}