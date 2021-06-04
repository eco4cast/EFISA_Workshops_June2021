#' Dynamic logistic growth model
#'
#' @param p Population at current time step
#' @param r Population growth rate
#' @param k Carrying capacity
#' @return Population at next time step
logistic_growth <- function(p, r, k) {
  (k * p * exp(r)) / (k + p * (exp(r) - 1))
}
