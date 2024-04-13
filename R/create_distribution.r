#' Create Distribution
#'
#' Description
#' Function that creates structure with class "distribution
#
#' Usage
#' create_distribution(x, dims=1)
#
#' Arguments
#' @param x a numberic vector, matrix, or dataframe
#' @param dims integer: Which dimensions are regarded as ‘rows’
#'               or ‘columns’ to sum over. For row*, the sum or
#'             mean is over dimensions dims+1, ...; for col* it
#'           is over dimensions 1:dims.
#'
#' Value
#' @return A "distribution" is a structure with distribution samples
#'         that contains attributes meanval (mean of distribution)
#          and covval(covariance of distribution)
#' @examples
#' Sigma <- matrix(c(10,3,3,2),2,2)
#' vals <- mvrnorm(n = 1000, rep(0, 2), Sigma)
#' distr1 <- create_distribution(vals)



create_distribution <- function(x, dims = 1){
  stopifnot (is.numeric(x) || is(x, "data.frame"))
  stopifnot(length(dim(x))==2)
  stopifnot(nrow(x)>1)
  stopifnot (length(x) > 2)
  stopifnot(dims>0)
  x_mean <- colMeans(x, dims = dims)
  x_cov <- cov(x)
  x <- structure(x, class = 'distribution', meanval=x_mean, covval=x_cov)

}
