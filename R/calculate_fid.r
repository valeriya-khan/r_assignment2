#' Calculate FID
#'
#' Frechet Inception distance
#'
#' Arguments
#' @param x object of "distribution" structure with
#'          attributes "meanval" and "covval"
#' @param y object of "distribution" structure with
#'          attributes "meanval" and "covval"
#' Details
#' The Frechet Inception Distance, or FID for short,
#' is a metric for evaluating the quality of generated
#' images and specifically developed to evaluate the
#' performance of generative adversarial networks.
#' The inputs are usually calculated from feature vectors
#' obtained by passing the images through the machine learning
#' model
#
#' Value
#' @return Return FID number which represents estimate how close are
#'  given distributions. The smaller number indicates closer
#'  distributions.
#'
#' @examples
#' wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
#'                   sep=",")
#' distr1 <- create_distribution(wine[1:6])
#' distr2 <- create_distribution(wine[7:12])
#' calculate_fid(distr1, distr2)

calculate_fid <- function(x,y){
  stopifnot(is(x, "distribution"))
  stopifnot(is(y, "distribution"))
  stopifnot("covval" %in% names(attributes(y)))
  stopifnot("meanval" %in% names(attributes(x)))
  stopifnot("meanval" %in% names(attributes(y)))
  stopifnot("covval" %in% names(attributes(x)))
  stopifnot("covval" %in% names(attributes(y)))
  stopifnot(setequal(dim(attr(x,"meanval")),dim(attr(y,"meanval"))))
  stopifnot(setequal(dim(attr(x,"covval")),dim(attr(y,"covval"))))
  ssdiff <- sum((attr(x,"meanval")-attr(y,"meanval"))^2)
  val <- t(attr(x,"covval"))%*%attr(y,"covval")
  val
  covmean <- sqrtm(val)
  fid <- ssdiff + tr(attr(x,"covval") + attr(y,"covval") - 2.0 * covmean)
  fid
}
