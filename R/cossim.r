#' Calculate cossine similarity of two vectors
#'
#' @param A vector
#' @param B vector
#' @return cosine similarity of vectors A and B
#' @example
#' cossim(c(1,2,3), c(4,5,6))

cossim <- function(A,B) { (sum(A*B))/sqrt((sum(A^2))*(sum(B^2))) }
