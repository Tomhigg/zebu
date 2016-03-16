#' calculate the standard error of the mean
#' 
#' @description
#' calculatess the standard error of the mean for a vector.
#' 
#' @param x a numeric vector
#' 
#' @author
#' Tom Higginbottom
#' 
#' @examples
#' a <- rnorm(1000, 20, 5)
#' mean(a)
#' se(a)
#' 
#' @export se
#' @aliases se

se <- function(x) sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))