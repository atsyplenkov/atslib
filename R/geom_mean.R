#' Compute geometric mean/average
#'
#' @param x vector
#'
#' @return double
#' @export
#'
gmean <- function(x){

  x_na <- x[!base::is.na(x)]

  10^base::mean(base::log10(x_na))

}
