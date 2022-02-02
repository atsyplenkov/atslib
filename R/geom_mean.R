#' Compute geometric mean/average
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
gmean <- function(x){

  x_na <- x[!is.na(x)]

  10^mean(log10(x_na))

}
