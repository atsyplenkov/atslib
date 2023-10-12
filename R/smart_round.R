#' @title Rounding to 3 signif digits
#'
#' @description This function rounds decimal values to a 3 signif digits.
#' @param x numerical value
#' @keywords round
#' @export
#' @examples
#' smart_signif(3.4699878)

smart_signif <- function(x){
  dplyr::case_when(
    x < 10 ~ signif(x, 3),
    dplyr::between(x, 10, 100) ~ round(x, 1),
    x >=100 ~ round(x)
  )
}

#' @title Rounding to 2 decimal digits
#'
#' @description This function rounds decimal values to a 2 decimal digits.
#' @param x numerical value
#' @keywords round
#' @export
#' @examples
#' smart_round(3.4699878)

smart_round <- function(x){
  dplyr::case_when(
    x < 10 ~ round(x, 2),
    dplyr::between(x, 10, 100) ~ round(x, 1),
    x >= 100 ~ round(x)
  )
}
