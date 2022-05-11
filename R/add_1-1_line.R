#' Adding 1-1 line to a ggplot
#'
#' @param type linetype
#' @param color line color
#' @param size line size
#' @param add_zero_line logical, add 0-0 lines
#' @param ... Other arguments passed to \code{geom_abline}
#'
#' @details This is a dummy workaround of the original \code{geom_abline}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(mpg, drat)) +
#' expand_limits(x = c(.0001, 0.0001),
#'               y = c(.0001, 0.0001)) +
#'   Add_1_line(add_zero_line = T) +
#'   geom_point()
#'
#'}
#'
#' @export

Add_1_line <- function(type = "dashed",
                       color = "black",
                       size = .6,
                       add_zero_line = FALSE,
                       ...){

  if (add_zero_line == T) {

    list(ggplot2::geom_abline(intercept =  0,
                slope = 1,
                linetype = type,
                color = color,
                size = size,
                ...),
         ggplot2::geom_hline(yintercept = 0,
                 linetype = type,
                 color = color,
                 size = size),
         ggplot2::geom_vline(xintercept = 0,
                 linetype = type,
                 color = color,
                 size = size))

  } else {

    ggplot2::geom_abline(intercept =  0,
                slope = 1,
                linetype = type,
                color = color,
                size = size)

  }
}
