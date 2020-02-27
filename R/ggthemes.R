#' Minimal ggplot2 theme using the Ubuntu Condensed and Ubuntu fonts
#'
#' @param base_font_size base font size
#' @param legend legend position
#' @param base_font_family base font family
#' @param ... Other arguments passed to \code{theme_pubclean}
#'
#' @details The Ubuntu Condensed and Ubuntu fonts are both Google fonts;
#' they can be found at \url{https://fonts.google.com/specimen/Ubuntu+Condensed}
#' and \url{https://fonts.google.com/specimen/Ubuntu}. These fonts must be
#' installed locally on your computer for this theme to work.
#'
#' @examples
#' \dontrun{
#' library(ggpubr)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'     theme_clean()
#'}
#'
#' @export
theme_clean <- function(base_font_family = "Ubuntu",
                        base_font_size = 12,
                        legend = "bottom",
                        ...) {

  ggpubr::theme_pubclean(base_family = base_font_family,
                         base_size = base_font_size,
                         ...) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "Ubuntu Condensed"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = legend,
      strip.background = ggplot2::element_blank()
    )
}

#' Adding R2 value for a linear regression
#'
#' @param method model type
#' @param formula model formula
#' @param ... Other arguments passed to \code{stat_poly_eq}
#'
#' @details This is a dummy workaround of the original \code{stat_poly_eq}
#'
#' @examples
#' \dontrun{
#' library(ggpubr)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'          Add_R2() +
#'     theme_clean()
#'}
#'
#' @export

Add_R2 <- function(method = "lm",
                   formula = "y ~ x",
                   ...) {

  ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                           stat(adj.rr.label),
                                           sep = "~~~~")),
                        formula = formula,
                        rr.digits = 2,
                        # coef.digits = 2,
                        parse = TRUE,
                        ...)

}
