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

#' A ggplot2 theme based on see::theme_lucid
#'
#' @param base_font_size base font size
#' @param legend legend position
#' @param base_font_family base font family
#' @param ... Other arguments passed to \code{theme_lucid}
#'
#' @examples
#' \dontrun{
#' library(see)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "A Lovely Plot",
#'          subtitle = "What can the subtitle tell us?") +
#'     theme_hp()
#'}
#'
#' @export

theme_hp <- function(base_font_family = "Noto Sans",
                     base_font_size = 12,
                     legend = "bottom",
                     ...) {

  see::theme_lucid(base_family = base_font_family,
                   base_size = base_font_size,
                   legend.position = legend,
                   ...) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_line(color = "grey50"))
}


#' Adding R2 value for a linear regression
#'
#' @param method model type
#' @param formula model formula
#' @param add_line Should we add a \code{geom_smooth} to the plot?
#' @param color color of the \code{geom_smooth}
#' @param lty linetype of the \code{geom_smooth}
#' @param conf_int Should we display confindence intervals of the \code{geom_smooth}?
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
                   formula = y ~ x,
                   add_line = T,
                   color,
                   lty = "dashed",
                   conf_int = F,
                   ...) {

  if(add_line == TRUE){
    if(missing(color)) {

      list(
        ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                                 stat(rr.label),
                                                 sep = "~~~~")),
                              formula = formula,
                              rr.digits = 2,
                              # coef.digits = 2,
                              parse = TRUE,
                              ...),
        ggplot2::geom_smooth(formula = formula,
                             method = method,
                             # color = color,
                             linetype = lty,
                             se = conf_int)
      )
    } else {
      list(
        ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                                 stat(rr.label),
                                                 sep = "~~~~")),
                              formula = formula,
                              rr.digits = 2,
                              # coef.digits = 2,
                              parse = TRUE,
                              ...),
        ggplot2::geom_smooth(formula = formula,
                             method = method,
                             color = color,
                             linetype = lty,
                             se = conf_int)
      )
    }
  } else {

    ggpmisc::stat_poly_eq(aes(label =  paste(stat(eq.label),
                                             stat(rr.label),
                                             sep = "~~~~")),
                          formula = formula,
                          rr.digits = 2,
                          # coef.digits = 2,
                          parse = TRUE,
                          ...)
  }
}

#' A ggplot theme for maps
#'
#' @param ... Other arguments passed to \code{theme_minimal}
#'
#' @details This is a slightly modified Timo Grossenbacher theme from
#' this tutorial \url{https://timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/}
#'
#' @export

theme_map <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Ubuntu", color = "#22211d"),
      # remove all axes
      axis.line = ggplot2::element_blank(),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      # add a subtle grid
      panel.grid.major = ggplot2::element_line(color = "#dbdbd6", size = 0.6),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      plot.margin = ggplot2::unit(c(.5, .5, .2, .5), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.spacing = ggplot2::unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      legend.title = ggplot2::element_text(size = 13),
      legend.text = ggplot2::element_text(size = 11, hjust = 0, color = "#4e4d47"),
      plot.title = ggplot2::element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "#4e4d47",
                                            margin = ggplot2::margin(b = -0.1,
                                                                     t = -0.1,
                                                                     l = 2,
                                                                     unit = "cm"),
                                            debug = F),
      plot.caption = ggplot2::element_text(size = 9,
                                           hjust = .5,
                                           margin = ggplot2::margin(t = 0.2,
                                                                    b = 0,
                                                                    unit = "cm"),
                                           color = "#939184"),
      ...
    )
}
