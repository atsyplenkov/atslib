% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggthemes.R
\name{theme_hp}
\alias{theme_hp}
\title{A ggplot2 theme based on see::theme_lucid}
\usage{
theme_hp(
  base_font_family = "Noto Sans",
  base_font_size = 12,
  legend = "bottom",
  ...
)
}
\arguments{
\item{base_font_family}{base font family}

\item{base_font_size}{base font size}

\item{legend}{legend position}

\item{...}{Other arguments passed to \code{theme_lucid}}
}
\description{
A ggplot2 theme based on see::theme_lucid
}
\examples{
\dontrun{
library(see)

ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    labs(title = "A Lovely Plot",
         subtitle = "What can the subtitle tell us?") +
    theme_hp()
}

}
