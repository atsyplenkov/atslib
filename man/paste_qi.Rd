% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_ci.R
\name{paste_qi}
\alias{paste_qi}
\title{Print quantile interval}
\usage{
paste_qi(.x, type = "median", width = 0.95)
}
\arguments{
\item{.x}{numeric. Numeric vector containing the desired numbers.
Can contain NA}

\item{type}{character. Either \code{mean} or \code{median}. I.e. will function
calculate mean or median quantile interval}

\item{width}{numeric. vector of probabilities to use that
determine the widths of the resulting intervals. Only one probability is
allowed. I.e. \code{0.95} to calculate 95\% QI}
}
\value{
character
}
\description{
A function to print quantile intervals from \code{\link[ggdist:point_interval]{ggdist::mean_qi()}} or
\code{\link[ggdist:point_interval]{ggdist::median_qi()}}
}
