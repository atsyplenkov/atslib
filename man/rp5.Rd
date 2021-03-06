% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rp5_functions.R
\name{rp5}
\alias{rp5}
\title{A function to read and clean meteo data from \url{https://rp5.ru}}
\usage{
rp5(csv_path, timezone = "Europe/Moscow")
}
\arguments{
\item{csv_path}{a full path to a downloaded .csv file from \url{https://rp5.ru};
it works OK only with UTF-8 ecnoding and .csv extenstion}

\item{timezone}{timezone of the meteostation}
}
\description{
A function to read and clean meteo data from \url{https://rp5.ru}
}
\examples{
\dontrun{
library(atslib)

rp5("data/raw/meteo/Moscow/27605.16.11.2019.30.01.2020.1.0.0.ru.utf8.00000000.csv")

}

}
