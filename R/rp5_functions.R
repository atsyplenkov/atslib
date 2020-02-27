#' A function to read and clean meteo data from \url{https://rp5.ru}
#'
#' @param csv_path a full path to a downloaded .csv file from \url{https://rp5.ru};
#' it works OK only with UTF-8 ecnoding and .csv extenstion
#' @param timezone timezone of the meteostation
#'
#' @examples
#' \dontrun{
#' library(atslib)
#'
#' rp5("data/raw/meteo/Moscow/27605.16.11.2019.30.01.2020.1.0.0.ru.utf8.00000000.csv")
#'
#'}
#'
#' @export

rp5 <- function(csv_path, timezone = "Europe/Moscow") {

  meteo <- read.csv(csv_path,
                    header = T, encoding = "UTF-8",
                    sep = ";", skip = 6,
                    row.names = NULL)

  names(meteo) <- c("time", names(meteo)[3:ncol(meteo)])
  meteo <- meteo[c("time", "RRR", "T", "Po")]
  colnames(meteo) <- c("time", "RRR", "TTT", "Po")

  is.na(meteo$RRR) <- meteo$RRR == "No precipitation" |
    meteo$RRR == "Trace of precipitation" |
    meteo$RRR == "Следы осадков" |
    meteo$RRR == "Осадков нет"

  meteo %>%
    dplyr::mutate(time = as.POSIXct(strptime(time, "%d.%m.%Y %H:%M")),
           time = lubridate::force_tz(time = time,
                                      tzone = timezone),
           RRR = as.numeric(as.character(RRR)),
           days = lubridate::floor_date(time, "day"),
           months = lubridate::floor_date(time, "month"),
           years = lubridate::year(time),
           #Pmm = Po * 13.6 / 1000
           Pmm = Po * 133.322) %>%
    dplyr::arrange(time) -> meteo

  return(meteo)

}
