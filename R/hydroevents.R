hydro_events <- function(dataframe,
                         q = NULL,
                         datetime = NULL,
                         window = 1) {

  locmin <- function(x, datetime, window = 1) {

    timestep <- as.double(signif(difftime(head(datetime)[5],
                                          head(datetime)[4],
                                          units = "hours"), 4))

    N2star <- round(window / timestep)
    N2star <- ifelse(N2star %% 2 == 0, N2star  + 1, N2star)
    Nobs <- length(x)
    Ngrp <- ceiling(Nobs / N2star)
    Nfil <- (N2star - 1L) / 2L
    Mid <- as.integer((N2star) / 2)
    LocMin <- sapply(seq(N2star, Nobs), function(i)
      min(x[seq(i - N2star + 1L, i)]) == x[i - Mid]
    )
    LocMin <- c(rep(FALSE, Nfil), LocMin, rep(FALSE, Nfil))
    return(LocMin)

  }

  dataframe %>%
    dplyr::mutate(q = zoo::na.approx(q, rule = 2)) %>%
    dplyr::mutate(LocMin = locmin(x = q,
                           datetime = datetime,
                           window = window),
           test = ifelse(LocMin == FALSE, NA, 1)) -> dataframe

  # Remove multiple local minimums
  dataframe$test <- lapply(seq(1, length(dataframe$test)), function(i)
    dataframe$test[i - 1L] == dataframe$test[i])

  dataframe$LocMin[dataframe$test == T] <- FALSE

  # Name the hydrological events
  dataframe$he <- NA
  dataframe$he[dataframe$LocMin == T] <- 2:(length(dataframe$he[dataframe$LocMin == T])+1)
  dataframe$he[1] <- 1

  # Fill the gaps of he's
  dataframe %>%
    dplyr::mutate(he = zoo::na.locf(he),
           he = as.factor(he)) %>%
    dplyr::select(-LocMin, - test) %>%
    dplyr::as_tibble() -> dataframe

  return(dataframe)
}
