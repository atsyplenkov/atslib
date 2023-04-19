#' Interactive graph of several Y-variables
#'
#' @param .data dataframe.
#' @param .x character.
#' @param .y character.
#' @param .c4a_pal character.
#' @param ... other params applied to dyOptions
#'
#' @return a dygraph plot
#' @export
#'
#' @importFrom dygraphs dygraph dyOptions
#' @importFrom xts xts
#' @importFrom dplyr pull
#' @importFrom cols4all c4a

multi_plot <-
  function(
    .data,
    .x,
    .y,
    .c4a_pal = "tableau.color_blind",
    ...
    ){

    options(xts_check_TZ = FALSE)

    .y_len <-
      base::length(.y)
    .y_colors <-
      cols4all::c4a(palette = .c4a_pal, n = .y_len)
    # as.vector(see::metro_colors()[base::seq_len(.y_len)])

    .data_y1 <-
      .data[, .y]

    .data_datetime <-
      dplyr::pull(.data[, .x], 1)

    is_valid <-
      class(.data_datetime) %in% c("POSIXt", "POSIXct")

    is_date <-
      class(.data_datetime) %in% c("Date")

    if (any(is_valid)) {
      .tz <- lubridate::tz(.data_datetime[1])

    cat(paste0("Timezone is ", .tz, "\n"))

    xts::xts(x = .data_y1,
             order.by = .data_datetime,
             tzone = .tz) %>%
      dygraphs::dygraph() %>%
      dygraphs::dyOptions(colors = .y_colors,
                          useDataTimezone = TRUE,
                          ...)

    } else if (any(is_date)) {
      xts::xts(x = .data_y1,
               order.by = .data_datetime) %>%
        dygraphs::dygraph() %>%
        dygraphs::dyOptions(colors = .y_colors,
                            # useDataTimezone = TRUE,
                            ...)
    }
  }
