#' Print quantile interval
#'
#' @description
#' A function to print quantile intervals from [ggdist::mean_qi()] or
#' [ggdist::median_qi()]
#'
#' @param .x numeric. Numeric vector containing the desired numbers.
#' Can contain NA
#' @param type character. Either `mean` or `median`. I.e. will function
#' calculate mean or median quantile interval
#' @param width numeric. vector of probabilities to use that
#' determine the widths of the resulting intervals. Only one probability is
#' allowed. I.e. `0.95` to calculate 95% QI
#'
#' @return character
#' @export
#'
#' @md

paste_qi <-
  function(.x, type = "median", width = 0.95){

    stopifnot(width < 1)

    if (type == "median") {

      m_function <-
        function(i){
          median(i, na.rm = T)
        }

    } else if (type == "mean"){

      m_function <-
        function(i){
          mean(i, na.rm = T)
        }
    }

    q_w <-
      1 - width

    paste0(
      atslib::smart_round(
        m_function(.x)
      ),
      " [",
      scales::percent(width),
      " CI, ",
      atslib::smart_round(
        quantile(.x, q_w / 2, na.rm = T)
      ),
      "; ",
      atslib::smart_round(
        quantile(.x, 1 - (q_w/ 2), na.rm = T)
      ),
      "]"
    )

  }
