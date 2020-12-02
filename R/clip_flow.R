#' Clip flow measurements to zero when no rain has fallen
#'
#' The flumes that measure flow sometimes get stuck. This is clear when there
#' has been no rainfall recently, but the flume is still recording flow. When
#' this occurs, we set the flow to zero. This function sorts the
#' \code{data.frame} by date_time and then calculates the max rain in the
#' previous \code{window} rows of the data.frame (ignoring NAs).
#' If this max is zero, then flow is set to zero.
#'
#' @param data A \code{data.frame} containing columns \code{date_time},
#'   \code{flow}, and \code{rain}.
#' @param window An integer indicating the number of minutes to calculate the
#'   max rain over. Default is 24*60, the number of minutes in a day.
#' @return A \code{data.frame} arranged by \code{date_time} with the flow column
#'   updated so that flow is 0 when no rain has fallen.
#' @import dplyr
#' @importFrom lubridate days
#' @importFrom zoo rollapply
#' @export
clip_flow <- function(data,
                      window = 24*60) {
  stopifnot("flow" %in% names(data),
            "date_time" %in% names(data),
            "rain" %in% names(data))

  data$original = TRUE
  
  # Construct a data.frame with every minute
  d_full <- data.frame(date_time = seq(min(data$date_time),
                                       max(data$date_time),
                                       by = "1 min"))

  # Expand data set to include every minute
  d <- dplyr::full_join(d_full, data, by = "date_time")  %>%

    # Calculate rain in last 24 hours
    dplyr::ungroup() %>%
    dplyr::mutate(rain_max = zoo::rollapply(rain,
                                            window,
                                            max_or_na, # see below
                                            partial = TRUE,
                                            align = "right")) %>%

    # Set flow to 0 if no rain in last 24 hours
    dplyr::mutate(flow = ifelse(rain_max > 0 | is.na(rain_max), flow, 0)) %>%

    # Remove unnecessary rows and columns
    dplyr::filter(original) %>%
    dplyr::select(-rain_max, -original)

  stopifnot(all(dim(data) == dim(d)))

  return(d)
}



#' Returns max or NA (rather than -Inf) if there are no non-NA values
#'
#' The max function returns -Inf and a warning if there are no non-NA values in
#' their arguments. This function suppress the warning and returns NA.
#'
#' @param x a numeric vector
#'
#' @return the result of max or NA if there are no non-NA values
#'
max_or_na <- function(x) {
  if (all(is.na(x))) return(NA)
  return(max(x, na.rm = TRUE))
}
