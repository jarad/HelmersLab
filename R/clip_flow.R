#' Clip flow measurements to zero when no rain has fallen
#'
#' The flumes that measure flow sometimes get stuck. This is clear when there
#' has been no rainfall recently, but the flume is still recording flow. When
#' this occurs, we set the flow to zero. This function sorts the
#' \code{data.frame} by date_time and then calculates the rain in the previous
#' 24*12 rows of the data.frame as this assumes the data are in 5 minute
#' increments.
#'
#' @param data A \code{data.frame} containing columns \code{date_time},
#'   \code{flow}, and \code{rain}.
#' @param window An integer (default is 24*12, i.e. the number of 5-minute
#' intervals in 24 hours) indicating the number of observations in the
#' window.
#' @return A \code{data.frame} arranged by \code{date_time} with the flow column
#'   updated so that flow is 0 when no rain has fallen.
#' @import dplyr
#' @importFrom lubridate days
#' @importFrom zoo rollapply
#' @export
clip_flow <- function(data,
                      window = 24 * 12) {  # number of  5-minute intervals in 24 hours
  stopifnot("flow" %in% names(data),
            "date_time" %in% names(data),
            "rain" %in% names(data))

  d <- data %>%
    dplyr::arrange(date_time) %>%

    # Calculate rain in last 24 hours
    dplyr::ungroup() %>%
    dplyr::mutate(rain_last24 = zoo::rollapply(rain,
                                               window,
                                               max,
                                               partial = TRUE,
                                               align = "right")) %>%

    # Set flow to 0 if no rain in last 24 hours
    dplyr::mutate(flow = ifelse(rain_last24 > 0, flow, 0)) %>%

    # Remove unnecessary columns
    dplyr::select(-rain_last24)
}
