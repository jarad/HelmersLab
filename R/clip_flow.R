#' Clip flow measurements to zero when no rain has fallen
#'
#' The flumes that measure flow sometimes get stuck. This is clear when there
#' has been no rainfall recently, but the flume is still recording flow. When
#' this occurs, we set the flow to zero.
#'
#' @param data A \code{data.frame} containing columns \code{date_time},
#'   \code{flow}, and \code{rain}.
#' @return A \code{data.frame} arranged by \code{date_time} with the flow column
#'   updated so that flow is 0 when no rain has fallen.
#' @import dplyr
#' @importFrom lubridate days
#' @export
clip_flow <- function(data) {
  stopifnot("flow" %in% names(data),
            "date_time" %in% names(data),
            "rain" %in% names(data))

  d <- data %>%
    dplyr::arrange(date_time)

  # Calculate moving average
  d$rain_24hour_moving_average <- NA
  for (i in 1:nrow(d)) {
    # Get times within 24 hours of the current time
    current_time <- d$date_time[i]
    ii <- which(d$date_time <= current_time &
                  d$date_time > current_time - lubridate::days(1))

    # Calculate 24 hour moving average
    d$rain_24hour_moving_average[i] <- mean(d$rain[ii], na.rm=TRUE)
  }

  # Set flow to zero when last 24 hours had no rain
  d %>%
    dplyr::mutate(flow = ifelse(rain_24hour_moving_average == 0,
                         0, flow)) %>%
    dplyr::select(-rain_24hour_moving_average)
}
