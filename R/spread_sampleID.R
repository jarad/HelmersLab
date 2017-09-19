#' Assigns a sampleID to the entire rain event.
#'
#' During a rain event, runoff samples are collected and, later, the sample is
#' sent to an analytical lab for analysis. These samples need to be attributed
#' to the entire event
#'
#' @param A \code{data.frame} containing the columns \code{sampleID} and
#' \code{flow}.
#' @import dplyr
#' @importFrom zoo na.locf
#' @export
#' @examples
#' d <- data.frame(flow = c(0,1,NA,1,0),
#'                 sampleID = c(NA,NA,1,NA,NA))
#' spread_sampleID(d)
#'
#' d <- data.frame(flow = c(0,1,NA,1,NA,1,0),
#'                 sampleID = c(NA,NA,1,NA,2,NA,NA))
#' spread_sampleID(d)
#'
spread_sampleID <- function(data) {
  stopifnot("flow"     %in% names(data),
            "sampleID" %in% names(data))

  tmp_value <- "9999999"

  data %>%
    mutate(
      sampleID = ifelse(flow > 0 | is.na(flow), sampleID, tmp_value),
      sampleID = zoo::na.locf(sampleID, fromLast = TRUE),  # carry backward
      sampleID = ifelse(flow > 0 & sampleID == tmp_value, NA, sampleID),
      sampleID = zoo::na.locf(sampleID, fromLast = FALSE), # carry forward
      sampleID = ifelse(sampleID == tmp_value, NA, sampleID))
}
