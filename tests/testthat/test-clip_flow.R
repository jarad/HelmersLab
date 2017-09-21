suppressMessages(library("dplyr"))

context("clip_flow")

ctime <- as.POSIXct(900*round(as.numeric(Sys.time())/900),
                    origin="1970-01-01")

test_that("provides an error when data.frame doesn't have columns", {
  d <- data.frame(date_time = 1, flow = 1, rain = 1)
  expect_error(clip_flow(d[,-1]))
  expect_error(clip_flow(d[,-2]))
  expect_error(clip_flow(d[,-3]))
})

test_that("leaves flow when there is rain", {
  d <- data.frame(date_time = seq.POSIXt(ctime,
                                         ctime + lubridate::days(3),
                                         by = "5 min"),
                  flow = 1,
                  rain = 1)
  expect_equal(clip_flow(d), d)
})


test_that("leaves flow when rain is all NAs", {
  d <- data.frame(date_time = seq.POSIXt(ctime,
                                         ctime + lubridate::days(3),
                                         by = "5 min"),
                  flow = 1,
                  rain = NA)
  expect_equal(clip_flow(d), d)
})

test_that("zeros flow when there is no rain", {
  d <- data.frame(date_time = seq.POSIXt(ctime,
                                         ctime + lubridate::days(3),
                                         by = "5 min"),
                  flow = 100,
                  rain = 0)
  expect_equal(clip_flow(d), d %>% mutate(flow = 0))
})

test_that("deals with NAs in rain", {
  # Rain
  d <- data.frame(date_time = seq.POSIXt(ctime,
                                         ctime + lubridate::days(3),
                                         by = "5 min"),
                  flow = 1,
                  rain = 1)
  d$rain[2] = NA
  expect_equal(clip_flow(d), d)

  # No rain
  d$rain = 0
  d$rain[2] = NA
  expect_equal(clip_flow(d), d %>% mutate(flow = ifelse(is.na(flow), NA, 0)))
})
#
# test_that("uses last 24 hours" , {
#   d <- data.frame(date_time = seq.POSIXt(ctime,
#                                          ctime + lubridate::days(3),
#                                          by = "10 min"),
#                   flow = 1) %>%
#     mutate(rain = ifelse(date_time < ctime + lubridate::days(1),
#                          1, 0))
# })
