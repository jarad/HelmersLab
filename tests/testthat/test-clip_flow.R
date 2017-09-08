context("clip_flow")

test_that("clip_flow provides an error when data.frame doesn't have columns", {
  d <- data.frame(date_time = 1, flow = 1, rain = 1)
  expect_error(clip_flow(d[,-1]))
  expect_error(clip_flow(d[,-2]))
  expect_error(clip_flow(d[,-3]))
})

test_that("clip_flow provides warning if duplicate times exist", {
  now <- as.POSIXct(Sys.Date())
  d <- data.frame(date_time = rep(now,2), flow = 1, rain = 1)
  expect_warning(clip_flow(d))
})

test_that("clip_flow leaves flow when there is rain", {
  d <- data.frame(date_time = seq.POSIXt(as.POSIXct(Sys.Date()),
                                         as.POSIXct(Sys.Date()+3),
                                         by = "5 min"),
                  flow = 1,
                  rain = 1)
  expect_equal(d, clip_flow(d))
})

test_that("clip_flow zeros flow when there is no rain", {
  d <- data.frame(date_time = seq.POSIXt(as.POSIXct(Sys.Date()),
                                         as.POSIXct(Sys.Date()+3),
                                         by = "5 min"),
                  flow = 100,
                  rain = 0)
  expect_equal(d %>% dplyr::mutate(flow = 0), clip_flow(d))
})
