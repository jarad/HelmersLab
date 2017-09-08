context("spread_sampleID")

test_that("gives errors when missing columns", {
  d <- data.frame(flow = 1, sampleID = 1)
  expect_error(spread_sampleID(d[,-1]))
  expect_error(spread_sampleID(d[,-2]))
})

test_that("gives proper results", {
  d <- data.frame(flow = c(0,1,NA,1,0),
                  sampleID = c(NA,NA,1,NA,NA))

  expect_equal(spread_sampleID(d), d %>% dplyr::mutate(sampleID = c(NA,1,1,1,NA)))
})

test_that("gives proper results", {
  d <- data.frame(flow = c(0,1,NA,1,NA,1,0),
                  sampleID = c(NA,NA,1,NA,2,NA,NA))

  expect_equal(spread_sampleID(d), d %>% dplyr::mutate(sampleID = c(NA,1,1,2,2,2,NA)))
})
