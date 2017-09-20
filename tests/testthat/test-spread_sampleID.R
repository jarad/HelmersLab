context("spread_sampleID")

test_that("gives errors when missing columns", {
  d <- data.frame(flow = 1, sampleID = 1)
  expect_error(spread_sampleID(d[,-1]))
  expect_error(spread_sampleID(d[,-2]))
})

test_that("gives proper result for no samples", {
  d <- data.frame(flow = c(0,1,1,1,1,1,0),
                  sampleID = rep(NA,7))
  
  expect_equal(spread_sampleID(d), d)
})

test_that("gives proper result for one sample", {
  d <- data.frame(flow = c(0,1,NA,1,0),
                  sampleID = c(NA,NA,"1",NA,NA))

  expect_equal(spread_sampleID(d), 
               d %>% dplyr::mutate(sampleID = c(NA,"1","1","1",NA)))
})

test_that("gives proper result for two samples", {
  d <- data.frame(flow = c(0,1,NA,1,NA,1,0),
                  sampleID = c(NA,NA,"1",NA,"2",NA,NA))

  expect_equal(spread_sampleID(d), 
               d %>% dplyr::mutate(sampleID = c(NA,"1","1","2","2","2",NA)))
})

test_that("gives proper result when file ends with non-NAs", {
  d <- data.frame(flow = c(0,1,1,1,NA),
                  sampleID = c(NA,NA,NA,NA,"1"))
  
  expect_equal(spread_sampleID(d), 
               d %>% dplyr::mutate(sampleID = c(NA,"1","1","1","1")))  
})

test_that("gives proper result when file begins with non-NAs", {
  d <- data.frame(flow = c(NA,1,1,1,0),
                  sampleID = c("1",NA,NA,NA,NA))
  
  expect_equal(spread_sampleID(d), 
               d %>% dplyr::mutate(sampleID = c("1","1","1","1",NA)))  
})
