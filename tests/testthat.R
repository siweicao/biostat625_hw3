# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(linreg)

linreg_model=linreg(Temp~Wind+Month+Ozone, airquality)
lm_model=lm(Temp~Wind+Month+Ozone, airquality)
lm_sum=summary(lm_model)
linregsum=linreg_sum(Temp~Wind+Month+Ozone, airquality)

test_that("linreg works", {
  expect_equal(
    linreg_model$coefficients,
    lm_model$coefficients
  )
  expect_equal(
    linreg_model$residuals,
    lm_model$residuals
  )
  expect_equal(
    linreg_model$rank,
    lm_model$rank
  )
  expect_equal(
    linreg_model$fitted.values,
    lm_model$fitted.values
  )
  expect_equal(
    linreg_model$df.residual,
    lm_model$df.residual
  )
})

test_that("linreg_sum works", {
  expect_equal(
    linregsum$coefficients,
    lm_sum$coefficients
  )
  expect_equal(
    linregsum$sigma,
    lm_sum$sigma
  )
  expect_equal(
    linregsum$r.squared,
    lm_sum$r.squared
  )
  expect_equal(
    linregsum$adj.r.squared,
    lm_sum$adj.r.squared
  )
  expect_equal(
    linregsum$fstatistic,
    lm_sum$fstatistic
  )
  expect_equal(
    linregsum$residuals,
    lm_sum$residuals
  )
  expect_equal(
    linregsum$df,
    lm_sum$df
  )
})






