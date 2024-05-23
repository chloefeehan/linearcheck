# Source: ChatGPT

library(testthat)
library(ggplot2)
library(lmtest)
library(vdiffr)

# Defining acf_pacf function outside of test_that
acf_pacf <- function(data, x, y) {
  par(mfrow=c(2,2))
  acf(data[[x]], main = "X ACF Plot")
  pacf(data[[x]], main = "X PACF Plot")
  acf(data[[y]], main = "Y ACF Plot")
  pacf(data[[y]], main = "Y PACF Plot")
}

# Test for acf_pacf
test_that("acf_pacf works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  vdiffr::expect_doppelganger("ACF and PACF plots",
                              function() acf_pacf(data, "x", "y")
  )
})

# Test for dwtable and commentary
test_that("dwtable works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  # Output of dwtable function
  output <- capture.output(dwtable(data, x, y, commentary = TRUE))

  # Expected output
  expected_dw_test <- "Durbin-Watson test"
  expected_commentary <- "Commentary:  There is significant evidence to conclude there is independence in the model because the p-value: 0.6264 is greater than 0.05. This passes the independence assumption. "

  # Check if the output is the same as the expected output
  expect_true(any(grepl(expected_dw_test, output)))
  expect_true(any(grepl(expected_commentary, output)))
})
