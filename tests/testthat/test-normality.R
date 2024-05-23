# Source: ChatGPT for how to format the tests for graphs,
# using the expect_doppelganger function, and how to check
# if the test and the commentary are the same

library(testthat)
library(ggplot2)
library(lmtest)
library(vdiffr)

# Defining qqgraph function outside of test_that
qqgraph <- function(data, x, y) {
  plot <- ggplot(data = data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "lightblue") +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    labs(x = "X", y = "Y", title = "Quantile-Quantile Plot") +
    theme_bw()
  return(plot)
}

# Test for QQ plot
test_that("qqplot works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  vdiffr::expect_doppelganger("QQ Plot",
                              function() qqgraph(data, "x", "y")
  )
})

# Test for swtable and commentary
test_that("swtable works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  # Output of swtable function
  output <- capture.output(swtable(data, x, commentary = TRUE))

  # Expected output
  expected_sw_test <- "Shapiro-Wilk normality test"
  expected_commentary <- "Commentary:  There is significant evidence to conclude there is normality in the model because the p-value: 0.9606 is greater than 0.05. This passes the normality assumption. "

  # Check if the output is the same as the expected output
  expect_true(any(grepl(expected_sw_test, output)))
  expect_true(any(grepl(expected_commentary, output)))
})
