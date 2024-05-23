# Source: ChatGPT for how to format the tests for graphs,
# using the expect_doppelganer function, and how to check
# if the test and the commentary are the same

library(testthat)
library(ggplot2)
library(lmtest)
library(vdiffr)

# Defining linear plot with regression line outside of test_that
linearplot <- function(data, x, y, title) {
  plot <- ggplot(data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", fill = "lightblue", color = "aquamarine4") +
    theme_bw() +
    labs(title = title) +
    theme_bw()
  print(plot)
}

# Test for linear plot
test_that("linearplot works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  vdiffr::expect_doppelganger("Linear Plot",
                              function() linearplot(data, "x", "y", "title")
  )
})

# Test for stats_table and commentary
test_that("stats_table works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  # Creating the model
  model <- lm(y ~ x, data = data)

  # Output of stats_table function
  output <- capture.output(stats_table(model, commentary = TRUE))

  # Expected output
  expected_stats_test <- "Call:"
  expected_commentary <- c("The adjusted r-squared value is 0.9853. Since 0.9853 is greater than or equal to 0.8 this demonstrates a strong positive relationship between the predictor and response variables.", "\n",
"There is significant evidence to conclude that at least one of the predictors in the model has a significant effect on the response because the p-value: 1e-04.")

  # Check if the output is the same as the expected output
  expect_true(any(grepl(expected_stats_test, output)))
  expect_true(any(grepl(expected_commentary, output)))
})

