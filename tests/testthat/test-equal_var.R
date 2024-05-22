# test_that("bptable works", {
#   data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
#
#   # Capture the output of the bptable function
#   output <- capture.output(bptable(data, x, y, commentary = TRUE))
#
#   # Define the expected parts of the output
#   expected_bp_test <- "studentized Breusch-Pagan test"
#   expected_commentary <- "Commentary:  There is significant evidence to conclude there is homoscedasticity in the model because the p-value: 0.5386  is greater than 0.05. This passes the equal variance assumption. "
#
#   # Check if the output contains the expected parts
#   expect_true(any(grepl(expected_bp_test, output)))
#   expect_true(any(grepl(expected_commentary, output)))
# })

#source: ChatGPT

library(ggplot2)
library(linearcheck)

test_that("residplot works", {
  # Create test data
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  # Call residplot function
  plot <- residplot(data, "x", "y")

  print(plot)

  # Check if the result is a ggplot object
  expect_true("ggplot" %in% class(plot))

  # Check if the plot contains expected components
  expect_true("geom_point" %in% plot$layers)
  expect_true("Fitted" %in% plot$labels$x)
  expect_true("Residuals" %in% plot$labels$y)
  expect_true("Residuals vs. Fitted Plot" %in% plot$labels$title)
})
