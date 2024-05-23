test_that("bptable works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  # Capture the output of the bptable function
  output <- capture.output(bptable(data, x, y, commentary = TRUE))

  # Define the expected parts of the output
  expected_bp_test <- "studentized Breusch-Pagan test"
  expected_commentary <- "Commentary:  There is significant evidence to conclude there is homoscedasticity in the model because the p-value: 0.5386  is greater than 0.05. This passes the equal variance assumption. "

  # Check if the output contains the expected parts
  expect_true(any(grepl(expected_bp_test, output)))
  expect_true(any(grepl(expected_commentary, output)))
})

# Source: ChatGPT for how to format the tests for graphs,
# using the expect_doppelganer function, and how to check
# if the test and the commentary are the same

library(ggplot2)
library(linearcheck)
library(vdiffr)


# Define residplot function outside of test_that
residplot <- function(data, x, y) {
  plot <-
    lm(y ~ x, data = data) |>
    broom::augment() |>
    ggplot(mapping = aes(y = .resid, x = .fitted)) +
    geom_point(color = "darkblue") +
    labs(x = "Fitted", y = "Residuals",
         title = "Residuals vs. Fitted Plot") +
    theme_bw()
  print(plot)
}

# Test that the residual plot function works
test_that("residplot works", {
  data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

  vdiffr::expect_doppelganger("Residual Plot",
                              function() residplot(data, "x", "y"))
})

