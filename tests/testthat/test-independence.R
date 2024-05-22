# Source: ChatGPT
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
