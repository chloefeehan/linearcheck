#' Makes a QQ Plot and does the Shapiro-Wilk's Test to Determine Normality
#'
#'
#' @param data the dataset name
#' @param x the x values
#' @param y the y values
#' @param title the title
#'
#' @return a scatterplot with a regression line
#'
#' @importFrom ggplot2 ggplot
#' @importFrom lmtest dwtest
#'
#' @export acf_pacf

# library(ggplot2)
# library(tidyverse)

# Returning ACF and PACF
acf_pacf <- function(data, x, y) {

  par(mfrow=c(2,2))
  acf(data$x, main = "X ACF Plot")
  pacf(data$x, main = "X PACF Plot")

  acf(data$y, main = "Y ACF Plot")
  pacf(data$y, main = "Y PACF Plot")
}


#' Printing out Durbin-Watson test and commentary if specified
#'
#' @param data a data frame containing the variables
#' @param x the independent variable name
#' @param y the dependent variable name
#' @param commentary a logical value indicating to print interpretations
#'
#' @return a Durbin-Watson test and interpretation
#'
#' @importFrom lmtest dwtest
#'
#' @export dwtable
#'

dwtable <- function(data, x, y, commentary = NULL) {
  dw_value <- dwtest(y ~ x, data = data)
  print(dw_value)

  if (commentary == TRUE) {
    commentary <- independence_commentary(dw_value)
    cat("\nCommentary: ", commentary, "\n")
  }
}

#' Printing out Durbin-Watson test and commentary if specified
#'
#' @param dw_value Durbin-Watson values from model
#'
#' @return a Durbin-Watson test interpretation
#'
#' @importFrom lmtest dwtest
#'
#' @export
#'

independence_commentary <- function(dw_value) {
  if (dw_value$p.value > 0.05) {
    commentary <- paste("There is significant evidence to conclude there is independence",
                        "in the model because the p-value:", round(dw_value$p.value, 4),
                        "is greater than 0.05. This passes the independence assumption.")
  } else {
    commentary <- paste("There is significant evidence to conclude there is not independence",
                        "in the model because the p-value:", round(dw_value$p.value, 4),
                        "is less than or equal to 0.05. This violates the independence assumption.")
  }
  return(commentary)
}


# dwtable(data, x, y, commentary = TRUE)
# data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
# acf_pacf(data, x, y)

