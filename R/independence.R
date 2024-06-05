#' Makes an ACF and PACF graph for both the X and Y variables
#'
#'
#' @param data the dataset name
#' @param x the x values
#' @param y the y values
#'
#' @return An ACF and PACF graph for both the X and Y variables
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom lmtest dwtest
#'
#' @export acf_pacf

# Returning ACF and PACF
acf_pacf <- function(data, x, y) {

  # x and y variables to vectors
  # Source: ChatGPT to convert x and y to vectors
  x <- as.vector(data[[deparse(substitute(x))]])
  y <- as.vector(data[[deparse(substitute(y))]])

  # printing acf and pacf plots
  par(mfrow=c(2,2))
  acf({{x}}, main = "X ACF Plot")
  pacf({{x}}, main = "X PACF Plot")

  acf({{y}}, main = "Y ACF Plot")
  pacf({{y}}, main = "Y PACF Plot")
}


#' Printing out Durbin-Watson test and commentary if specified
#'
#' @param data a data frame containing the variables
#' @param x the independent variable name in quotations
#' @param y the dependent variable name in quotations
#' @param commentary a logical value indicating to print interpretations
#'
#' @return a Durbin-Watson test and interpretation
#'
#' @importFrom lmtest dwtest
#'
#' @export dwtable

#Prints out Durbin-Watson test and commentary if specified
dwtable <- function(data, x, y, commentary = NULL) {

  # x and y variables to vectors
  # Source: ChatGPT to convert x and y to vectors
  x <- as.vector(data[[deparse(substitute(x))]])
  y <- as.vector(data[[deparse(substitute(y))]])

  dw_value <- dwtest(y ~ x, data = data)
  print(dw_value)

  if (!is.null(commentary) && commentary == TRUE) {
    commentary <- independence_commentary(dw_value)
    cat("\nCommentary: ", commentary, "\n")
  }
}

#' Helper function for commentary
#'
#' @param dw_value Durbin-Watson values from model
#'
#' @return a Durbin-Watson test interpretation
#'
#' @importFrom lmtest dwtest
#'
#' @export

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


