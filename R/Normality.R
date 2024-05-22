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
#' @importFrom tidyverse ggplot
#'
#' @export qqgraph


# library(ggplot2)
# library(tidyverse)


# Returning QQ Plot
qqgraph <- function(data, x, y) {

  plot <- ggplot(data = data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "lightblue") +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    labs(x = "X", y = "Y", title = "Quantile-Quantile Plot") +
    theme_bw()
  return(plot)
}


#' Printing out Shapiro-Wilk test and commentary if specified
#'
#' @param data the dataset name
#' @param x the x values
#' @param commentary a string of interpretations
#'
#' @return a summary table with Shapiro-Wilk test
#'
#' @importFrom stats shapiro.test
#'
#' @export swtable
#'

# Source for shapiro.test: https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/
swtable <- function(data, x, commentary = NULL) {
  x_col <- data$x
  sw_values <- shapiro.test(x_col)
  print(sw_values)

  if (commentary == TRUE) {
    commentary <- normality_commentary(sw_values)
    cat("\nCommentary: ", commentary, "\n")
  }
}

#' Printing out Shapiro-Wilk test commentary
#'
#' @param sw_values Shapiro-Wilk test values from model
#'
#' @return a string of interpretations
#'
#' @importFrom stats shapiro.test
#'
#' @export
#'

normality_commentary <- function(sw_values) {
  if (sw_values$p.value > 0.05) {
    commentary <- paste("There is significant evidence to conclude there is normality",
                         "in the model because the p-value:", round(sw_values$p.value, 4),
                         "is greater than 0.05. This passes the normality assumption.")
  } else {
    commentary <- paste("There is significant evidence to conclude there is not normality",
                        "in the model because the p-value:", round(sw_values$p.value, 4),
                        "is less than or equal to 0.05. This violates the normality assumption.")
  }
  return(commentary)
}


#swtable(data, x, commentary = TRUE)
#data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
#qqgraph(data, x, y)

