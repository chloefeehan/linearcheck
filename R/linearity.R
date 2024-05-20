#' Makes a scatter plot to determine if the data follows a linear pattern
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
#'
#' @export

library(ggplot2)
library(tidyverse)

linearplot <- function(data, x, y, title) {
  plot <- ggplot(data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", fill = "lightblue", color = "aquamarine4") +
    theme_bw() +
    labs(title = title)
  print(plot)
}

stats_table <- function(model, commentary = NULL) {
  model_summary <- summary(model)
  print(model_summary)

  if (commentary == TRUE) {
    commentary <- linearity_commentary(model)
    cat("\nCommentary: ", commentary, "\n")
  }
}

linearity_commentary <- function(model) {
  model_summary <- summary(model)
  adj_rsquared <- model_summary$adj.r.squared
  p_value <- model_summary$fstatistic["Pr(>F)"] ### GET P_VALUE
  if (adj_rsquared >= 0.8) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since ", round(adj_rsquared, 4),
                         " is greater than or equal to 0.8, this demonstrates a strong positive",
                         " relationship between the predictor and response variables.")
    } else if (adj_rsquared >= 0.5 & adj_rsquared < 0.8) {
      r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                           ". Since ", round(adj_rsquared, 4),
                           " is between 0.5 and 0.8, this demonstrates a moderate positive",
                           " relationship between the predictor and response variables.")
  } else if (adj_rsquared > 0 & adj_rsquared < 0.5) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since ", round(adj_rsquared, 4),
                           " is between 0 and 0.5, this demonstrates a weak positive",
                           " relationship between the predictor and response variables.")
  } else if (adj_rsquared > -0.5 & adj_rsquared < 0) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since ", round(adj_rsquared, 4),
                         " is between 0 and -0.5, this demonstrates a weak negative",
                         " relationship between the predictor and response variables.")
  } else if (adj_rsquared <= -0.5 & adj_rsquared > -0.8) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since ", round(adj_rsquared, 4),
                         " is between -0.5 and -0.8, this demonstrates a weak negative",
                         " relationship between the predictor and response variables.")
  } else if (adj_rsquared <= -0.8) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since ", round(adj_rsquared, 4),
                         " is less than or equal to -0.8, this demonstrates a strong negative",
                         " relationship between the predictor and response variables.")
  } else if (adj_rsquared == 0) {
    r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
                         ". Since the adjusted",
                         " r-squared equals zero, this demonstrates no",
                         " relationship between the predictor and response variables.")
  }
  if (p_value > 0.05) {
    p <- paste("There is significant evidence to conclude there is homoscedasticity",
                        "in the model because the p-value:",round(p_value, 4),
                        " is greater than 0.05. This passes the equal variance assumption.")
  } else {
    p <- paste("There is significant evidence to conclude there is heteroscedasticity",
                        "in the model because the p-value:", round(p_value, 4),
                        " is less than or equal to 0.05. This violates the equal variance assumption.")

  }

  return(r, p)
}




data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
model <- lm(y~x, data = data)
linearity_commentary(model)

linearplot(data, x, y, "title")

#source: https://www.statology.org/adjusted-r-squared-in-r/

