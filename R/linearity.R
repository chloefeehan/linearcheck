#' Makes a scatter plot to determine if the data follows a linear pattern
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


# library(ggplot2)
# library(tidyverse)


# Creates a linear plot with line of best fit
linearplot <- function(data, x, y, title) {
  plot <- ggplot(data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", fill = "lightblue", color = "aquamarine4") +
    theme_bw() +
    labs(title = title) +
    theme_bw()
  print(plot)
}

#' Collects summary statistics for model and prints interpretations
#'
#' @param model the linear model
#' @param commentary a string of interpretations
#'
#' @return a summary table and interpretation
#'
#' @importFrom lmtest summary
#'
#' @export

stats_table <- function(model, commentary = NULL) {
  model_summary <- summary(model)
  print(model_summary)

  if (commentary == TRUE) {
    commentary <- linearity_commentary(model)
    cat(commentary, "\n")
  }
}

#' Writes adjusted r-squared and p-value interpretations
#'
#' @param model the linear model
#'
#' @return interpretations
#'
#' @importFrom lmtest summary
#'
#' @export

linearity_commentary <- function(model) {
  #Get summary statistics
  model_summary <- summary(model)
  # Get adjusted r-squared value
  adj_rsquared <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic[1]
  df1 <- model_summary$fstatistic[2]
  df2 <- model_summary$fstatistic[3]
  # Calculate the p-value for the F-statistic
  p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)

  #Conditions for adjusted r-squared value and interpretations
  if (adj_rsquared >= 0.8) {
    direction <- "positive"
    value <-  "is greater than or equal to 0.8"
    strength <- "a strong"
  } else if (adj_rsquared >= 0.5 & adj_rsquared < 0.8) {
    direction <- "positive"
    value <-  "is between 0.5 and 0.8"
    strength <- "a moderate"
  } else if (adj_rsquared > 0 & adj_rsquared < 0.5) {
    direction <- "positive"
    value <-  "is between 0 and 0.5"
    strength <- "a weak"
  } else if (adj_rsquared > -0.5 & adj_rsquared < 0) {
    direction <- "negative"
    value <-  "is between -0.5 and 0"
    strength <- "a weak"
  } else if (adj_rsquared <= -0.5 & adj_rsquared > -0.8) {
    direction <- "negative"
    value <-  "is between -0.8 and -0.5"
    strength <- "a moderate"
  } else if (adj_rsquared <= -0.8) {
    direction <- "negative"
    value <-   "is less than or equal to -0.8"
    strength <- "a strong"
  } else if (adj_rsquared == 0) {
    direction <- ""
    value <-   "equals zero"
    strength <- "no"
  }
  #Pastes together the adjusted r-squared interpretations
  r <- paste0("The adjusted r-squared value is ", round(adj_rsquared, 4),
              ". Since ", round(adj_rsquared, 4),
              " ", value, " this demonstrates ", strength, " ", direction,
              " relationship between the predictor and response variables.")

  #Conditions for p-value interpretations
  if (p_value > 0.05) {
    p <- paste0("There is not enough evidence to conclude that at least one of the ",
                "predictors in the model has a significant effect on the response ",
                "because the p-value: ",round(p_value, 4),".")
  } else {
    p <- paste0("There is significant evidence to conclude that at least one of the ",
                "predictors in the model has a significant effect on the response ",
                "because the p-value: ", round(p_value, 4),".")
  }

  # Pastes both interpretations
  commentary <- cat(r, p, sep = "\n")
  return(commentary)
}

# data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
# model <- lm(y~x, data = data)
# linearity_commentary(model)
#
#stats_table(model, commentary = TRUE)

#linearplot(data, x, y, "title")

#source: https://www.statology.org/adjusted-r-squared-in-r/

