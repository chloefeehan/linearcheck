#' Makes a residual plot to determine if the residuals demonstrate homoscedasticity
#'
#'
#' @param data the dataset name
#' @param x the x values
#' @param y the y values
#'
#' @return a residual plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom lmtest bptest lm
#'
#' @export residplot

# ## figure out how to import packages
# library(ggplot2)
# library(lmtest)

# Making residual plot
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

#' Prints out Breusch-Pagan Test and commentary if specified
#'
#' @param data the dataset name
#' @param x the x values
#' @param y the y values
#' @param commentary a string of interpretation for Breusch-Pagan Test
#'
#' @return a summary table and interpretation
#'
#' @importFrom lmtest bptest
#'
#' @export bptable
#'

bptable <- function(data, x, y, commentary = NULL) {
  bp_values <- bptest(y ~ x, data = data)
  print(bp_values)

  if (commentary == TRUE) {
    commentary <- equalvar_commentary(bp_values)
    cat("\nCommentary: ", commentary, "\n")
  }
}

#' Prints out Breusch-Pagan Test interpretation
#'
#' @param bp_values the model's Breusch-Pagan values
#'
#' @return an interpretation of results
#'
#' @importFrom lmtest bptest
#'
#' @export
#'

# Commentary helper function
equalvar_commentary <- function(bp_values) {
    if (bp_values$p.value > 0.05) {
      commentary <- paste("There is significant evidence to conclude there is homoscedasticity",
                          "in the model because the p-value:",round(bp_values$p.value, 4),
                          " is greater than 0.05. This passes the equal variance assumption.")
    } else {
      commentary <- paste("There is significant evidence to conclude there is heteroscedasticity",
      "in the model because the p-value:", round(bp_values$p.value, 4),
      " is less than or equal to 0.05. This violates the equal variance assumption.")

    }
  return(commentary)
}


#bptable(data, x, y, commentary = TRUE)
#data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

