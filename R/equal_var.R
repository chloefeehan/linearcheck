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
#' @importFrom lmtest bptest
#' @importFrom kableExtra kable
#'
#' @export

## figure out how to import packages
library(ggplot2)
library(lmtest)
library(kableExtra)

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


bptable <- function(data, x, y, commentary = NULL) {
  bp_values <- bptest(y ~ x, data = data)
  bp_table <- data.frame(
    BP = bp_values$statistic,
    df = bp_values$parameter,
    p_value = bp_values$p.value
  )
  table <- kable(bp_table, "html", caption = "Breusch-Pagan Test") |>
    kable_styling(full_width = FALSE)
  print(table)

  if (is.null(commentary)) {

    commentary <- equalvar_commentary(data, x, y)
    cat(commentary)
  }
}


equalvar_commentary <- function(data, x, y) {
  bptable(data, x, y)
    if (bp_values$p.value > 0.05) {
      commentary <- cat("There is significant evidence to conclude there is homoscedasticity
                        in the model because the p-value:",bp_values$p.value,"is greater
                        than 0.05. This passes the equal variance assumption.")
    } else {
      commentary <- cat("There is significant evidence to conclude there is heteroscedasticity
                        in the model because the p-value:",bp_values$p.value,"is less
                        than or equal to 0.05. This violates the equal variance assumption.")
    }
  return(commentary)
}

bptable(data, x, y, commentary = TRUE)
equalvar_commentary(data, x, y)
data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
