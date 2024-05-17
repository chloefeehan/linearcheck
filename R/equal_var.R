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


bptable <- function(data, x, y) {
  bp_values <- bptest(y ~ x, data = data)
  bp_table <- data.frame(
    BP = bp_values$statistic,
    df = bp_values$parameter,
    pvalue = bp_values$p.value
  )

  table <- kable(bp_table, "html", caption = "Breusch-Pagan Test") |>
    kable_styling(full_width = FALSE)
  print(table)
}


bptable(data, x, y)

data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
