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

linearplot <- function(data, x, y, title, commentary = NULL) {
  library(ggplot2)
  library(tidyverse)

  plot <- ggplot(data, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", fill = "lightblue", color = "aquamarine4") +
    theme_bw() +
    labs(title = title)
  print(plot)
}

# commentary <- function() {
#  if
# }


#data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))

#linearplot(data, x, y, "title")

