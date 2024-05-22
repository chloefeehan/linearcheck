library(grid)
library(gridExtra)

assumption <- function(data, x, y, title) {
  model <- lm(y ~ x, data = data)
  #Independence assumption
  dwtable <- dwtable(data, x, y, commentary = TRUE)

  #Linearity assumption
  stats_table <- stats_table(model, commentary = TRUE)

  #Normality assumption
  swtable <- swtable(data, x, commentary = TRUE)

  #Equal Variance Assumption
  bptable<- bptable(data, x, y, commentary = TRUE)

}



data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
assumption(data, x, y, "hi")
