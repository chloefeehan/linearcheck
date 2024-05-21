
assumption <- function(data, x, y, title) {
  model <- lm(y ~ x, data = data)
  #Independence assumption
  acf_pacf(data, x, y)

  dwtable(data, x, y, commentary = TRUE)

  #Linearity assumption
  linearplot(data, x, y, title)

  stats_table(model, commentary = TRUE)

  #Normality assumption
  qqgraph(data, x, y)

  swtable(data, x, commentary = TRUE)

  #Equal Variance Assumption
  residplot(data, x, y)
  bptable(data, x, y, commentary = TRUE)


}



data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(2, 4, 7, 10, 11, 14))
assumption(data, x, y, "hi")
