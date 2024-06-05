# linearcheck R package

This is a package that checks the assumptions (equal variance, normality, independence, linearity) for a simple linear regression. Do not use this package for a multiple linear regression as there is only one x variable included.

### Equal Variance Assumption

-   residplot() allows you to specify x and y variables in a dataset to build a residual plot.

-   bptable() allows you to specify x and y variables in a dataset and prints Breusch-Pagan test results. Prints Breusch-Pagan interpretation if commentary = TRUE.

### Normality Assumption

-   qqgraph() allows you to specify x and y variables in a dataset to build a Quantile-Quantile plot.

-   swtable() allows you to specify x and y variables in a dataset and prints Shapiro-Wilk test results. Prints Shapiro-Wilk interpretation if commentary = TRUE.

### Independence Assumption

-   acf_pacf() allows you to specify x and y variables in a dataset to build ACF and PACF plots for both x and y variables.

-   dwtable() allows you to specify x and y variables in a dataset and prints Durbin-Watson test results. Prints Durbin-Watson interpretation if commentary = TRUE.

### Linearity Assumption

-   linearplot() allows you to specify x and y variables in a dataset to build a scatterplot with line of best fit.

-   stats_table() allows you to specify x and y variables in a dataset and prints summary statitistics. Prints adjusted R-squared and p-value interpretation if commentary = TRUE.

# Installation

1.  Install and load in devtools package

```{r}
install.packages("devtools")
library(devtools)
```

2.  Install r package from github and load in linearcheck package

```{r}
install_github("chloefeehan/linearcheck")
library(linearcheck)
```

# Examples

### Equal Variance Assumption

```{r}
data(cars)
residplot(cars, speed, dist)
bptable(cars, speed, dist, commentary = TRUE)
```

### Normality Assumption

```{r}
data(cars)
qqgraph(cars, speed, dist)
swtable(cars, dist, commentary = TRUE)
```

### Independence Assumption

```{r}
data(cars)
acf_pacf(cars, speed, dist) 
dwtable(cars, speed, dist, commentary = TRUE)
```

### Linearity Assumption

```{r}
data(cars)
linearplot(cars, speed, dist, title = "Plot") 
stats_table(cars, speed, dist, commentary = TRUE)
```
