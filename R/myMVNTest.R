#' myMVNTest
#'
#' @param data data in the form of a matrix or data frame
#' @param alpha a significance level less than 1
#'
#' @return returns a chi square plot for the data and whether or not the data is likely MVN
#' @export
#'
#' @examples \dontrun{myMVNTest(data, 0.05)}
myMVNTest <- function(data, alpha = 0.05) {
  if (alpha >= 1) {
    stop("Error: Please use a value for alpha that is less than 1.")
  }


  # Find the number of observations the data has
  n <- dim(data)[1]
  p <- dim(data)[2]

  # Calculate relevant data
  xbar <- as.matrix(colMeans(data), ncol = 1)
  S <- cov(data)

  # Define the squared generalized distance
  d <- vector(mode = "numeric", length = n)

  # Loop through the data to calculate squared generalized distance
  for (j in 1:n) {
    deviation <- t(as.matrix(data[j,] - t(xbar), nrow = 1, ncol = p))
    d[j] <- t(deviation) %*% solve(S) %*% deviation
  }
  d <- sort(d)
  chisq_quant <- vector(mode = "numeric", length = n)
  for (j in 1:n) {
    # Find the associated p-value
    p_value <- (j - 0.5)/n
    # Find the standard normal quantile and put it in vector
    chisq_quant[j] <- qchisq(p_value, df = p)
  }
  library(ggplot2)
  # Create a Q-Q plot with jitter to account for possible overlaps
  qq_plot <- ggplot() +
    geom_point(aes(x = chisq_quant, y = d), position = position_jitter(width = 0.1, height = 0.1)) +
    theme_minimal() +
    labs(title = "Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")

  comparison <- d[1:n] <= qchisq(0.5, df = p)
  count <- sum(comparison)
  if (count/n >= 0.5) {
    result <- "Likely normal."
  }
  else {
    result <- "Likely not normal."
  }



  return(list(result = result, plot = qq_plot))
}

data <- read.table("/Users/tuckercapps/Downloads/T4-6.DAT")
myMVNTest(data, 0.05)
