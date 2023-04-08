#' myQQ
#'
#' @param data a vector containing data
#'
#' @return returns a QQ-plot of the given data
#' @export
#'
#' @examples
myQQ <- function(data) {

  # Define the number of observations and sort data in ascending order
  n <- length(data) # observations
  sorted <- sort(data)

  # Create vectors for the sample quantiles and standard normal quantiles
  s_quant <- quantile(sorted, probs = seq(0, 1, length.out = n + 1))
  norm_quant <- vector(mode = "numeric", length = n)

  for (j in 1:n) {
    # Find the associated p-value
    p_value <- (j - 0.5)/n
    # Find the standard normal quantile and put it in vector
    norm_quant[j] <- qnorm(p_value)
  }

  library(ggplot2)

  # Create a Q-Q plot with jitter to account for possible overlaps
  qq_plot <- ggplot() +
    geom_point(aes(x = norm_quant, y = sorted), position = position_jitter(width = 0.1, height = 0.1)) +
    theme_minimal() +
    labs(title = "Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  return(qq_plot)
}
