#' myQQ
#'
#' @param data a vector containing data
#'
#' @return returns a QQ-plot of the given data
#' @export
#'
#' @examples \dontrun{myQQ(data)}
myQQ <- function(data) {

  # Define the number of observations and sort data in ascending order
  n <- length(data) # observations
  sorted <- sort(data)

  # Create vectors for the sample quantiles and standard normal quantiles
  s_quant <- quantile(sorted, probs = seq(0, 1, length.out = n ))
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
    geom_point(aes(x = norm_quant, y = s_quant), position = position_jitter(width = 0.1, height = 0.1)) +
    theme_minimal() +
    labs(title = "Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  return(qq_plot)
}
data <- read.table("/Users/tuckercapps/Downloads/T4-6.DAT")
X1 <- data[,1]
myQQ(X1)
