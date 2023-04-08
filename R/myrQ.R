#' myrQ
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples \dontrun{myrQ(data)}
myrQ <- function(data) {

  # Define the number of observations and sort data in ascending order
  n <- length(data) # observations
  sorted <- sort(data)


  # Create vectors for the sample quantiles and standard normal quantiles
  s_quant <- quantile(sorted, probs = seq(0, 1, length.out = n))
  norm_quant <- vector(mode = "numeric", length = n)

  for (j in 1:n) {
    # Find the associated p-value
    p_value <- (j - 0.5)/n
    # Find the standard normal quantile and put it in vector
    norm_quant[j] <- qnorm(p_value)
  }

  # Define necessary variables for rQ equation (JW 4-31)
  xbar <- mean(s_quant)
  qbar <- mean(norm_quant)
  numerator <- 0
  denominator1 <- 0
  denominator2 <- 0

  for (j in 1:n) {
    numerator <- numerator + (s_quant[j] - xbar)*(norm_quant[j] - qbar)
    denominator1 <- denominator1 + (s_quant[j] - xbar)^2
    denominator2 <- denominator2 + (norm_quant[j] - qbar)^2
  }
  denominator1 <- sqrt(denominator1)
  denominator2 <- sqrt(denominator2)
  rQ <- numerator/(denominator1*denominator2)

  cat("Correlation coefficient (rQ):", rQ, "\n")



}
data <- read.table("/Users/tuckercapps/Downloads/T4-6.DAT")
X1 <- data[,1]
myrQ(X1)
