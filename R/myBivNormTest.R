#' myBivNormTeset
#'
#' @param data data in the form of a bivariate matrix/data frame
#' @param alpha a significance level less than 1
#'
#' @return returns a plot of the data, an ellipse of constant density, a list of actual and predicted data inside ellipse
#' @export
#'
#' @examples \dontrun{myBivNormTest(data, 0.1)}
myBivNormTest <- function(data, alpha = 0.05) {
  if (dim(data)[2] != 2) {
    stop("Error: Data must be bivariate.")
  }
  if (alpha >= 1) {
    stop("Error: Please use a value for alpha that is less than 1.")
  }

  library(ellipse)
  # Find the number of observations the data has
  n <- dim(data)[1]

  # Calculate relevant data
  mu <- as.matrix(colMeans(data), ncol = 1)
  Sigma <- cov(data)

  # Define the quadratic form
  Q <- vector(mode = "numeric", length = n)

  # Loop through the data to calculate quadratic form
  for (i in 1:n) {
    deviation <- as.matrix(c(data[i,1] - mu[1], data[i,2] - mu[2]), nrow = 2, ncol = 1)
    Q[i] <- t(deviation)%*%solve(Sigma)%*%deviation
  }
  chiSq <- qchisq(1 - alpha, 2)

  # Calculate the ellipse
  confidence_level <- 1 - alpha
  ellipse_points <- ellipse(Sigma, centre = c(mu[1], mu[2]), level = confidence_level, npoints = 100)

  # Plot the data and the ellipse
  plot(data, xlab = "X", ylab = "Y", main = "Bivariate Data with Ellipse")
  lines(ellipse_points, col = "red")

  # Calculate the actual and predicted percentage contents of the ellipse
  inside_ellipse <- sum(Q <= chiSq)/n
  predicted_content <- confidence_level

  # Return a list with the actual and predicted percentage contents
  result <- list(actual_content = inside_ellipse, predicted_content = predicted_content)
  return(result)
}
data <- read.table("/Users/tuckercapps/Downloads/T4-6.DAT")
X1 <- data[,1:2]
myBivNormTest(X1, 0.05)
