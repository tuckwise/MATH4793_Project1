#' uniNormTest
#'
#' @param data # data in the form of dataframe, matrix, or vector
#'
#' @return # returns whether or not variables in a dataframe are normally distributed
#' @export
#'
#' @examples
#' \dontrun{uniNormTest(data)}
uniNormTest <- function(data) {

  # Define the number of observations and variables in the data
  n <- dim(data)[1] # observations
  p <- dim(data)[2] # variables

  # Find the mean of each variable and the covariance matrix
  xbar <- colMeans(data)
  S <- cov(data)

  # Loop through the data and find proportion of data that is within range
  for (i in 1:p) {

    lower1 <- xbar[i] - sqrt(S[i, i])
    upper1 <- xbar[i] + sqrt(S[i, i])

    counter1 <- 0
    for (j in 1:n) {
      if (data[j, i] > lower1 & data[j, i] < upper1) {
        counter1 <- counter1 + 1
      }
    }

    # Define our p-hats for each variable
    pName1 <- paste("pHat1", p, sep = "_")
    assign(pName1, counter1/n)

    # Compare our p-hats based on equations from JW 4-29
    if (abs(get(pName1) - 0.683) > 1.396/sqrt(n)) {
      firstTest <- FALSE
    }
    else {
      firstTest <- TRUE
    }

    lower2 <- xbar[i] - 2*sqrt(S[i, i])
    upper2 <- xbar[i] + 2*sqrt(S[i, i])

    counter2 <- 0
    for (j in 1:n) {
      if (data[j, i] > lower2 & data[j, i] < upper2) {
        counter2 <- counter2 + 1
      }
    }

    # Define our p-hats for each variable
    pName2 <- paste("pHat2", p, sep = "_")
    assign(pName2, counter2/n)

    if (abs(get(pName2) - 0.954) > 0.628/sqrt(n)) {
      secondTest <- FALSE
    }
    else {
      secondTest <- TRUE
    }


    if (firstTest & secondTest) {
      cat("Data for variable", i, "is likely normal.\n")
    }
    else if (!firstTest & secondTest) {
      cat("Data for variable", i, "is likely not normal.\n")
    }
    else if (!secondTest) {
      cat("Data for variable", i, "is definitely not normal.\n")
    }
  }
}
