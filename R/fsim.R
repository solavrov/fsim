
#' Build a matrix whose columns are samples of standard normally distributed
#' variables with given correlations
#'
#' @param numOfSamp Number of samples of each variable. Default is 1000
#' @param corMatrix Correlation matrix of variables that gives the number of variable as well
#' @return Matrix where columns are samples of variables
#' @export
buildNormSamp <- function(corMatrix, numOfSamp = 1000) {

  numOfVar <- nrow(corMatrix)
  e <- matrix(rnorm(numOfVar * numOfSamp), nrow = numOfVar)
  choleski <- t(chol(corMatrix))

  t(choleski %*% e)

}

#' Build a correlation matrix of a given dimension and given correlation
#' that is the same for each pair of variabes
#'
#' @param dimOfMat Matrix dimension
#' @param corrOfVar Correlation
#' @return Correlation matrix
#' @export
buildCorMatrix <- function(dimOfMat, corrOfVar = 0.5) {

  matrix(rep(corrOfVar, dimOfMat^2), nrow = dimOfMat) - diag(corrOfVar,dimOfMat) + diag(dimOfMat)

}

#' Simulate randomly walking prices of stock or else on a given day
#'
#' @param startPrice start price, today price
#' @param volatility volatility
#' @param daysFromStart day on which prices are simulated
#' @param expectedRR expected rate of return of stock or else
#' @param sampSize sample size
#' @param daysInYear number of business days in year
#' @param isRandom are samples random normal or evenly normal distributed
#'
#' @return vector of samples
#' @export
simPrices <- function(startPrice, volatility, daysFromStart, expectedRR,
                      sampSize = 10^3,
                      daysInYear = 252,
                      isRandom = FALSE) {

  if (isRandom) {
    normSamp <- rnorm(sampSize)
  } else {
    normSamp <- qnorm((1:sampSize) / (sampSize + 1))
  }

  return(

    startPrice *
      exp(
        volatility * sqrt(daysFromStart / daysInYear) * normSamp +
          (expectedRR - volatility^2/2) * (daysFromStart / daysInYear)
      )

  )

}


#' Simulate random walk path of stock price or else for given period
#'
#' @param startPrice start price
#' @param volatility volatility
#' @param days number of days in path
#' @param expectedRR expected rate of return of stock or else
#' @param daysInYear number of business days in year
#'
#' @return vector of stock price path
#' @export
simPricePath <- function(startPrice, volatility, days, expectedRR,
                         daysInYear = 252) {

  path <- startPrice


  for (i in 2:(days+1)) {


    nextPrice <- path[i-1] * exp(
      volatility * sqrt(1 / daysInYear) * rnorm(1) +
        (expectedRR - volatility^2/2) * (1 / daysInYear)
    )

    path <- append(path, nextPrice)

  }

  return (path[-1])

}


