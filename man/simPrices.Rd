% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsim.R
\name{simPrices}
\alias{simPrices}
\title{Simulate randomly walking prices of stock or else on a given day}
\usage{
simPrices(startPrice, volatility, daysFromStart, expectedRR, sampSize = 10^3,
  daysInYear = 252, isRandom = FALSE)
}
\arguments{
\item{startPrice}{start price, today price}

\item{volatility}{volatility}

\item{daysFromStart}{day on which prices are simulated}

\item{expectedRR}{expected rate of return of stock or else}

\item{sampSize}{sample size}

\item{daysInYear}{number of business days in year}

\item{isRandom}{are samples random normal or evenly normal distributed}
}
\value{
vector of samples
}
\description{
Simulate randomly walking prices of stock or else on a given day
}
