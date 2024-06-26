% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_derivative.R
\name{my_derivative}
\alias{my_derivative}
\title{Compute first-order numerical derivative using central difference method}
\usage{
my_derivative(x, y)
}
\arguments{
\item{x}{A numeric vector}

\item{y}{A numeric vector}
}
\value{
A numeric vector
}
\description{
Compute first-order numerical derivative using central difference method
}
\examples{
x = -10:10
y = x^2
dydx = my_derivative(x, y)
}
