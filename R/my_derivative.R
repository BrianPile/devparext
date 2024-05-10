#' Compute first-order numerical derivative using central difference method
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x = -10:10
#' y = x^2
#' dydx = my_derivative(x, y)
my_derivative = function(x, y) {
  d = c(NA)
  length(d) = length(x)

  for(ii in 1:length(x)) {
    if (ii == 1) {
      d[ii] = (y[ii+1]-y[ii])/(x[ii+1]-x[ii])
      # print("1")
    } else if (ii==length(x)) {
      d[ii] = (y[ii]-y[ii-1])/(x[ii]-x[ii-1])
      # print("last one")
    } else {
      d[ii] = (y[ii+1]-y[ii-1])/(x[ii+1]-x[ii-1])
      # print("main one")
    }
  }
  return(d)
}
