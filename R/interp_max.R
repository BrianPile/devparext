#' Interpolate maximum value of x-y data
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return a numeric value
#' @export
#'
#' @examples
interp_max = function(x, y) {

  # check arguments
  # TODO: what if the maximum is at the first or last index?
  # what if x or y has less than 3 points?
  stopifnot(
    length(x) & length (y) >= 3,
    length(x) == length(y),
    is.numeric(x),
    is.numeric(y)
  )

  # get index of max value
  idx_max = which.max(y)

  # if idx_max is first or last index of y then return that value
  if (idx_max == 1 | idx_max == length(y)) {
    return(y[idx_max])
  }

  x_points = x[idx_max + -1:1]
  y_points = y[idx_max + -1:1]
  fit_quad = stats::lm(y_points ~ x_points + I(x_points^2)) # create quadratic fit model
  p = unname(stats::coef(fit_quad)) # get the polynomial coefficients
  a = p[1]
  b = p[2]
  c = p[3]

  # interpolate the maximum value by finding the vertex x-coordinate
  x_interp_max = -b/2/a

  # # plot to check
  # plot(x, y, type = "l")
  # new_x = seq(1, 5, length.out = 100)
  # graphics::lines(new_x, stats::predict(fit_quad, data.frame(x_points = new_x)), col = "red")

  return(x_interp_max)
}
