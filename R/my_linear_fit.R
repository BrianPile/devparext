#' Create linear fit to x-y data
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return a dataframe
#' @export
#'
#' @examples
my_linear_fit = function(x, y) {
  linear_model = stats::lm(y ~ x)
  m = stats::coef(summary(linear_model))["x", "Estimate"]
  b = stats::coef(summary(linear_model))["(Intercept)", "Estimate"]
  r2 = summary(linear_model)$r.squared

  return(
    data.frame(
      m = m,
      b = b,
      r2 = r2
    )
  )

}
