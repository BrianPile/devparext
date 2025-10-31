#' Create linear fit to x-y data
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return a dataframe
#' @export
#'
# @examples
my_linear_fit = function(x, y) {
  linear_model = stats::lm(y ~ x)

  if (nrow(stats::coefficients(summary(linear_model))) < 2) {
    warning("Problem in my_linear_fit(). Returning NA.")
    return(
      data.frame(
        m = NA,
        b = NA,
        r2 = NA
      )
    )
  }

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
