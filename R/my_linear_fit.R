#' Create linear fit to x-y data
#'
#' @param x a numeric vector
#' @param y a numeric vector
#'
#' @return A data frame
#' @export
#'
#' @examples
#' x = 1:3
#' y = 2*x + 5 + rnorm(length(x), mean = 0 , sd = 0.1)
#' my_linear_fit(x, y)
my_linear_fit = function(x, y) {

  na_result = data.frame(
    m = NA_real_,
    b = NA_real_,
    r2 = NA_real_
  )

  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric vectors.")
  }

  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }

  if (anyNA(x) || anyNA(y)) {
    warning("`x` and `y` must not contain missing values. Returing NA.")
    return(na_result)
  }

  linear_model = stats::lm(y ~ x)
  fit_summary = summary(linear_model)
  coefs = stats::coef(fit_summary)

  # if (nrow(stats::coef(summary(linear_model))) < 2) {
  #   warning("Problem in my_linear_fit(). Returning NA.")
  #   return(
  #     data.frame(
  #       m = NA_real_,
  #       b = NA_real_,
  #       r2 = NA_real_
  #     )
  #   )
  # }

  # m = stats::coef(summary(linear_model))["x", "Estimate"]
  # b = stats::coef(summary(linear_model))["(Intercept)", "Estimate"]
  # r2 = summary(linear_model)$r.squared
  #
  # return(
  #   data.frame(
  #     m = m,
  #     b = b,
  #     r2 = r2
  #   )
  # )

  data.frame(
    m = unname(coefs["x", "Estimate"]),
    b = unname(coefs["(Intercept)", "Estimate"]),
    r2 = unname(fit_summary$r.squared)
  )

}
