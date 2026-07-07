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

  # input validation
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric vectors.")
  }

  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }

  if (length(x) == 1) {
    warning("`x` and `y` are single-valued. A linear fit required at aleast two points. Returning NA.")
    return(na_result)
  }

  if (anyNA(x) || anyNA(y)) {
    warning("`x` and `y` must not contain missing values. Returing NA.")
    return(na_result)
  }

  # model fitting
  linear_model = stats::lm(y ~ x)
  fit_summary = summary(linear_model)
  coefs = stats::coef(fit_summary)

  # handle other edge cases (e.g., x = c(2, 2, 2) where length > 1 but variance is 0)
  if (!"x" %in% rownames(coefs)) {
    warning("Slope could not be estimated (possibly due to zero variance in x). Returning NA.")
    return(na_result)
  }

  # extract and return results
  data.frame(
    m = unname(coefs["x", "Estimate"]),
    b = unname(coefs["(Intercept)", "Estimate"]),
    r2 = unname(fit_summary$r.squared)
  )

}
