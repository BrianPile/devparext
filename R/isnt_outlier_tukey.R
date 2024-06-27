#' Determine outliers using Tukey's rule
#'
#' @param x A numeric vector
#' @param k A numeric value, default is k = 1.5
#' @param na.rm A logical indicating whether NA values should be removed before
#'   the computation proceeds
#'
#' @return A logical vector, TRUE means that the value is not an outlier
#' @export
#'
#' @examples
isnt_outlier_tukey = function(x, k = 1.5, na.rm = TRUE) {
  quar = stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr = diff(quar)
  isnt_out = (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
  return(isnt_out)
}
