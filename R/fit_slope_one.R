#' Linear Fitting with Forced Slope of One
#'
#' Fits a linear model where the slope is strictly constrained to 1, leaving
#' only the y-intercept to be estimated. This function is designed to return a
#' single-row data frame, making it fully compatible with `dplyr::summarize()`.
#'
#' @param x A numeric vector of independent (predictor) values.
#' @param y A numeric vector of dependent (response) values.
#'
#' @return A data frame (specifically a `data.frame`) with three numeric columns:
#'   \item{slope}{The fixed slope, which is always 1.}
#'   \item{intercept}{The estimated y-intercept coefficient.}
#'   \item{r_squared}{The manually calculated coefficient of determination ($R^2$).
#'   Note that because the slope is constrained, this value can mathematically
#'   be negative if the model fits worse than a simple horizontal line at the mean of `y`.}
#'
# @importFrom stats lm coef mean
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Single use case:
#' fit_slope_one(x = c(1, 2, 3), y = c(2.1, 2.9, 4.2))
#'
#' # Using inside a dplyr pipeline with groups:
#' df <- data.frame(
#'   group = rep(c("A", "B"), each = 4),
#'   x     = c(1, 2, 3, 4,   1, 2, 3, 4),
#'   y     = c(2.1, 2.9, 4.2, 4.8,   5.5, 6.4, 7.6, 8.5)
#' )
#'
#' df %>%
#'   group_by(group) %>%
#'   summarize(fit_slope_one(x, y))
#'
fit_slope_one <- function(x, y) {
  # 1. Input Validation & Handling Missing Values
  if (length(x) != length(y)) {
    stop("Arguments 'x' and 'y' must be vectors of the same length.")
  }

  # Remove missing pairs to ensure correct R2 calculations
  complete_cases <- !is.na(x) & !is.na(y)
  x_clean <- x[complete_cases]
  y_clean <- y[complete_cases]

  if (length(x_clean) < 1) {
    return(data.frame(slope = 1, intercept = NA_real_, r_squared = NA_real_))
  }

  # 2. Fit the constrained model using namespaced stats functions
  model <- stats::lm(y_clean ~ 1 + offset(x_clean))
  intercept_val <- unname(stats::coef(model)["(Intercept)"])

  # 3. Manually calculate the true R-squared
  y_pred        <- 1 * x_clean + intercept_val
  ss_residual   <- sum((y_clean - y_pred)^2)
  ss_total      <- sum((y_clean - mean(y_clean))^2)

  # Avoid division by zero if all y values are identical
  if (ss_total == 0) {
    r_squared_val <- if (ss_residual == 0) 1 else -Inf
  } else {
    r_squared_val <- 1 - (ss_residual / ss_total)
  }

  # 4. Return as a clean data frame
  data.frame(
    slope     = 1,
    intercept = intercept_val,
    r_squared = r_squared_val
  )
}
