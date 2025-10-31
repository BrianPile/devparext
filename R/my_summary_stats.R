#' My summary statistics function
#'
#' @param df A data frame with data in column x
#'
#' @return A data frame with summary statistics
#' @export
#' @importFrom rlang .data
#'
# @examples
my_summary_stats = function(df) {
  df = df |>
    dplyr::mutate(isnt_outlier = isnt_outlier_tukey(.data$x)) |>
    dplyr::summarize(count = dplyr::n(),
              outliers = sum(!.data$isnt_outlier, na.rm = TRUE),
              min = min(ifelse(.data$isnt_outlier == TRUE, .data$x, NA), na.rm = TRUE),         # this includes non-outliers only
              Q1 = stats::quantile(.data$x, 0.25, na.rm = TRUE),
              median = stats::median(.data$x, na.rm = TRUE),                                     # this includes all data points
              # median = median(ifelse(isnt_outlier == TRUE, x, NA), na.rm = TRUE), # this includes non-outliers only
              Q3 = stats::quantile(.data$x, 0.75, na.rm = TRUE),
              max = max(ifelse(.data$isnt_outlier == TRUE, .data$x, NA), na.rm = TRUE),         # this includes non-outliers only
              IQR = .data$Q3 - .data$Q1)
  return(df)
}
