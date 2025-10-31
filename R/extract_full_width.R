#' Extract full-width from a distribution
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param threshold a numeric value
#' @param plot_debug a Boolean value
#'
#' @return a numeric value
#' @export
#'
# @examples
extract_full_width = function(x, y, threshold = -20, plot_debug = FALSE) {
  # TODO: have option for linear vs dB y-values? for now it will be for dB values...optical spectra

  # normalize y-values
  y_norm = y - max(y)

  # find indices where y_norm is greater than or equal to threshold
  above_thresh_idx = which(y_norm >= threshold)

  # find first and last indices where y_norm is above the threshold
  left_idx = min(above_thresh_idx)
  right_idx = max(above_thresh_idx)

  # check that the left and right indices are not the first and last index, respectively
  if (left_idx == 1) {
    warning("Entire left-side is above threshold. Returning `NA`.")
    return(NA)
  } else if (right_idx == length(y_norm)) {
    warning("Entire right-side is above threshold. Returning `NA`.")
    return(NA)
  }

  # interpolate the left and right x-values at threshold
  left_x = approx(
    x = y_norm[left_idx + c(-1, 0)],
    y = x[left_idx + c(-1, 0)],
    xout = threshold
  )$y

  right_x = approx(
    x = y_norm[right_idx + c(0, 1)],
    y = x[right_idx + c(0, 1)],
    xout = threshold
  )$y

  # calculate full-width
  full_width = right_x - left_x

  # PLOT DEBUG #
  peak_x = x[which.max(y)]

  if (plot_debug == TRUE) {
    plot(x, y_norm, type = "l",
         xlim = peak_x + 5*c(-full_width, full_width),
         ylim = c(-70, 10))
    grid()
    abline(h = threshold, col = "red")
    abline(v = left_x, col = "red")
    abline(v = right_x, col = "red")

    print(stringr::str_glue("Full-width: {round(full_width, 3)}"))
    readline(prompt="Press [enter] to continue, or [esc] to quit:")
  }
  #

  # return full-width result
  return(full_width)
}

# graphical_check_full_width = function(df, group_data) {
#
#   print(group_data)
#   extract_full_width(df$wavelength, df$power, plot_debug = TRUE)
#
# }



# df_osa = data.table::fread(here("inst/extdata/poor_spectra_for_bw_extraction_development/ELS0025_OSA.csv"))
# df_osa = data.table::fread(here("inst/extdata/poor_spectra_for_bw_extraction_development/spx_els_osa_data.csv"))
#
# extract_full_width(wav, pow, threshold = -65, plot_debug = TRUE)


# df_osa |>
#   group_by(els_id, channel, If) |>
#   group_walk(~graphical_check_full_width(.x, .y))

