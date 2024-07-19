#' Extract wavelength temperature coefficient
#'
#' @param temperature A vector of temperature data
#' @param peak_wl A vector of peak wavelength data
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' temps = c(25, 50, 75)
#' peak_wavs = c(1308, 1310.4, 1312.3)
#' temp_coef = extract_wl_temp_coef(temps, peak_wavs)$temp_coef
#' r2 = extract_wl_temp_coef(temps, peak_wavs)$rsquared
#' print(temp_coef)
extract_wl_temp_coef = function(temperature, peak_wl) {
  # stopifnot("")

  linearMod = stats::lm(peak_wl ~ temperature, data.frame("temperature" = temperature, "peak_wl" = peak_wl))
  temp_coef = stats::coef(summary(linearMod))["temperature", "Estimate"]
  rsq = summary(linearMod)$r.squared
  return_df = data.frame("temp_coef" = temp_coef,
                         "rsquared" = rsq)
  return(return_df)
}
