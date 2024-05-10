#' Extract peak wavelength from optical spectrum data
#'
#' @param wav A numeric vector of spectrum wavelengths
#' @param power A numeric vector of spectrum powers
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' wav = seq(1300, 1320, by = 1)
#' pow = rep(-70, length(wav))
#' pow[10] = -10
#' peak_wav = extract_peak_wav(wav, pow)
extract_peak_wav = function(wav, power) {
  idx_pmax = which.max(power)
  peak_wav = wav[idx_pmax]
}
