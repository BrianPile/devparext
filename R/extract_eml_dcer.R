#' Extract EML DC extinction ratio from P-Vea data
#'
#' @param voltage A numeric vector of source voltage values
#' @param power A numeric vector of measure optical power values
#' @param V0 A numeric value for the reference voltage. The devault value is V0
#'   = 0V
#' @param V1 A numeric value for the applied EAM voltage which attenuates the
#'   laser power. The default value is V1 = -1.0V
#'
#' @return A numeric value DCER = 10*log10(P0/P1)
#' @export
#'
#' @examples
extract_eml_dcer = function(voltage, power, V0 = 0, V1 = -1.0) {
  p0 = stats::approx(voltage, power, V0)$y
  p1 = stats::approx(voltage, power, V1)$y
  DCER = 10*log10(p0/p1)
  return(DCER)
}
