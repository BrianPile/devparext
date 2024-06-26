#' Extract Pf from EML P-Vea data
#'
#' @param voltage A numeric vector of source voltages
#' @param power A nueric vector of measured optical powers
#' @param V0 A numeric value of modulator bias voltage
#'
#' @return A numeric value for Pf
#' @export
#'
#' @examples
extract_eml_pf = function(voltage, power, V0 = 0) {
  Pf = stats::approx(voltage, power, V0)$y
  return(Pf)
}
