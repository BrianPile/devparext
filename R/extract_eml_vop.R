#' Extract EAM operating voltage
#'
#' @param voltage A numeric vector of sourced EA voltages
#' @param dPdV A numeric vector of dPdV values
#'
#' @return A numeric value of the operating voltage
#' @export
#'
# @examples
extract_eml_vop = function(voltage, dPdV) {
  Vop = voltage[which.max(dPdV)]
  return(Vop)
}
