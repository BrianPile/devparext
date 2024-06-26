#' Extract EAM photocurrent resistance
#'
#' @param voltage A numeric vector of sourced EA voltages
#' @param dVdI A numeric vector of calculated dVdI values
#' @param V0 A numeric value of the extraction voltage
#'
#' @return A numeric value of the photocurrent resistance at the operating voltage
#' @export
#'
#' @examples
extract_eml_rphoto = function(voltage, dVdI, V0) {
  voltage_sub = voltage[dVdI > 0]
  dVdI_sub = dVdI[dVdI > 0]

  # if (isempty(dVdI_sub)) {
  #   return(NA)
  # }

  if (length(dVdI_sub) == 0) {
    return(NA)
  }

  # Rphoto = dVdI[which.min(dVdI)]
  Rphoto = dVdI_sub[voltage_sub == V0]
  return(Rphoto)
}
