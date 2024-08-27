#' New one
#'
#' @param voltage a numeric vector
#' @param dPdV a numeric vector
#'
#' @return a numeric value
#' @export
#'
#' @examples
extract_eml_vop2 = function(voltage, dPdV) {
  Vop = interp_max(voltage, dPdV)
  return(Vop)
}
