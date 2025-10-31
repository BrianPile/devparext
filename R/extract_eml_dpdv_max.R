#' Extract the max dP/dV from the EAM P-Vea curve
#'
#' @param V a numeric vector of sourced EAM voltages
#' @param P a numeric vector of measured EML output powers
#'
#' @return a numeric value
#' @export
#'
# @examples
extract_eml_dpdv_max = function(V, P) {
  dPdV = my_derivative(V, P)
  Vop = extract_eml_vop(V, dPdV)
  dPdV_max = stats::approx(V, dPdV, Vop)$y
  return(dPdV_max)
}
