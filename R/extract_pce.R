

#' Extract Power Conversion Efficiency
#'
#' @param I sweep currents
#' @param P sweep powers
#' @param V sweep voltages
#' @param I0 current to extract the parameter at
#'
#' @returns a numeric value as percentage
#' @export
#'
# @examples
extract_pce = function(I, P, V, I0) {

  # return NA in some cases

  # check if I0 is NA
  if (is.na(I0)) {
    # warning("extract_pf_from_pi: I0 is NA, returning NA")
    return(NA)
  }

  # check if all currents are NA
  if(all(is.na(I))) {
    warning("All I values are NA, returning NA")
    return(NA)
  }

  if (I0 < min(I) | I0 > max(I)) {
    # warning("extract_pf_from_pi: I0 is not in the range of I, returning pf = NA")
    return(NA)
  }

  if (length(I) == 1) {
    # warning("extract_pf_from_pi: current vector is length 1, returning pf = NA")
    return(NA)
  }

  if (sum(!is.na(P)) < 2) {
    message("Need at least two non-NA values to interpolate!!! Returning NA!!!")
    return(NA)
  }

  # calculate power conversion efficiency in percent
  Pf = extract_pf_from_pi(I, P, I0)
  Vf = extract_vf_from_vi(I, V, I0)
  PCE = 100 * Pf/I0/Vf
  return(PCE)

}
