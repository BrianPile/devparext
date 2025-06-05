#' Extract the output power at a given bias current
#'
#' @param I Sweep currents
#' @param P Sweep powers
#' @param I0 A bias current to extract output power at
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' I = seq(0, 10)
#' P = sqrt(seq(0, 5, length.out = length(I)))
#' I0 = 3.75
#' Pf = extract_pf_from_pi(I, P, I0 = I0)
extract_pf_from_pi = function(I, P, I0) {

  # stop the code execution if conditions not met
  # stopifnot(
  #   "current and power vectors must have length greater than 1" = length(I) > 1 & length(P) > 1
  # )

  # check if I0 is NA
  if (is.na(I0)) {
    # warning("extract_pf_from_pi: I0 is NA, returning NA")
    return(NA)
  }

  # return NA in some cases
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

  Pf = stats::approx(I, P, I0)$y

  return(Pf)
}
