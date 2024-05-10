#' Extract LD operating current from P-I data
#'
#' @param I Sweep currents
#' @param P Sweep powers
#' @param Pop The operating power to extract Iop at
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' I = seq(0, 10)
#' P = sqrt(seq(0, 10))
#' Pop = 1
#' Iop = extract_iop_from_pi(I, P, Pop = Pop)
#' plot(I, P, type = "l")
#' points(Iop, Pop, col = "red")

extract_iop_from_pi = function(I, P, Pop = 10e-3) {

  # if the P-I curve doesn't reach Pop, return NA
  if (max(P) <= Pop) {
    return(NA)
  }

  # get Isat and then get only powers where current is less than Isat
  Isat = extract_isat_from_pi(I, P)
  P = P[I < Isat]

  idx = which.min(abs(P - Pop))
  Iop = I[idx]
}
