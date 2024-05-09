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
  stopifnot("current and power vectors must have length greater than 1" = length(I) > 1 & length(P) > 1,
            "I0 is not in the range of I" = I0 >= min(I) & I0 <= max(I))

  Pf = stats::approx(I, P, I0)$y
}
