#' Extract forward voltage from V-I data
#'
#' @param I Sweep currents
#' @param V Sweep voltages
#' @param I0 Bias current to extract forward voltage at
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' I = seq(0, 10)
#' V = sqrt(seq(0, 10))
#' I0 = 4
#' Vf = extract_vf_from_vi(I, V, I0 = I0)
#' plot(I, V, type = "l")
#' points(I0, Vf, col = "red")
extract_vf_from_vi = function(I, V, I0) {
  stopifnot("I and V must have length greater than 1" = length(V) > 1 & length(I) > 1,
            "I0 must be in the range of I" = I0 >= min(I) & I0 <= max(I))

  Vf = stats::approx(I, V, I0)$y
}
