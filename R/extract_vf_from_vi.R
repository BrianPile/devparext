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

  # check if I0 is NA
  if (is.na(I0)) {
    warning("extract_vf_from_pi: I0 is NA, returning NA")
    return(NA)
  }

  # return NA in some cases
  if (I0 < min(I) | I0 > max(I)) {
    warning("extract_vf_from_pi: I0 is not in the range of I, returning vf = NA")
    return(NA)
  }

  # for (this_i in I0) {
  #   if (this_i < min(I) | this_i > max(I)) {
  #     warning("extract_vf_from_pi: I0 is not in the range of I, returning vf = NA")
  #     return(NA)
  #   }
  # }

  # if (length(I) == 1) {
  #   warning("extract_vf_from_pi: current vector is length 1, returning vf = NA")
  # }

  Vf = stats::approx(I, V, I0)$y
}
