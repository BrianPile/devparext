#' Extract current at a given voltage
#'
#' @param V Sweep voltages
#' @param I Sweep currents
#' @param V0 Voltage to extract current at. It is usually a single value, but it
#' can also be a vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' V = c(0, 1, 2, 3)
#' I = c(-5, -2, 2, 5)
#' V0 = 1.5
#' I = extract_i_from_iv(V, I, V0 = V0)
extract_i_from_iv = function(V, I, V0 = -1.0) {
  stopifnot("Va nd I are different lengths" = length(V) == length(I),
            "V0 is not in range of V"       = V0 >= min(V) & V0 <= max(V),
            "V is not numeric"              = is.numeric(V),
            "I is not numeric"              = is.numeric(I),
            "V0 is not numeric"             = is.numeric(V0))

  I = stats::approx(V, I, V0)$y
}
