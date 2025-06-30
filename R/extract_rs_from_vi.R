#' Extract series resistance from V-I data
#'
#' @param I Vector of currents
#' @param V Vector of voltages
#' @param I0 A bias current to extract the series resistance at
#' @param Ispan The span width to apply a linear fit to
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' I = seq(0, 100e-3, 1e-3)
#' V = 1 + seq(0, 1, length.out = length(I))
#' I0 = 50e-3
#' Ispan = 6e-3
#' Rs = extract_rs_from_vi(I, V, I0 = I0, Ispan = Ispan)
#' print(Rs)

extract_rs_from_vi = function(I, V, I0, Ispan = 4*1e-3) {
  # this is original parameter checking. changing to stopifnot() but need to
  # make sure it will still work as intended. BP 2024-05-10

  # change Ispan to be a percentage of max current (10%)

  if (is.na(I0) == TRUE) {
    return(NA)
  }

  # check if all currents are NA
  if(all(is.na(I))) {
    warning("All I values are NA, returning NA")
    return(NA)
  }

  if (I0 >= max(I) | I0 <= min(I)) {
    warning("I0 is not with in the range of currents")
    return(NA)
  }

  # stopifnot("I0 must be numeric" = is.numeric(I0),
  #           "I0 must be in the range of I" = I0 >= min(I) & I0 <= max(I))

  Ispan = 0.10 * max(I)
  Isub = I[I>=(I0-Ispan/2) & I<=(I0+Ispan/2)]
  Vsub = V[I>=(I0-Ispan/2) & I<=(I0+Ispan/2)]

  # create linear fit to the V-I subspan
  linearMod = stats::lm(V ~ I, data = data.frame("I" = Isub, "V" = Vsub))
  Rs = stats::coef(summary(linearMod))["I", "Estimate"] # get the slope of the linear fit
}
