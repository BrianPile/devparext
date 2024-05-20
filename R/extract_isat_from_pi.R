#' Extract saturation current (Isat) from power-current data
#'
#' @param I A numeric vector of sweep currents
#' @param P A numeric vector of power measurements
#'
#' @return A numeric value
#' @export
#'
#' @examples
extract_isat_from_pi = function(I, P) {

  idx_Pmax = which.max(P)
  Isat0 = I[idx_Pmax]

  if (idx_Pmax == length(I)) {
    Isat = utils::tail(I, 1)
    return(Isat)
  }

  if (idx_Pmax == 1) {
    Isat = I[1]
    return(Isat)
  }

  idx = c(idx_Pmax-1, idx_Pmax, idx_Pmax+1)
  xpoints = I[idx]
  xpoints2 = I[idx]^2
  ypoints = P[idx]
  model = stats::lm(ypoints ~ xpoints + xpoints2)

  a = stats::coef(model)["xpoints2"]
  b = stats::coef(model)["xpoints"]
  c = stats::coef(model)["(Intercept)"]

  if (any(is.na(c(a, b, c)))) {
    warning("extract_isat_from_pi: one of the linear model coefficients wasd NA. defaulting to Isat at Pmax no fitting.")
    Isat = I[idx_Pmax]
  } else {
    Isat = -b/2/a
    Isat = unname(Isat)
  }

  return(Isat)
}
