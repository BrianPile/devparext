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

  Isat = -b/2/a
  Isat = unname(Isat)

  return(Isat)
}
