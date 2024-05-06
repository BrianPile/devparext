extract_se_from_pi = function(I, P, I0, Ispan = 4e-3) {
  stopifnot("I0 can not be missing value (NA)" = is.na(I0) == FALSE)

  # get data in a sub-span centered about I0
  Isub = I[I >= (I0-Ispan/2) & I <= (I0+Ispan/2)]
  Psub = P[I >= (I0-Ispan/2) & I <= (I0+Ispan/2)]

  # create linear fit to the P-I curve
  linear_model = stats::lm(P ~ I, data = data.frame("I" = Isub, "P" = Psub))

  # get the slope of the linear fit
  SE = stats::coef(summary(linear_model))["I", "Estimate"]
}
