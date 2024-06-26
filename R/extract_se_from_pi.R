#' Extract slope efficiency at a given bias current
#'
#' @param I Sweep currents
#' @param P Sweep output powers
#' @param I0 Bias current to extract slope efficiency at
#' @param Ispan The current span width that a linear fit is applied to
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' I = seq(0, 10)
#' P = sqrt(seq(0, 5, length.out = length(I)))
#' I0 = 5
#' SE = extract_se_from_pi(I, P, I0 = I0, Ispan = 2)
extract_se_from_pi = function(I, P, I0, Ispan = 4e-3) {
  # stopifnot("I0 can not be missing value (NA)" = is.na(I0) == FALSE)

  if((is.na(I0) == TRUE)) {
    print("Problem extracting SE (1) !!!")
    SE = NA
    return(SE)
  }
  if(I0 >= max(I)) {
    print("Problem extracting SE (2) I0 is greater than Imax!!!")
    SE = NA
    return(SE)
  }

  # get data in a sub-span centered about I0
  Isub = I[I >= (I0-Ispan/2) & I <= (I0+Ispan/2)]
  Psub = P[I >= (I0-Ispan/2) & I <= (I0+Ispan/2)]

  # create linear fit to the P-I curve
  linear_model = stats::lm(P ~ I, data = data.frame("I" = Isub, "P" = Psub))

  # get the slope of the linear fit
  SE = stats::coef(summary(linear_model))["I", "Estimate"]
}
