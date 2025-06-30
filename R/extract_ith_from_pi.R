#' Extract threshold current
#'
#' @param I A numeric vector of sourced current values
#' @param P A numeric vector of measured power values
#' @param n1_smooth Smooth span for P-I curve
#' @param n2_smooth Smooth span for P-I curve 1st derivative
#' @param n3_smooth Smooth span for P-I curve 2nd derivative
#' @param plot_debug A Boolean value for setting the plot inspection mode
#'
#' @return
#' @export
#' @importFrom graphics abline hist grid par
#' @importFrom stats approx
#'
#' @examples

extract_ith_from_pi = function(I, P, n1_smooth = 1, n2_smooth = 1, n3_smooth = 1, plot_debug = FALSE) {
  # TODO: remove negative values from second derivative?

  # check the data for NA, error if found
  if (any(is.na(c(I, P)))) {
    message("NA's found in current or power! Returning NA!!!")
    return(list(NA, NA))
  }

  # remove duplicated data
  P = P[!duplicated(I)]
  I = I[!duplicated(I)]

  # apply first level of smoothing to power
  P = my_smooth(P, N = n1_smooth)

  # normalize the power
  P = P / max(P)

  # subset P-I data at currents where:
  #   1. I is less than Isat
  #   2. P is less than 0.2
  Isat = I[which.max(P)]
  idx = P < 0.15 & I < Isat
  I = I[idx]
  P = P[idx]

  # return NA's if I or P is zero length, or length 1
  if (length(I) <= 1 | length(P) <= 1) {
    return(list(NA, NA))
  }

  # calculate 1st and 2nd derivatives
  d1 = my_derivative(I, P)
  d1 = my_smooth(d1, n2_smooth)         # second level of smoothing
  d2 = my_derivative(I, d1)
  d2 = my_smooth(d2, N = n3_smooth)     # third level of smoothing


  #### 1st derivative method ####

  # find the "average level of d1 above threshold by using a histogram of values
  # greater than the mean
  d1_above_mean = d1[d1 > mean(d1)]
  if (length(d1_above_mean) == 0) {
    Ith_1st_deriv = NA
    d1_level_above_thresh = NA
  } else {
    h_midpoints = hist(d1[d1 > mean(d1)], breaks = 20, plot = FALSE)$mids
    h_counts = hist(d1[d1 > mean(d1)], breaks = 20, plot = FALSE)$counts
    d1_level_above_thresh = h_midpoints[which.max(h_counts)]

    # find the current where d1 is half of the avg above threshold value (that is
    # the definition of the first-derivative threshold current)
    # idx_min = which.min(abs(d1 - d1_level_above_thresh/2))
    # d1_points = d1[idx_min + -1:1]     # get closest point and nearest neighbors
    # I_points = I[idx_min + -1:1]       # get corresponding current points
    # Ith_1st_deriv = approx(d1_points, I_points, d1_level_above_thresh/2)$y

    # get vector representing d1 - d1_level_above_thresh/2 zero crossings
    zero_crossing = c(0, diff(sign( d1 - d1_level_above_thresh/2 )))

    # get current at first upward zero crossing, and the index
    first_upward_crossing = I[zero_crossing > 1][1]
    if (is.na(first_upward_crossing)) {
      Ith_1st_deriv = NA
    } else {
      idx = which(I == first_upward_crossing)
      d1_points = d1[idx + -1:1]     # get closest point and nearest neighbors
      I_points = I[idx + -1:1]       # get corresponding current points
      Ith_1st_deriv = approx(d1_points, I_points, d1_level_above_thresh/2)$y
    }

  }


  #### 2nd derivative method ####
  d2_rms = sqrt(mean(d2^2))
  pks = pracma::findpeaks(d2, minpeakheight = 2*d2_rms, sortstr = TRUE)

  # if NULL was returned, set ITh_2nd_deriv to NA, else do quadratic fitting
  if ( is.null(pks) ) {
    Ith_2nd_deriv = NA
  } else {
    idx_pk = min(pks[, 2])

    d2_points = d2[idx_pk + -1:1]
    I_points  = I[idx_pk + -1:1]
    I_points2  = I[idx_pk + -1:1]^2

    # quadratic fit
    model = stats::lm(d2_points ~ I_points + I_points2)
    a = stats::coef(model)["I_points2"]
    b = stats::coef(model)["I_points"]
    c = stats::coef(model)["(Intercept)"]

    Ith_2nd_deriv = -b/2/a
    Ith_2nd_deriv = unname(Ith_2nd_deriv)
  }
  ###############################


  #### plots ####
  if (plot_debug) {
    par(mfrow = c(2,2))
    par(mar = c(4, 4, 1.5, 0.5))

    # 11
    plot(I, P, type = "l")
    abline(v = Ith_1st_deriv, col = "blue")
    abline(v = Ith_2nd_deriv, col = "red")
    grid()

    # 12
    plot(I, d1, type = "l")
    abline(h = d1_level_above_thresh, col = "blue")
    abline(v = Ith_1st_deriv, col = "blue")
    grid()

    # 21
    plot(I, d2, type = "l")
    abline(h = 2*d2_rms, col = "red")
    abline(v = Ith_2nd_deriv, col = "red")
    grid()

    # 22
    hist(d1, breaks = 20)
    abline(v = mean(d1))
    grid()
    ################

    # pause for inspection
    print(paste("Ith1d: ", round(Ith_1st_deriv/1e-3, 3), " mA"))
    print(paste("Ith2d: ", round(Ith_2nd_deriv/1e-3, 3), " mA"))
    invisible(readline(prompt = "press [enter] to continue: "))
  }

  return(
    list(Ith_1st_deriv, Ith_2nd_deriv)
  )
}
