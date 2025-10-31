#' Extract threshold current from P-I data
#'
#' @param I A numeric vector of currents
#' @param P A numeric vector of powers
#' @param method The threshold current extraction method
#' @param n1_smooth An integer. P-I curve moving average filter length.
#' @param n2_smooth An integer. dP/dI curve moving average filter length.
#' @param n3_smooth An integer. d2P/dI2 curve moving average filter length.
#'
#' @return A numeric value
#' @export
#'
# @examples
# I = example_liv$`current[mA]`
# P = example_liv$`power[mW]`
# Ith = extract_ith_from_pi_old(I, P, method = "second_derivative")
extract_ith_from_pi_old = function(I, P, method = "intercept", n1_smooth = 1, n2_smooth = 1, n3_smooth = 1) {

  # TODO: modify 2nd derivative method so pracma::findpeaks isn't relied on it
  # should be possible to use base R instead.

  # n1_smooth: smooth span for P-I curve smoothing
  # n2_smooth: smooth span for dP/dI curve smoothing
  # n3_smooth: smooth span for d/dI(dP/dI) curve smoothing

  if(method == "intercept") {
    #### intercept ####

    # check if there is enough power to bother extracting Ith
    if(max(P) < 4e-3) {
      Ith = NA
      print("Power too low, skipping Ith extraction.")
      return(Ith)
    }

    # subset the data to exclude currents greater than Isat
    idx_Pmax = which.max(P)
    Isat = I[idx_Pmax]
    Isub = I[I<=Isat]
    Psub = P[I<=Isat]

    P1 = 2e-3
    P2 = 4e-3

    idx1 = which.min(abs(Psub-P1))
    idx2 = which.min(abs(Psub-P2))

    P1 = Psub[idx1]
    P2 = Psub[idx2]

    I1 = Isub[idx1]
    I2 = Isub[idx2]

    slope = (P2-P1) / (I2-I1)
    yintercept = P1-slope*I1
    Ith = -yintercept / slope #this is the x-intercept

    if (Ith < 0 | is.na(Ith)) {
      Ith = NA
    }

  } else if (method == "normalized_intercept") {
    #### normalized intercept ####

    # normalize the power
    P = P/max(P, na.rm = TRUE)

    # subset the data to exclude currents greater than Isat
    idx_Pmax = which.max(P)
    Isat = I[idx_Pmax]
    Isub = I[I<=Isat]
    Psub = P[I<=Isat]

    P1 = 0.10 # 10%
    P2 = 0.15 # 15%

    idx1 = which.min(abs(Psub-P1))
    idx2 = which.min(abs(Psub-P2))

    P1 = Psub[idx1]
    P2 = Psub[idx2]

    I1 = Isub[idx1]
    I2 = Isub[idx2]

    slope = (P2-P1) / (I2-I1)
    yintercept = P1-slope*I1
    Ith = -yintercept/slope #this is the x-intercept

    if (Ith < 0 | is.na(Ith)) {
      Ith = NA
    }

  } else if (method == "normalized_intercept_fit") {
    #### normalized intercept fit ####

    Isat = extract_isat_from_pi(I, P)
    Pmax = max(P)

    if (is.na(Isat)) {
      warning("Problem in extractIth(normalized_intercept_fit. Isat = NA. Returning NA")
      return(NA)
    }

    # condition the L-I curve
    P = P - P[1] # make power start at zero
    P = P/max(P) # normalize to max power of L-I curve
    P = P[I < Isat] # eliminate L-I curve past Isat
    I = I[I < Isat]

    # get the portion of the L-I curve where the power is between P1% and P2% of the
    # maximum value (can tune the percentages, 2 & 10 is ok compromise as of 2022-10-13)
    idx_tofit = P >= 0.05 & P <= 0.15

    if (any(idx_tofit) == FALSE | any(is.na(idx_tofit))) {
      Ith = NA
      return(Ith)
    }

    Ivalues_to_fit = I[idx_tofit]
    Pvalues_to_fit = P[idx_tofit]

    # linear fit
    linearModel = stats::lm(Pvalues_to_fit ~ Ivalues_to_fit)
    if (nrow(stats::coefficients(summary(linearModel))) < 2) {
      warning("Problem in extractIth (normailzed_intercept_fit) fitting. Returning NA.")
      Rth = NA
      return(Rth)
    }

    intercept = stats::coef(summary(linearModel))["(Intercept)", "Estimate"]
    slope = stats::coef(summary(linearModel))["Ivalues_to_fit", "Estimate"]

    # calculate Ith as the x-intercept
    Ith = -intercept/slope

    if (Ith < 0 | is.na(Ith)) {
      Ith = NA
    }

  } else if (method == "second_derivative") {
    #### second_derivative ####

    Pnorm = P/max(P, na.rm = TRUE)
    Isat = extract_isat_from_pi(I, P)

    idx_sub = Pnorm < 0.2 & I < Isat
    Isub = I[idx_sub]
    Psub = P[idx_sub]
    Pnormsub = Pnorm[idx_sub]

    if (pracma::isempty(Isub)) {

      Ith = NA
      print("Ith = NA !!! Isub was empty!!!! csv file was:")
      # print(basename(f))

    } else {

      dLdI = my_derivative(I, my_smooth(Pnorm, N = n1_smooth))
      dLdI2 = my_derivative(I, my_smooth(dLdI, N = n2_smooth))
      dLdI2 = my_smooth(dLdI2, N = n3_smooth)

      dLdI2sub = dLdI2[idx_sub]

      dLdI2sub[is.infinite(dLdI2sub)] = NA
      peak_threshold = 2.0 * sqrt(mean(dLdI2sub^2, na.rm = TRUE))

      # make sure sure input to findpeaks does not contain NAs
      if (any(is.na(dLdI2sub))) {
        warning("extractIth 2ndderiv: input to findpeaks contained NAs!!!")
        return(NA)
      } else {
        pks = pracma::findpeaks(dLdI2sub,
                                minpeakheight = peak_threshold)
      }

      if (is.null(pks)) {
        Ith = NA
        print("Ith 2nd Deriv failed: no dLdI2 pks were found!!!")
      } else {
        idx_max = pks[1, 2]

        # idx_max = which.max(dLdI2) # this works only if the max peak of dLdI2 is the first one, and is threshold
        idx = c(idx_max-1, idx_max, idx_max+1)

        # quadratic fit to the 3-points around dLdI2 peak value
        ypoints = dLdI2sub[idx]
        xpoints = Isub[idx]
        xpoints2 = Isub[idx]^2
        model = stats::lm(ypoints ~ xpoints + xpoints2)

        a = stats::coef(model)["xpoints2"]
        b = stats::coef(model)["xpoints"]
        c = stats::coef(model)["(Intercept)"]

        Ith = -b/2/a
        Ith = unname(Ith)

      }

      # # PLOTS
      # par(mfrow=c(2,2))
      #
      # plot(I, Pnorm, type = "l",
      #      # main = basename(f), cex.main = 0.75,
      #      main = "main title")
      # grid()
      # abline(v = Ith1, lty = "solid", col = "blue")
      # abline(v = Ith2, lty = "solid", col = "red")
      #
      # plot(Isub, Pnormsub, type = "l",
      #      main = basename(f), cex.main = 0.75)
      # grid()
      # abline(v = Ith1, lty = "solid", col = "blue")
      # abline(v = Ith2, lty = "solid", col = "red")
      #
      # plot(I, dLdI2, type = "l",
      #      main = basename(f), cex.main = 0.75)
      # grid()
      # abline(v = Ith2, lty = "solid", col = "red")
      #
      # plot(Isub, dLdI2sub, type = "l",
      #      main = basename(f), cex.main = 0.75)
      # grid()
      # abline(v = Ith2, lty = "solid", col = "red")
      # abline(h = peak_threshold, lty = "solid", col = "red")
      #
      # print("debug breakpoint")
    }

  } else {
    stop("Ith extraction method not valid!!!")
  }

  return(Ith)
}
