#' Extract kink deviation from P-I curve
#'
#' @param I a numeric vector of sourced current values
#' @param P a numeric vector of measured output powers
#' @param Istart a numeric value for the beginning of the current span to check
#' @param Istop a numeric value for the end of the current span to check
#' @param n1_smooth an integer defining the moving average smoothing width (should be odd, or will be converted to odd value)
#' @param plot_debug a logical value to enable plot debug mode
#'
#' @return a data frame
#' @export
#'
# @examples
extract_kink_from_pi = function(I, P, Istart, Istop, n1_smooth = 5, plot_debug = FALSE) {

  if (all(is.na(I))) {
    warning("All I values were NA, returning NAs")
    return(data.frame("KINK" = NA, "Ikink" = NA))
  }


  # level1 smoothing
  # n1_smooth = 5
  P = my_smooth(P, n1_smooth)

  # check if Istart and Istop are valid
  if (is.na(Istart) | is.na(Istop)) {
    return(
      data.frame(
        "KINK" = NA,
        "Ikink" = NA
      ))
  }

  # subset the current values
  Isub = I[I >= Istart & I <= Istop]

  # if the subset is empty return NA's
  if (identical(Isub, integer(0))) {
    return(
      data.frame(
        "KINK" = NA,
        "Ikink" = NA
      ))
  }

  # quadratic fitting
  Isub_squared = Isub^2
  SE = my_derivative(I, P)
  SEsub = SE[I >= Istart & I <= Istop]

  quadraticModel = stats::lm(SEsub ~ Isub + Isub_squared)
  c = stats::coefficients(quadraticModel)["(Intercept)"]
  b = stats::coefficients(quadraticModel)["Isub"]
  a = stats::coefficients(quadraticModel)["Isub_squared"]
  fitPoints = a*Isub^2 + b*Isub + c

  # calculate differences between fit and data
  SE_diff = SEsub - fitPoints
  SE_diff_pcnt = 100*(SEsub - fitPoints)/c # this is normalized to y-intercept of quadratic fit
  SE_diff_min = min(SE_diff)
  SE_diff_max = max(SE_diff)

  # calculate kink parameters
  Ikink = Isub[which.max(abs(SE_diff_pcnt))]
  KINK = max(abs(SE_diff_pcnt)) * sign(SE_diff_pcnt[Isub == Ikink])


  # DEBUG PLOTS
  if (plot_debug) {
    par(mfrow = c(2,2))
    par(mar = c(4, 4, 1.5, 0.5))

    # 11
    plot(I /1e-3, P /1e-3,
         type = "l",
         # main = basename(liv_file), cex.main = 0.7,
         xlab = "Current (mA)",
         ylab = "Power (mW)")
    grid()

    if (abs(KINK) > 20) {
      abline(v = Ikink /1e-3, lty = "dotted", col = "red")
    }

    # 12
    plot(I /1e-3, SE,
         type = "l",
         # ylim = c(-0.5, 0.65),
         xlab = "Current (mA)")
    grid()

    graphics::lines(Isub /1e-3, SEsub, col = "red")
    graphics::lines(Isub / 1e-3, fitPoints, col = "green")
    graphics::lines(Isub / 1e-3, fitPoints + 0.2*c, col = "green", lty = "dashed")
    graphics::lines(Isub / 1e-3, fitPoints - 0.2*c, col = "green", lty = "dashed")

    if (abs(KINK) > 20) {
      abline(v = Ikink /1e-3, lty = "dotted", col = "red")
    }

    # 21
    plot(Isub /1e-3, SE_diff,
         type = "l")
    grid()

    abline(v = Ikink /1e-3, lty = "dotted", col = "red")

    # 22
    plot(Isub /1e-3, SE_diff_pcnt, type = "l", ylim = c(-100, 100))
    grid()

    abline(v = Ikink /1e-3, lty = 3, col = "red")
    abline(h = 20, lty = "dashed", col = "green")
    abline(h = -20, lty = "dashed", col = "green")

    par(mfrow = c(1,1))
    ##

    print(paste("KINK =", round(KINK), "%"))
    print(paste("Ikink = ", Ikink/1e-3, "mA"))
    invisible(readline(prompt = "press [enter] to continue: "))
  }
  # end plot debug

  # check if Ikink or KINK are empty, assign NA if they are
  if (identical(Ikink, numeric(0))) {
    Ikink = NA
  }
  if (identical(KINK, numeric(0))) {
    KINK = NA
  }

  # return the results in a data frame
  return(data.frame("KINK" = KINK, "Ikink" = Ikink))
}
