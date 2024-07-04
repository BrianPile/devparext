extract_kink_from_pi = function(I, P, Istart, Istop) {
  # 2023-05-03 version update

  # check if Istart and Istop are valid
  if (is.na(Istart) | is.na(Istop)) {
    return(
      data.frame(
        "KINK" = NA,
        "Ikink" = NA
      ))
  }

  # Isub = I[between(I, Istart, Istop)]
  Isub = I[I >= Istart & I <= Istop]
  if (pracma::isempty(Isub)) {
    return(
      data.frame(
        "KINK" = NA,
        "Ikink" = NA
      ))
  }

  Isub_squared = Isub^2
  SE = my_derivative(I, P)
  # SEsub = SE[between(I, Istart, Istop)]
  SEsub = SE[I >= Istart & I <= Istop]

  quadraticModel = stats::lm(SEsub ~ Isub + Isub_squared)
  c = stats::coefficients(quadraticModel)["(Intercept)"]
  b = stats::coefficients(quadraticModel)["Isub"]
  a = stats::coefficients(quadraticModel)["Isub_squared"]
  fitPoints = a*Isub^2 + b*Isub + c

  SE_diff = SEsub - fitPoints
  SE_diff_pcnt = 100*(SEsub - fitPoints)/c # this is normalized to y-intercept of quadratic fit
  SE_diff_min = min(SE_diff)
  SE_diff_max = max(SE_diff)

  Ikink = Isub[which.max(abs(SE_diff_pcnt))]
  KINK = max(abs(SE_diff_pcnt)) * sign(SE_diff_pcnt[Isub == Ikink])

  # # plots for debugging (comment this out when not in use)
  # par(mfrow = c(2,2))
  # plot(I /1e-3, P /1e-3,
  #      type = "l",
  #      main = basename(liv_file), cex.main = 0.7,
  #      xlab = "Current (mA)",
  #      ylab = "Power (mW)")
  # grid()
  #
  # if (KINK > 20) {
  #   abline(v = Ikink /1e-3,
  #          lty = 3,
  #          col = "red")
  # }
  #
  # plot(I /1e-3, SE,
  #      type = "l",
  #      # ylim = c(-0.5, 0.65),
  #      xlab = "Current (mA)",
  #      main = basename(liv_file), cex.main = 0.7)
  # grid()
  #
  # lines(Isub /1e-3, SEsub,
  #       col = "red")
  #
  # lines(Isub / 1e-3, fitPoints,
  #       col = "black")
  #
  # abline(v = Ikink /1e-3,
  #        lty = 3)
  #
  # plot(Isub /1e-3, SE_diff,
  #      type = "l")
  # grid()
  #
  # abline(v = Ikink /1e-3,
  #        lty = 3,
  #        col = "red")
  #
  # plot(Isub /1e-3, SE_diff_pcnt,
  #      type = "l",
  #      ylim = c(-100, 100))
  # grid()
  #
  # abline(v = Ikink /1e-3,
  #        lty = 3,
  #        col = "red")
  #
  # abline(h = 20,
  #        lty = 3,
  #        col = "red")
  #
  # abline(h = -20,
  #        lty = 3,
  #        col = "red")
  #
  # par(mfrow = c(1,1))
  # ##

  # print(paste("KINK =", round(KINK), "%"))
  # print(paste("KINK2 =", round(KINK2), "%"))
  # print(paste("Ikink = ", Ikink/1e-3, "mA"))
  #

  # check if Ikink or KINK are empty, assign NA if they are
  if (identical(Ikink, numeric(0))) {
    Ikink = NA
  }
  if (identical(KINK, numeric(0))) {
    KINK = NA
  }

  # return the results in a data frame
  return(
    data.frame(
      "KINK" = KINK,
      "Ikink" = Ikink
    )
  )
}
