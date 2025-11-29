#' Mode hop detection analysis of peak wavelength vs bias current data
#'
#' @param If Numeric vector of bias currents
#' @param Lp Numeric vector of peak wavelengths
#' @param plot_debug Logical flag to enable visual inspection of the analysis
#' @param plot_title String for the debug plot title
#'
#' @returns A data frame with analysis metrics
#' @export
#'
# @examples
extract_mode_hop = function(
    If,
    Lp,
    plot_debug = FALSE,
    plot_title = "default_title"
) {

  # calculations ----

  # rename x-y data variables
  x = If    # bias current
  y = Lp    # peak wavelength

  # quadratic fitting
  quadratic_model = stats::lm(y ~ x + I(x^2))
  y_fit = stats::predict(quadratic_model, data.frame(x = x))
  coefs = stats::coefficients(quadratic_model) # coefficients
  # y = ax^2 + bx + c
  a = coefs[[3]]
  b = coefs[[2]]
  c = coefs[[1]]

  r_squared = summary(quadratic_model)$r.squared # r-squared
  resids = stats::residuals(quadratic_model) # get residuals
  max_resid = max(abs(resids))

  # quality-of-fit metric
  y_range = max(y) - min(y)
  resids_norm_pct = 100 * resids / y_range
  max_resid_norm_pct = max(abs(resids_norm_pct))

  # find mode-hop current if max_resid_norm_pct > 4%. Find the two largest resids_norm and average them
  mh_current = NA_real_
  idx = order(abs(resids), decreasing = TRUE)
  if (length(idx) >= 2) {
    largest_two_positions = idx[1:2]
    if (max_resid_norm_pct > 4) {
      mh_current = mean(x[largest_two_positions])
    }
  }


  # debug plots ----
  main_cex = 0.9

  if (plot_debug) {
    # config plot layout
    par(
      mfrow = c(2,2),
      oma = c(0, 0, 3, 0),         # outer margin
      mar = c(5, 4, 2, 1) + 0.1    # margin for each plot
    )

    # subplot 11
    plot(x, y, type = "l", main = "Peak WL vs If", cex.main = main_cex)
    graphics::points(x, y)
    graphics::lines(x, y_fit, col = "magenta", lwd = 2, lty = 2)
    # lines(x, a*x^2 + b*x + c, col = "blue", lwd = 4)

    if (max_resid_norm_pct > 4) {
      abline(v = mh_current)
    }

    grid()

    usr = par("usr")
    text11 = stringr::str_glue(
      "y={round(a, 2)}x^2+{round(b,2)}x+{round(c,2)}\n",
      "r2={round(r_squared, 5)}\n",
      "y_range={round(y_range, 2)}"
    )
    graphics::text(
      x = usr[1]+0.05*(usr[2]-usr[1]),
      y = usr[4]-0.05*(usr[4]-usr[3]),
      labels = text11,
      adj = c(0, 1),
      cex = 0.9
    )


    # subplot 12
    plt_yrange = max(resids) - min(resids)
    plot(
      x, resids, type = "l",
      ylim = c(min(resids) - 0.04*plt_yrange, max(resids) + 0.2*plt_yrange),
      main = "residuals",
      cex.main = main_cex
    )
    abline(h = 0, lty = 1, lwd = 2)
    # points(largest_two_positions, resids[largest_two_positions], col = "magenta")
    graphics::points(x[largest_two_positions], resids[largest_two_positions], pch = 16, col = "black")
    grid()

    # annotation 12
    usr = par("usr")
    # text1 = paste0("max |resid|: ", round(max_resid, 4))
    text12 = stringr::str_glue(
      "max |resids|={round(max_resid, 4)}"
    )
    graphics::text(
      x = usr[1] + 0.05*(usr[2] - usr[1]),
      y = usr[4] - 0.05*(usr[4] - usr[3]),
      labels = text12,
      adj = c(0, 1),
      cex = 0.9
    )

    # subplot 21
    plt_yrange = max(resids_norm_pct) - min(resids_norm_pct)
    plot(
      x, resids_norm_pct, type = "l",
      ylim = c(min(resids_norm_pct) - 0.04*plt_yrange, max(resids_norm_pct) + 0.2*plt_yrange),
      ylab = "resids_norm_pct (%)",
      main = "100 x residuals/y_range",
      cex.main = main_cex
    )
    abline(h = 0, lty = 1, lwd = 2)
    graphics::points(x[largest_two_positions], resids_norm_pct[largest_two_positions], pch = 16, col = "black")
    grid()

    # annotation 21
    usr = par("usr")
    # text1 = paste0("max |resid|: ", round(max_resid, 4))
    text21 = stringr::str_glue(
      "max |resids_norm_pct|={round(max_resid_norm_pct, 4)}%"
    )
    graphics::text(
      x = usr[1] + 0.05*(usr[2] - usr[1]),
      y = usr[4] - 0.05*(usr[4] - usr[3]),
      labels = text21,
      adj = c(0, 1),
      cex = 0.9
    )

    # Add common super title with DUT information
    super_cex = 1
    graphics::mtext(plot_title, outer = TRUE, cex = super_cex, line = 1)

    par(mfrow = c(1,1)) # reset layout
    invisible(readline(prompt = "press [enter] to continue: "))
  }

  # return results ----
  return(
    tibble::tibble(
      r2 = r_squared,
      max_resid = max_resid,
      max_resid_norm_pct = max_resid_norm_pct,
      mh_current = mh_current
    )
  )

}
