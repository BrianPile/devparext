#' Extract thermal resistance using single-temperature method
#'
#' @param Pth A vector of thermal power dissipation values
#' @param Lp A vector of peak wavelength values
#' @param m_hs A numeric value of the wavelength temperature coefficient (nm/C)
#'
#' @return A data frame with column names Rth2, mRth2Pth, Rth2r2
#' @export
#'
# @examples
extract_rth2 = function(Pth, Lp, m_hs) {

  if (all(is.na(Lp))) {
    Rth = NA
    return(Rth)
  }

  linearMod = stats::lm(Lp ~ Pth, data = data.frame("Pth" = Pth, "Lp" = Lp))

  if (nrow(stats::coefficients(summary(linearMod))) < 2) {
    warning("Problem in extractRth2 fitting. Returning NA.")
    Rth = NA
    return(Rth)
  }

  m_Pth = stats::coefficients(summary(linearMod))["Pth", "Estimate"]
  Rth2 = m_Pth/m_hs
  r2 = summary(linearMod)$r.squared

  return(
    data.frame(
      "Rth2" = Rth2,
      "mRth2Pth" = m_Pth,
      "Rth2r2" = r2
    )
  )

}
