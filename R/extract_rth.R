extract_rth = function(temps, pow_thermal, peak_wav) {

  linearMod = stats::lm(peak_wav ~ temps + pow_thermal,
                 data = data.frame("temps" = temps,
                                   "pow_thermal" = pow_thermal,
                                   "peak_wav" = peak_wav))

  m_hs = stats::coefficients(summary(linearMod))["temps", "Estimate"]
  m_pow_thermal = stats::coefficients(summary(linearMod))["pow_thermal", "Estimate"]
  b = stats::coefficients(summary(linearMod))["(Intercept)", "Estimate"]
  r2 = summary(linearMod)$r.squared

  Rth = m_pow_thermal/m_hs

  return(
    data.frame(
      "Rth" = Rth,
      "mRthhs" = m_hs,
      "mRthpow_thermal" = m_pow_thermal,
      "b" = b,
      "Rthr2" = r2
    )
  )
}
