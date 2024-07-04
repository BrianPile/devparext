#' Extract the signal bandwidth, at 20dB down, from spectrum data
#'
#' @param wav A numeric vector of spectrum wavelength data
#' @param power A numeric vector of power spectrum data
#'
#' @return A numeric value, or NA if the calculation failed
#' @export
#'
#' @examples
#' wav = seq(1300, 1320, by = 0.1)
#' pow = rep(-70, length(wav)) + rnorm(length(wav), 0, 1)
#' pow[101:103] = c(-30, -5, -30)
#' pow[50:52] = c(-40, -38, -40)
#' bandwidth = extract_bw20dB(wav, pow)
#' print(bandwidth)
extract_bw20dB = function(wav, power) {

  if (is.na(extract_smsr(wav, power))) {
    return(NA)
  }

  if (extract_smsr(wav, power) < 20) {
    # warning("BW20dB: SMSR was less than 20dB. Returning NA.")
    return(NA)
  }

  p_norm = power - max(power) # normalize the spectrum
  Lp = wav[which.max(p_norm)] # peak wavelength

  # 1. find right-side -20dB wavelength
  p_right = p_norm[wav >= Lp]
  wav_right = wav[wav >= Lp]
  idx_right = which.min(abs(p_right + 20)) # get index nearest to the right-side -20dB point

  if ( p_right[idx_right]+20 <= 0 ) {
    # print("nearest right-point is LESS than -20dB")
    idx = c(idx_right, idx_right-1)
    L20dB_right = stats::approx(p_right[idx], wav_right[idx], -20)$y
  } else if ( p_right[idx_right]+20 > 0 ) {
    # print("nearest right-point is GREATER than -20dB")
    idx = c(idx_right, idx_right+1)
    L20dB_right = stats::approx(p_right[idx], wav_right[idx], -20)$y
  } else {
    stop("Stop1: we should not be here!!!")
  }

  # 2. find left-side -20dB wavelength
  p_left = p_norm[wav <= Lp]
  wav_left = wav[wav <= Lp]
  idx_left = which.min(abs(p_left + 20))

  if ( p_left[idx_left]+20 <= 0 ) {
    # print("nearest left-point is LESS than -20dB")
    idx = c(idx_left, idx_left+1)
    L20dB_left = stats::approx(p_left[idx], wav_left[idx], -20)$y
  } else if ( p_left[idx_left]+20 > 0 ) {
    # print("nearest right-point is GREATER than -20dB")
    idx = c(idx_left-1, idx_left)
    L20dB_left = stats::approx(p_left[idx], wav_left[idx], -20)$y
  } else {
    stop("Stop2: we should not be here!!!")
  }

  BW_20dB = L20dB_right - L20dB_left # calculate the -20dB bandwidth result

  return(BW_20dB)
}
