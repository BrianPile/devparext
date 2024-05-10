#' Extract side mode suppression ratio from spectrum data using masking method
#'
#' @param wav A numeric vector of spectrum wavelength data
#' @param power A numeric vector of spectrum power data
#' @param mask_width A numeric value, in nanometers (nm), defining the mask width
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' wav = seq(1300, 1320, by = 0.1)
#' pow = rep(-70, length(wav))
#' pow[wav == 1310] = -10
#' pow[wav == 1305] = -45
#' smsr = extract_smsr2(wav, pow, mask_width = 2.0)
#' print(smsr)
extract_smsr2 = function(wav, power, mask_width = 2.0) {
  # 2024-01-23 New SMSR extraction method using the masking technique.
  peak_wav = extract_peak_wav(wav, power)            # get peak wavelength
  idx = abs((wav - peak_wav)) >= mask_width/2 # get indices of data outside the mask range
  max_power1 = max(power)               # get highest peak power
  max_power2 = max(power[idx])          # get second highest peak power, by masking of main peak
  SMSR = max_power1 - max_power2        # calculate SMSR
  return(SMSR)
}
