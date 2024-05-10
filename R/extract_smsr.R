#' Extract side mode suppresion ratio from spectrum data
#'
#' @param wav A vector of spectrum wavelength data
#' @param power A vector of spectrum power data
#' @param smooth_fraction A parameter for smoothing the power data.
#' smooth_fraction = smooth_points/total_points. smooth_fraction > 0.003 is not
#' recommended.
#'
#' @return A numeric value
#' @export
#'
#' @examples
#' wav = seq(1300, 1320, by = 0.1)
#' pow = rep(-70, length(wav)) + rnorm(length(wav), 0, 1)
#' pow[101:103] = c(-30, -10, -30)
#' pow[50:52] = c(-40, -38, -40)
#' smsr = extract_smsr(wav, pow)
#' print(smsr)
extract_smsr = function(wav, power, smooth_fraction = 0) {
  # 2024-01-23 Added smooth_fraction parameter with default value 0. If
  # smooth_fraction is not equal to zero, the power data is smoothed by a number
  # of points equal to the total data points multiplied by smooth_fraction,
  # rounded down.

  if (smooth_fraction > 0.003) {
    warning("extract_smsr() paramter value: smooth_fraction > 0.003 is not reccomended by Brian!!!")
  }

  if (smooth_fraction != 0) {
    npoints = length(wav)
    npoints_smooth = round(npoints * smooth_fraction)
    power = my_smooth(power, npoints_smooth)
  }

  pks = pracma::findpeaks(power,
                  npeaks = 2,
                  nups = 2,
                  ndowns = 2,
                  zero = "+",
                  minpeakdistance = 2,
                  sortstr = TRUE)

  # check that at least two peaks were found
  if (is.null(pks)) {
    return(NA)
  }
  if (nrow(pks) < 2) {
    return(NA)
  }

  SMSR = pks[1, 1] - pks[2, 1]
}
