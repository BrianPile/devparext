#' Extract peak power from optical spectrum
#'
#' @param power A numeric vector of spectrum power
#'
#' @returns A numeric value
#' @export
#'
# @examples
extract_peak_power = function(power) {
  peak_power = max(power)
  return(peak_power)
}
