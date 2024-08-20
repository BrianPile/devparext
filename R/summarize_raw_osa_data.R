#' Create a summary from optical spectra raw data
#'
#' @param df_osa a data frame containing wavelength and power data
#'
#' @return a data frame containing OSA-extracted parameteric data
#' @export
#'
#' @examples
summarize_raw_osa_data = function(df_osa) {
  df_summary_osa = df_osa |>
    dplyr::summarize(
      Lp = extract_peak_wav(.data$wavelength, .data$power),
      SMSR = extract_smsr(.data$wavelength, .data$power)
    )
}
