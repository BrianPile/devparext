#' Create a summary from the raw data
#'
#' @param df_eml a data frame
#'
#' @return a data frame
#' @importFrom rlang .data
#' @export
#'
# @examples
summarize_raw_eml_data = function(df_eml) {
  df_summary_eml = df_eml |>
    dplyr::summarize(
      Pf1ea0 = extract_eml_pf(.data$voltage, .data$power) / 1e-3,
      Veaop = extract_eml_vop(.data$voltage, .data$dPdV),
      Pfop = extract_eml_pf(.data$voltage, .data$power, V0 = .data$Veaop) / 1e-3,
      DCERop = extract_eml_dcer(.data$voltage, .data$power, V0 = 0, V1 = .data$Veaop),
      dPdVmax = extract_eml_dpdv_max(.data$voltage, .data$dPdV),
      Rphoto = extract_eml_rphoto(.data$voltage, .data$dVdI, .data$Veaop)
    )

  return(df_summary_eml)
}
