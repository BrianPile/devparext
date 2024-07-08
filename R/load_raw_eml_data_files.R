#' Load many raw EAM data files into a data frame
#'
#' @param paths The file path(s) to folder(s) containing the csv files
#' @param pattern A pattern to match certain file names
#'
#' @return A data frame with all the rows of data combined
#' @export
#' @importFrom rlang .data
#'
#' @examples
load_raw_eml_data_files = function(paths, pattern = "-EML\\.csv") {
  # due to (non standard evaluation) NSE notes in R CMD check related to
  # data.table
  eml_id = NULL

  list_of_files = list.files(path = paths,
                             full.names = TRUE,
                             pattern = pattern)

  df_eml0 = list_of_files |>
    purrr::set_names() |>
    purrr::map(\(x) data.table::fread(x)) |>
    purrr::list_rbind(names_to = "file_name") |>
    dplyr::mutate(test_date_eml = file.info(.data$file_name)$ctime, .before = "file_name")

  df_eml = df_eml0 |>
    dplyr::rename(voltage = .data$`voltage[V]`, power = .data$`power[mW]`, current = .data$`current[mA]`) |>
    dplyr::mutate(eml_id = basename(.data$file_name), .before = "file_name") |>
    dplyr::select(-.data$file_name) |>
    dplyr::mutate(eml_id = stringr::str_replace(.data$eml_id, "-EML.csv", "")) |>
    tidyfast::dt_separate(
      eml_id,
      into = c("waferID", "cellID", "barID", "dieID", "tempC", "If", "testID"),
      sep = "-"
    ) |>
    dplyr::relocate(.data$voltage, .after = "testID") |>
    dplyr::relocate(.data$power, .after = "voltage") |>
    dplyr::relocate(.data$current, .after = "power") |>
    dplyr::relocate(.data$testID, .before = "tempC") |>
    tidyr::unite(
      "SN",
      "waferID":"dieID",
      remove = TRUE,
      sep = "-"
    ) |>
    dplyr::mutate(tempC = as.numeric(stringr::str_replace(.data$tempC, "C", "")),
           If = as.numeric(stringr::str_replace(.data$If, "mA", "")),
           power = .data$power *1e-3,
           current = .data$current * 1e-3,
           If = .data$If * 1e-3) |>
    dplyr::group_by(dplyr::across(!c("voltage", "power", "current"))) |>
    dplyr::mutate(dPdV = my_derivative(.data$voltage, .data$power),
           dVdI = my_derivative(.data$current, .data$voltage)) |>
    dplyr::ungroup()

  return(df_eml)
}
