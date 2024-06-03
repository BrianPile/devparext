#' Load many raw LIV data files into a data frame
#'
#' @param paths The file path(s) to folder(s) containing the csv files
#' @param pattern A pattern to match certain file names
#'
#' @return A data frame with all the rows of data combined
#' @export
#' @importFrom rlang .data
#'
#' @examples
load_raw_osa_data_files = function(paths, pattern = "-OSA.csv$") {
  osa_id = NULL # due to NSE notes in R CMD check related to data.table

  osa_file_paths = paths
  osa_files = list.files(path = osa_file_paths,
                         full.names = TRUE,
                         pattern = pattern)

  # load and combine rows of csv files into a data frame, with file name in a new column
  # remove units from wavelength and power columns
  # create osa_test_date column
  df_osa0 = osa_files |>
    purrr::set_names() |>
    purrr::map(\(x) data.table::fread(file = x)) |>
    purrr::list_rbind(names_to = "file_name") |>
    dplyr::rename(wavelength = .data$`wavelength[nm]`, power = .data$`power[dBm]`) |>
    dplyr::mutate(osa_test_date = file.info(.data$file_name)$ctime, .before = "file_name")

  # massage the data into desired formats
  df_osa = df_osa0 |>
    dplyr::mutate(osa_id = basename(.data$file_name), .before = "file_name") |>  # create temporary osa_id column
    dplyr::select(-.data$file_name) |> # drop file_name column
    dplyr::mutate(osa_id = stringr::str_replace_all(.data$osa_id, "-OSA.csv", "")) |>  # delete unwanted parts of osa_id
    # separate (using tidyfast::dt_separate) osa_id into its parts, and drop it
    tidyfast::dt_separate(
      osa_id, into = c("waferID", "cellID", "barID", "dieID", "tempC", "If", "facetID", "ARID", "HRID", "testID"),
      sep = "-",
      remove = TRUE
    ) |>
    # unite device info columns to create SN
    tidyr::unite(
      col = "SN",
      c("waferID":"dieID"), # columns to unite
      sep = "-",
      remove = TRUE
    ) |>
    dplyr::select(-c("facetID":"HRID")) |>
    dplyr::relocate(c(.data$wavelength, .data$power), .after = "testID") |>  # move some columns around
    dplyr::relocate(.data$testID, .before = "tempC") |>
    dplyr::mutate(If = as.numeric(stringr::str_replace(.data$If, "mA", "")) *1e-3, # format some columns
           tempC = as.numeric(stringr::str_replace(.data$tempC, "C", ""))) |>
    tibble::as_tibble()

  return(df_osa)


}
