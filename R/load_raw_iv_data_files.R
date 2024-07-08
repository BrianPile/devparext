#' Load many raw IV data files into a data frame
#'
#' @param paths The file path(s) to folder(s) containing the csv files
#' @param pattern A pattern to match certain file names
#'
#' @return A data frame with all the rows of data combined
#' @export
#' @importFrom rlang .data
#'
#' @examples
load_raw_iv_data_files = function(paths, pattern = "-IV\\.csv$") {
  iv_id = NULL # due to NSE notes in R CMD check related to data.table

  # get list of LIV files in the file path
  iv_files = list.files(path = paths,
                        full.names = TRUE,
                        pattern = pattern)

  # load and combine the rows of the iv files
  df_iv = iv_files |>
    purrr::set_names() |>
    purrr::map(\(x) data.table::fread(x)) |>
    purrr::list_rbind(names_to = "file_name") |>
    dplyr::mutate(test_date_iv = file.info(.data$file_name)$ctime, .before = "file_name")

  # massage iv data
  df_iv = df_iv  |>
    dplyr::rename(current = .data$`current[A]`, voltage = .data$`voltage[V]`) |>
    dplyr::mutate(iv_id = basename(.data$file_name), .before = "file_name") |> # create temporary iv_id column
    dplyr::select(-.data$file_name) |> # drop file_name column
    dplyr::mutate(iv_id = stringr::str_replace_all(.data$iv_id, "-IV.csv", "")) |> # delete unwanted parts of iv_id
    # separate (using tidyfast::dt_separate) iv_id into its parts, and drop it
    tidyfast::dt_separate(
      iv_id,
      into = c("waferID", "cellID", "barID", "dieID", "tempC", "facetID", "ARID", "HRID", "testID"),
      sep = "-"
    ) |>
    dplyr::select(-c("facetID":"HRID")) |>
    # unite device info columns to create SN
    tidyr::unite("SN", c("waferID":"dieID"), sep = "-", remove = TRUE) |>
    # move some columns around
    dplyr::relocate(c(.data$voltage, .data$current), .after = "testID") |>
    dplyr::relocate(.data$testID, .before = "tempC") |>
    dplyr::mutate(tempC = as.numeric(stringr::str_replace(.data$tempC, "C", ""))) |>
    tibble::as_tibble()

  return(df_iv)
}
