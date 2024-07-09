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
load_raw_liv_data_files = function(paths, pattern = "-LIV\\.csv") {
  liv_id = NULL # due to NSE notes in R CMD check related to data.table


  # get list of LIV files in the file path
  liv_files = list.files(path = paths,
                         full.names = TRUE,
                         pattern = pattern)

  # load and combine the rows of the liv files, then create column containing
  # file creation date
  df_liv0 = liv_files  |>
    purrr::set_names() |>
    purrr::map(\(x) data.table::fread(file = x)) |>
    purrr::list_rbind(names_to = "file_name") |>
    dplyr::mutate(test_date_liv = file.info(.data$file_name)$ctime, .before = "file_name")


  # add a dummy mpd_current column if needed
  if (! "mpd_current[mA]" %in% colnames(df_liv0)) {
    df_liv0["mpd_current[mA]"] = 0
  }

  # massage liv data
  df_liv = df_liv0  |>
    dplyr::mutate(liv_id = basename(.data$file_name), .before = "file_name") |> # create temporary liv_id column
    dplyr::select(-"file_name") |> # drop file_name column
    dplyr::mutate(liv_id = stringr::str_replace_all(.data$liv_id, "-LIV.csv", "")) |> # delete unwanted parts of liv_id
    dplyr::rename(
      current = .data$`current[mA]`,
      power = .data$`power[mW]`,
      voltage = .data$`voltage[V]`,
      mpd_current = .data$`mpd_current[mA]`
    ) |>
    # separate (using tidyfast::dt_separate) liv_id into its parts, and drop it
    tidyfast::dt_separate(
      col = liv_id,
      into = c("waferID", "cellID", "barID", "dieID", "tempC", "facetID", "ARID", "HRID", "testID"),
      sep = "-",
      remove = TRUE
    ) |>
    # unite device info columns to create SN
    tidyr::unite(
      "SN",
      c("waferID":"dieID"),
      sep = "-",
      remove = TRUE
    ) |>
    dplyr::select(-c("facetID":"HRID")) |>
    dplyr::relocate("testID", .before = "tempC") |>
    dplyr::relocate(c("current", "power", "voltage", "mpd_current"), .after = "tempC") |> # move some columns around
    # use SI units
    dplyr::mutate(tempC = as.numeric(stringr::str_replace(.data$tempC, "C", "")),
                  current = .data$current * 1e-3,
                  power = .data$power * 1e-3,
                  mpd_current = .data$mpd_current * 1e-3) |>
    # calculate dL/dI and dV/dI
    dplyr::group_by(dplyr::across(!c("current", "power", "voltage", "mpd_current"))) |>
    dplyr::mutate(dLdI = my_derivative(.data$current, .data$power),
                  dVdI = my_derivative(.data$current, .data$voltage)) |>
    dplyr::ungroup()

  return(df_liv)

}
