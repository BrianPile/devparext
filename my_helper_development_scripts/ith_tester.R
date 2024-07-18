library(tidyverse)

# plots view debug analysis function
graphical_debug_ith = function(I, P, group, ...) {
  print(group)
  # print(group$SN)

  Ith_list = extract_ith_from_pi(I, P, ...)
  print(Ith_list)

  # pause for inspection
  # invisible(readline(prompt = "press [enter] to continue: "))
}

# csv_path = system.file(
#   "extdata", package = "devparext", mustWork = TRUE
# ) |> list.files(
#   full.names = TRUE,
#   pattern = "stelight_noisy_liv_data.csv"
# )

#### load some data ####

# HTOL28 problem data
df_liv = read_csv(file = "./inst/extdata/HTOL28 problem liv/71634_10-23-54-89.csv")

# WA24-0013 liv
df_liv = load_raw_liv_data_files(paths = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Allentown Lab/Work Orders/WA24-0013/LIV_POETA") |>
  select(-test_date_liv)

# walk through the groups and view the ith algorithm internal data
# variables
n1_smooth = 1
n2_smooth = 3
n3_smooth = 3

df_liv |>
  # filter(SN %in% sn_Ith1d_probs) |>
  filter(SN == "24FC00275-T14-NA-NA",
         testID == "00_Vea=RF_OUT_driver_on",
         tempC == 75) |>
  # group_by(SN) |>
  group_by(across(-c(current:dVdI))) |>
  group_walk( ~ graphical_debug_ith(
    .x$current, .x$power, .y,
    n1_smooth, n2_smooth, n3_smooth,
    plot_debug = TRUE))
