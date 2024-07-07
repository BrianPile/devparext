library(tidyverse)

# plots view debug analysis function
graphical_debug_ith = function(I, P, group, ...) {
  print(group)

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

# walk through the groups and view the ith algorithm internal data
# variables
n1_smooth = 2
n2_smooth = 9
n3_smooth = 9

df_liv |>
  # filter(SN %in% sn_Ith1d_probs) |>
  # filter(SN == "71636_30-41-17-H6") |>
  group_by(SN) |>
  group_walk( ~ graphical_debug_ith(
    .x$current, .x$pd_current, .y,
    n1_smooth, n2_smooth, n3_smooth,
    plot_debug = TRUE))


