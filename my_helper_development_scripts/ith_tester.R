library(tidyverse)

# plots view debug analysis function
graphical_debug_ith = function(I, P, group, ...) {
  print(group)

  Ith_list = extract_ith_from_pi(I, P, ...)
  # print(Ith_list)

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

# # HTOL26? EMLs
# csv_path = "./inst/extdata/stelight_noisy_liv_data.csv"
# df_liv = read_csv(csv_path)

# # HTOL26? EMLs
# csv_path = "./inst/extdata/htol26.csv"
# df_liv = read_csv(csv_path)

# # EEVEE CoC
# df_liv = read_csv(file = "./inst/extdata/ith_problem_data/71636_30-NA-29M-K3.csv")
#
# # HTOL18 sample
# df_liv = read_csv(file = "./inst/extdata/ith_problem_data/HTOL18 71026[10]-21-K-10.csv")
#
# EEVEE CoCs POETA data
df_liv = load_raw_liv_data_files("/Users/brianpile/Dropbox (Personal)/my_R_packages/devparext/inst/extdata/eevee cband coc data/LIV_POETA")

# sivers bar test data
# df_liv = data.table::fread(file = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/Lasers/Almae/C-Band/Phase 2/data/Sivers/orders/PT23-0258 cband phase3/work package 2 - processing batch A/bartest data/P10515/data/P10515_combined_LIV.csv")

# walk through the groups and view the ith algorithm internal data
# variables
n1_smooth = 5
n2_smooth = 5
n3_smooth = 5

df_liv |>
  # filter(SN == "71636_30-52-30-N4") |>
  group_by(SN) |>
  group_walk( ~ graphical_debug_ith(
    .x$current, .x$power, .y,
    n1_smooth, n2_smooth, n3_smooth,
    plot_debug = F))


df_liv |>
  summarize(
    .by = SN,
    Ith1d = extract_ith_from_pi(
      current, power,
      plot_debug = TRUE,
      n1_smooth = 3,
      n2_smooth = 3,
      n3_smooth = 3
    )[[1]]
  )

