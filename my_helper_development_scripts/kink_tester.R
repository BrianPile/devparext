library(tidyverse)

# plots view debug analysis function
graphical_debug_kink = function(I, P, group, ...) {
  print(group)

  df_return = extract_kink_from_pi(I, P, ...)
  print(df_return)
  # pause for inspection
  # invisible(readline(prompt = "press [enter] to continue: "))
}

#### load some data ####

# # HTOL28 problem data
# df_liv = read_csv(file = "./inst/extdata/HTOL28 problem liv/71634_10-23-54-89.csv")

# WA22-0019 GIBLE baseline CoCs
df_liv = load_raw_liv_data_files(paths = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Allentown Lab/Work Orders/2022/WA22-0019/LIV_POETA")

sn_list = sample(unique(df_liv$SN), 20)

df_liv |>
  filter(SN %in% sn_list) |>
  group_by(SN, tempC, testID) |>
  group_walk(
    ~ graphical_debug_kink(.x$current, .x$power, .y,
                           Istart = 50e-3, Istop = 495e-3, plot_debug = TRUE)
  )
