library(tidyverse)

# plots view debug analysis function
graphical_debug_kink = function(I, P, group, ...) {
  print(group)

  df_return = extract_kink_from_pi(I, P, ...)
  print(df_return)
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

df_liv |>
  group_by(SN) |>
  group_walk(
    ~ graphical_debug_kink(.x$current, .x$pd_current, .y,
                           Istart = 50e-3, Istop = 450e-3, plot_debug = TRUE)
  )


