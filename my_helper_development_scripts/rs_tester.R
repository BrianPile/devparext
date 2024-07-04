library(tidyverse)

# # load some data
# # sivers bar test data
# df_liv0 = data.table::fread(file = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/Lasers/Almae/C-Band/Phase 2/data/Sivers/orders/PT23-0258 cband phase3/work package 2 - processing batch A/bartest data/P10515/data/P10515_combined_LIV.csv")
sn_list = sample(unique(df_liv0$SN), 200)
df_liv = df_liv0 |>
  filter(SN %in% sn_list)

graphical_debug_extract_rs = function(I, P, V, group) {

  dVdI = my_derivative(I, my_smooth(V, N = 7))
  Rs = extract_rs_from_vi(I, V, I0 = 400e-3)

  # plots
  x0 = 400e-3
  y0 = extract_vf_from_vi(I, V, I0 = x0)
  b = y0 - Rs*x0

  par(mfrow = c(1, 2))
  par(mar = c(4, 4, 1.5, 0.5))

  plot(I, V, type = "l", ylim = c(0, 3))
  abline(b, Rs, lty = "dotted")
  # plot(x = x0 -0.1/2*max(I))
  grid()

  plot(I, dVdI, type = "l", ylim = c(0, 5))
  abline(v = Rs, lty = "dotted")
  grid()

  # pause for inspection
  print(group)
  print(paste("Rs = ", Rs))
  invisible(readline(prompt = "press [enter] to continue: "))
}

df_liv |>
  # filter(df_liv %in% sn_list) |>
  group_by(SN) |>
  group_walk(
    ~ graphical_debug_extract_rs(.x$current, .x$power, .x$voltage, .y)
  )
