# Rth tester



# setup ----
library(tidyverse)
library(here)
# library(devparext)
theme_set(theme_bw())


# load data ----
liv_files = list.files(
  path = '/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/Documents/work orders singapore/WO26-0119 PT11B psuedo quad pup/data/raw/LIV (IS) (Post wirebond)',
  full.names = TRUE,
  pattern = "_LIV[.]csv$"
)

df_liv = liv_files |>
  set_names() |>
  map(\(x) loadr::load_liv(x)) |>
  list_rbind() |>
  as_tibble()

osa_files = list.files(
  path = '/Users/brianpile/POET Technologies Dropbox/Brian Pile/Brian Pile/Documents/work orders singapore/WO26-0119 PT11B psuedo quad pup/data/raw/OSA (Post wirebond)',
  full.names = TRUE,
  pattern = "_OSA[.]csv$"
)

df_osa = osa_files |>
  set_names() |>
  map(\(x) loadr::load_osa(x)) |>
  list_rbind() |>
  as_tibble()


# summarization ----
df_summary_liv = df_liv |>
  group_by(across(-c(current:mpd_current))) |>
  summarize(
    Pf400 = extract_pf_from_pi(current, power, I0 = 400e-3) / 1e-3,
    Vf400 = extract_vf_from_vi(current, voltage, I0 = 400e-3),
    .groups = "drop"
  )

df_summary_osa = df_osa |>
  group_by(across(-c(wavelength, power))) |>
  summarize(
    .groups = "drop",
    Lp = extract_peak_wav(wavelength, power),
    SMSR = extract_smsr(wavelength, power, plot_debug = FALSE, plot_title = unique(paste(sep = "-", fc_id, ch, temperature, If/1e-3))),
    BW20dB = extract_full_width(wavelength, power, threshold = -20, plot_debug = FALSE, plot_title = unique(paste(sep = "-", fc_id, ch, temperature, If/1e-3)))
  )

df_rth_input_data = df_liv |>
  group_by(across(-c(current, power, voltage, mpd_current))) |>
  reframe(
    If = df_osa$If |> unique() |> sort(),
    Pf = map_dbl(If, \(x) extract_pf_from_pi(current, power, I0 = x)),
    Vf = map_dbl(If, \(x) extract_vf_from_vi(current, voltage, I0 = x)),
    Pth = If*Vf - Pf
  ) |>
  left_join(df_summary_osa) |>
  print(n = 20)

df_Rth = df_rth_input_data |>
  filter(
    fc_id == "26FC01482",
    ch == "1"
  ) |>
  # group_by(across(-c(temperature, If:BW20dB))) |>
  summarize(
    df_return = extract_rth(temperature, Pth, Lp),
    .groups = "drop"
  )
