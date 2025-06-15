# smsr tester
library(tidyverse)
library(here)


input_data_path = here("local-private-data/sivers P11032 problem SMSR")

osa_files = list.files(
  path = input_data_path,
  full.names = TRUE,
  pattern = "-OSA[.]csv$"
)

df_osa = osa_files |>
  set_names() |>
  map(\(x) data.table::fread(x)) |>
  list_rbind(names_to = "fullpath") |>
  as_tibble()

df_osa |>
  filter(If == 400e-3) |>
  group_by(across(-c(wavelength, power))) |>
  summarize(
    SMSR = extract_smsr(wavelength, power, plot_debug = TRUE, plot_title = paste(sep = "-", unique(SN), unique(If)))
  )


