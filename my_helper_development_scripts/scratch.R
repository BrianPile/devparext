
library(tidyverse)

load("/Users/brianpile/POET Technologies Dropbox/Brian Pile/Allentown Lab/Work Orders/WA24-0014/analysis/raw_data.RData")


df_summary_eml = df_eml |>
  group_by(across(-c(voltage:static_ER_dB))) |>
  summarize_raw_eml_data() |>
  ungroup()
