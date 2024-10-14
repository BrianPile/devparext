# smsr tester
library(tidyverse)


graphical_smsr_tester = function(wav, power, group_data) {

  print(basename(group_data$fullpath))
  extract_smsr(wav, power, plot_debug = TRUE)

}



input_data_path = "/Users/brianpile/POET Technologies Dropbox/Brian Pile/Allentown Lab/Data/SimonHL - Oct 2024/71U16 PT09C B5#15 gchip25 grating25 HL/OSA_POETA"

osa_files = list.files(
  path = input_data_path,
  full.names = TRUE,
  pattern = "-OSA[.]csv$"
)

df_osa = osa_files |>
  set_names() |>
  map(\(x) data.table::fread(x)) |>
  list_rbind(names_to = "fullpath")

df_osa |>
  group_by(fullpath) |>
  group_walk(~graphical_smsr_tester(.x$`wavelength[nm]`, .x$`power[dBm]`, .y)) |>
  ungroup()

