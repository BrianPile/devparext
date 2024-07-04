# develop raw liv summarizer function
# Brian Pile
# 2024-06-12
library(tidyverse)

input_data_path = "./inst/extdata/P10514 problematic data/"
df_liv = load_raw_liv_data_files(paths = input_data_path)

df_liv = read_csv(file = "./inst/extdata/P10514 problematic data/71635_30-23-74-82.csv")

If_vec = c(200e-3, 400e-3)
Ix_vec = c(50e-3, 200e-3)
Pop = c(50e-3, 80e-3)

df_summary_liv = df_liv |>
  group_by(across(!c(current:voltage))) |>
  # summarize(n = n()) |>
  summarize_raw_liv_data(If_vec, Ix_vec, Pop, n1_smooth = 3, n2_smooth = 3, n3_smooth = 3, Ik1 = 50, Ik2 = 495) |>
  ungroup()

print(df_summary_liv)
df_summary_liv |> clipr::write_clip()

df_liv |>
  filter(SN == "71635_30-NA-80-E8") |>
  ggplot(aes(x = current, y = power)) +
  geom_path()
