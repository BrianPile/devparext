
df_liv = readr::read_csv(file = "./inst/extdata/ith_problem_data/HTOL18 71026[10]-21-K-10.csv")


df_liv |>
  dplyr::summarize(Ith = extract_ith_from_pi(current, pd_current, method = "second_derivative", n2_smooth = 9, n3_smooth = 9))

