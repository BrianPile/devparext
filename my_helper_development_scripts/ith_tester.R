library(tidyverse)
library(ggpubr)

# variables
n1_smooth = 1
n2_smooth = 9
n3_smooth = 7

csv_path = system.file("extdata", package = "devparext", mustWork = TRUE) |> list.files(full.names = TRUE,
                                                                                        pattern = "stelight_noisy_liv_data.csv")
df_liv = read_csv(csv_path)

df_liv = df_liv |>
  group_by(SN, `Burn In`) |>
  mutate(dLdI = my_derivative(current, my_smooth(pd_current, N = n1_smooth)),
         dLdI2 = my_derivative(current, my_smooth(dLdI, n2_smooth)))

# parameter summary
df_summary_liv = df_liv |>
  group_by(SN, `Burn In`) |>
  summarize(
    Ithlin = extract_ith_from_pi(current, pd_current, method = "normalized_intercept_fit"),
    Ith2d = extract_ith_from_pi(current, pd_current,
                                method = "second_derivative",
                                n1_smooth = n1_smooth,
                                n2_smooth = n2_smooth,
                                n3_smooth = n3_smooth)
  ) |>
  ungroup()

# calculate change in parameters after burn in
df_summary_liv = df_summary_liv |>
  mutate(.by = SN,
         dIthlin_pct = 100*(Ithlin / first(Ithlin, order_by = `Burn In`) - 1),
         dIth2d_pct = 100*(Ith2d / first(Ith2d, order_by = `Burn In`) - 1))

# plot p-i curves with vlines at Ith
df_liv |>
  ggplot(aes(x = current/1e-3, y = pd_current/1e-6, color = as.factor(`Burn In`))) +
  geom_path() +
  geom_vline(
    data = df_summary_liv,
    mapping = aes(xintercept = Ithlin/1e-3, color = as.factor(`Burn In`))
  ) +
  coord_cartesian(xlim = c(5, 10), ylim = c(0, 10)) +
  facet_wrap(~ SN) +
  labs(x = "Current (mA)",
       y = "PD Current (uA)",
       color = "Burn In (Hr)")

# plot change in Ith
p1 = df_summary_liv |>
  ggplot(aes(x = `Burn In`, y = dIthlin_pct, group = SN)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(-10, 10), color = "red") +
  labs(title = "Ith method=normalized_intercept_fit")

p2 = df_summary_liv |>
  ggplot(aes(x = `Burn In`, y = dIth2d_pct, group = SN)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(-10, 10), color = "red") +
  labs(title = "Ith method=second_derivative, dldi smooth")

ggarrange(p1, p2)


# #
# df_liv |>
#   ggplot(aes(x = current/1e-3, y = my_smooth(dLdI2, N = n3_smooth), color = as.factor(`Burn In`))) +
#   geom_path() +
#   coord_cartesian(xlim = c(0, 20), ylim = c(0, 1.5)) +
#   facet_wrap(~ SN)


# # compare Ith methods
# df_summary_liv |>
#   pivot_longer(cols = c(Ithlin, Ith2d),
#                names_to = "method",
#                values_to = "Ith") |>
#   # filter(`Burn In` == 0) |>
#   ggplot(aes(x = SN, y = Ith /1e-3, fill = method))+
#   geom_col(position = "dodge") +
#   facet_wrap(~ `Burn In`)
