#### Explorative Data Analysis #################################################

## First Analysis: Maternity rules in general ----------------------------------
## Change in average maternity length
plot_avg_mat <- data_expl |>
  group_by(year) |>
  summarise(mean_mat_ld_total = mean(mat_ld_total, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = mean_mat_ld_total)) +
  geom_line() +
  labs(
    title = "Average maternity leave duration over time",
    x = "Year",
    y = "Average total maternity leave duration"
  )+
  theme_light()

#plot_avg_mat
ggsave("Project_files/plots/avg_maternity.png", plot = plot_avg_mat,  create.dir = TRUE)

## Plot: voluntary vs. mandatory------------------------------------------------
leave_long <- data_expl |>
  pivot_longer(
    cols = c(mat_mandatory, mat_voluntary),
    names_to = "type",
    values_to = "leave_duration"
  ) |> 
  mutate(type = recode(type,
                       mat_mandatory = "Mandatory",
                       mat_voluntary = "Voluntary"))

plot_mat_vol <- ggplot(leave_long,aes(year, leave_duration, colour = type)) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1.2
  ) +
  scale_color_manual(values = mat_type)+
  labs(
    title = "Average Mandatory and Voluntary Maternity Leave",
    x = "Year",
    y = "Leave duration (weeks)",
    colour = "Leave type"
  ) +
  theme_light()
ggsave("Project_files/plots/mat_vol.png", plot = plot_mat_vol, create.dir = TRUE)

## Plotting the offered lengths of maternity leave -----------------------------
plot_mat_length <- ggplot(data_expl, aes(x = mat_ld_total)) +
  geom_histogram() +
  labs(
    title = "Distribution of total maternity leave duration",
    x = "Total maternity leave duration in weeks",
    y = "Count"
  )+
  theme_light()

plot_mat_length
ggsave("Project_files/plots/maternity_length.png", plot = plot_mat_length,  create.dir = TRUE)

## Maternity leave for women 1970 vs 2024 --------------------------------------

mat_leave_comparison <- data_expl |>
  filter(year %in% c(1970, 2024))

## Total leave
ggplot(mat_leave_comparison, aes(x = country, y = mat_ld_total, fill = factor(year)))+
  geom_col(position = "dodge")+
  labs(
    x = "Country",
    y = "Leave duration measured in weeks",
    fill = "Year",
    title = "Total Maternity Leave: 1970 vs 2024"
  )+
  theme_bw()

## mandatory leave
ggplot(mat_leave_comparison, aes(x = country, y = mat_manditory, fill = factor(year)))+
  geom_col(position = "dodge")+
  labs(
    x = "Country",
    y = "Leave duration measured in weeks",
    fill = "Year",
    title = "Mandatory Maternity Leave: 1970 vs 2024"
  )+
  theme_bw()

## voluntary leave
ggplot(mat_leave_comparison, aes(x = country, y = mat_voluntary, fill = factor(year)))+
  geom_col(position = "dodge")+
  labs(
    x = "Country",
    y = "Leave duration measured in weeks",
    fill = "Year",
    title = "Voluntary Maternity Leave: 1970 vs 2024"
  )+
  theme_bw()


## new idea. only show the calculated differences between the years, with bar for each type of maternity leave
diff_data <- data_expl |>
  filter(year %in% c(1970, 2024)) |>
  select(country, year, mat_mandatory, mat_voluntary) |>
  pivot_longer(
    cols = c(mat_mandatory, mat_voluntary),
    names_to = "type",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = year,
    values_from = value,
    names_prefix = "y"
  ) |>
  mutate(
    diff = y2024 - y1970,
    type = recode(type,
                  mat_mandatory = "Mandatory",
                  mat_voluntary = "Voluntary")
  ) |>
  filter(!is.na(diff))

# order countries by total change (mandatory + voluntary) for a nicer axis
country_order <- diff_data |>
  group_by(country) |>
  summarise(total = sum(diff, na.rm = TRUE)) |>
  arrange(total) |>
  pull(country)

diff_data <- diff_data |>
  mutate(country = factor(country, levels = country_order))

plot_comparison <- ggplot(diff_data, aes(y = country, x = diff, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = mat_type) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  labs(
    title = "Change in Maternity Leave, 1970 vs. 2024",
    y = NULL,
    x = "Total difference in leave weeks (2024 − 1970)",
    fill = "Leave type"
  ) +
  theme_light()

ggsave("Project_files/plots/comparison.png", plot = plot_comparison,  create.dir = TRUE)
