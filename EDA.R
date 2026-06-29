# Set up
library(ggplot2)
library(tidyverse)

## Key variables
key_vars <- data |>
  select(country, year,
         mat_m_ld_bb, mat_m_ld_ab,
         mat_v_ld_bb, mat_v_ld_ab,
         par1_ld, par1_fr, par1_for_whom,
         par2_ld, par2_fr, par2_for_whom, currency)

summary(key_vars)

## clean data
## new attempt: differentiate between mandatory and voluntary leave


data_expl <- data |>
  select (country, year,
          mat_m_ld_bb, mat_m_ld_ab,
          mat_v_ld_bb, mat_v_ld_ab,
          par1_ld, par1_fr, par1_for_whom,
          par2_ld, par2_fr, par2_for_whom, currency) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -99))) |>
  ## calculate maternity leave
  mutate(
    mat_ld_total = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab, mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
    mat_manditory = rowSums(across(c(mat_m_ld_bb, mat_m_ld_ab)), na.rm = TRUE),
    mat_voluntary = rowSums(across(c(mat_v_ld_bb, mat_v_ld_ab)), na.rm = TRUE),
  )

## check how NAs work
data_expl |> 
  filter(is.na(mat_m_ld_bb) & is.na(mat_m_ld_ab) & is.na(mat_v_ld_bb) & is.na(mat_v_ld_ab)) |>
  select(country, year, mat_ld_total) 
## No case of all NAs 

## mandatory leave for women for 1970 vs 2024
mat_leave_comparison <- data_expl |> 
  filter(year %in% c(1970, 2024))

# Plot 1
ggplot(mat_leave_comparison, aes(x = country, y = mat_manditory))+
  geom_col()+
  facet_wrap (~year)+
  labs(
    x = "Country",
    y = "Mandatory Maternity Leave (weeks)",
    fill = "Year",
    title = "Mandatory Maternity Leave: 1970 vs 2024"
  )

# Plot 2
ggplot(mat_leave_comparison, aes(x = country, y = mat_manditory, fill = factor(year)))+
  geom_col(position = "dodge")+
  labs(
    x = "Country",
    y = "Mandatory Maternity Leave (weeks)",
    fill = "Year",
    title = "Mandatory Maternity Leave: 1970 vs 2024"
  )

## compare type of maternity leave for different countries
data_mat_long <- data_expl |>
    filter(year %in% c(1970, 2024)) |>
    select(country, year, mat_voluntary, mat_manditory) |>
    pivot_longer(
      cols = c(mat_voluntary, mat_manditory),
      names_to = "leave_type",
      values_to = "weeks"
    )

head(data_mat_long)


ggplot(data_mat_long, aes(x = country, y = weeks, fill = leave_type)) +
  geom_col() +
  facet_wrap(~ year) +
  labs(
    x = "Country",
    y = "Maternity Leave (weeks)",
    fill = "Leave Type",
    title = "Maternity Leave by Country"
  ) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 


## comparing maternity leave over the years (for specific countries)
# Germany

mat_ger <- data_expl |> 
  filter(country == "DE")

ggplot (mat_ger, aes(x = year, y = mat_ld_total))+
  geom_line()+
  labs(
    x = "Year",
    y = " Total maternity leave in weeks",
    title = "Maternity leave over the Years"
  )+
  theme_light()

## Ireland
mat_ir <- data_expl |> 
  filter (country == "IE")
ggplot(mat_ir, aes(x = year, y = mat_ld_total))+
  geom_line()+
  labs(
    x = "Year",
    y = " Total maternity leave in weeks",
    title = "Maternity leave over the Years"
  )+
  theme_light()
