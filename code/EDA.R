########################## questions to ask ####################################
# 1 )How has maternity leave changed over the years depending on countries?
##  How does maternity leave change depending on the country,
##  comparing types of maternity leave and aid ?
################################################################################

# Set up
library(ggplot2)
library(tidyverse)
library(forcats)

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

#-------------------------------------------------------------------------------
## Maternity leave for women 1970 vs 2024

mat_leave_comparison <- data_expl |> 
  filter(year %in% c(1970, 2024))

## Total leave
ggplot(mat_leave_comparison, aes(x = country, y = mat_ld_total, fill = factor(year)))+
  geom_col(position = "dodge")+
  labs(
    x = "Country",
    y = "Leave duration measured in weeks",
    fill = "Year",
    title = "Mandatory Maternity Leave: 1970 vs 2024"
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
    title = "Mandatory Maternity Leave: 1970 vs 2024"
  )+
  theme_bw()


## new idea. only show the calculated differences between the years, with bar for each type of maternity leave



diff_data <- data_expl |>
  filter(year %in% c(1970, 2024)) |>
  select(country, year, mat_manditory, mat_voluntary) |>
  pivot_longer(
    cols = c(mat_manditory, mat_voluntary),
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
                  mat_manditory = "Mandatory",
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

ggplot(diff_data, aes(y = country, x = diff, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Mandatory" = "darkred", "Voluntary" = "grey" )) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  labs(
    title = "Change in Maternity Leave, 1970 vs. 2024",
    y = NULL,
    x = "Total difference in leave weeks (2024 − 1970)",
    fill = "Leave type"
  ) +
  theme_bw()


## We decided to choose 3 countries for analysis, one where there has been no signficant change (Germany),
## A country where there has been a significant change, negative (France) and positive (Italy)
## and one country where there was no mandatory maternity leave in 1970 (Finnland or UK)
#-------------------------------------------------------------------------------


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
 

################################################################################
# 3)  How do duration of maternity leave and amount of financial support correlate?
################################################################################

## trial: data from 2000 onwards

# step 1: check for amount of NAs for the funding variable, and then check values
data_corr <- data |>
  filter(year %in% c(2000:2024)) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~ na_if(.x, -99))) 

# Step 1a: check NA counts per country for par2_ld and par2_fr
na_check <- data_corr |>
  group_by(country) |>
  summarise(
    n_total = n(),
    na_ld2 = sum(is.na(par2_ld)),
    na_fr2 = sum(is.na(par2_fr)),
    na_ld3 = sum(is.na(par3_ld)),
    na_rr3 = sum(is.na(par3_rr))
  )

na_check ## only check the flat rate for AT; BE, EE FR
         ## check only replacement rate: CZ, DE, ES, GR

## data for fixed rate
data_corr_fr <- data_corr |>
  filter(country %in% c("AT","BE","EE","FR"))

## data for replacement rate
data_corr_rr <- data_corr |>
  filter (country %in% c("DK","FI","EE", "DE"))

## check for values throughout the years
#duration for fixed rate
ggplot(data_corr_fr, aes(x = year, y = par2_ld, color = country))+
  geom_line()+
  labs(title = "Duration of leave from 2000 to 2024 when receiving a fixed rate",
       x = "Year",
       y = "Weeks")
# fixed rate
ggplot(data_corr_fr, aes (x = year, y = par2_fr, color = country))+
  geom_line()+
  labs(title = "Fixed rate 2000 to 2024",
       x = "Year",
       y = "Price")

#duration replacement rate
ggplot(data_corr_rr, aes(x = year, y = par3_ld, color = country))+
  geom_line()+
  labs(title = "Duration of leave from 2000 to 2024 when receiving a replacement rate",
       x = "Year",
       y = "Weeks")

ggplot(data_corr_rr, aes(x = year, y = par3_rr, color = country))+
  geom_line()+
  labs(title = "Replacement rate from 2000 to 2024",
       x = "Year",
       y = "Rate")
## clear confusion: plot missing for DE?
data_corr_rr |>
  filter(country == "DE") |>
  select(year, par3_ld, par3_rr) |>
  print(n = 25)
## Nope, just overlap

# Correlation check ------------------------------------------------------------
cor_fr <- data_corr_fr |>
  group_by(country) |>
  summarise(
    n_obs    = sum(!is.na(par2_ld) & !is.na(par2_fr)),
    pearson  = cor(par2_ld, par2_fr, method = "pearson", use = "complete.obs"),
    spearman = cor(par2_ld, par2_fr, method = "spearman", use = "complete.obs")
  )
cor_fr

cor_rr <- data_corr_rr |>
  group_by(country) |>
  summarise(
    n_obs    = sum(!is.na(par3_ld) & !is.na(par3_rr)),
    pearson  = cor(par3_ld, par3_rr, method = "pearson", use = "complete.obs"),
    spearman = cor(par3_ld, par3_rr, method = "spearman", use = "complete.obs")
  )

cor_rr
