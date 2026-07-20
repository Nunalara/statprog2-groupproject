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