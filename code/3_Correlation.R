################################################################################
# 3)  How do duration of maternity leave and amount of financial support correlate?
################################################################################

## data from 2010 onwards --> frist plot: sudden dicrease since 2010

# step 1: check for amount of NAs for the funding variable, and then check values
data_corr <- data_expl |>
  filter(year %in% c(2010:2024)) |>
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

na_check ## Drop fixed rate, to many NAs

# check Correlation for replacement rate scheme --------------------------------

## countries where par3_rr is actually populated
rr_countries <- data_corr |>
  group_by(country) |>
  summarise(n_valid_rr = sum(!is.na(par3_ld) & !is.na(par3_rr))) |>
  filter(n_valid_rr > 0) |>
  pull(country)

rr_countries

## data for replacement-rate countries
data_corr_rr <- data_corr |>
  filter(country %in% rr_countries)

## within country correlation
country_rr <- data_corr_rr |>
  group_by(country) |>
  summarise(
    n_obs    = sum(!is.na(par3_ld) & !is.na(par3_rr)),
    pearson  = cor(par3_ld, par3_rr, method = "pearson", use = "complete.obs"),
    spearman = cor(par3_ld, par3_rr, method = "spearman", use = "complete.obs")
  )
country_rr ## A lot of NAs, as there is no Variance within a country,as the policies are fixed
## strong positive linear correlation for Finnland!
## slight positive correlation, but weak for Poland.
## negative Correlation for Lettland? (LT)
# cross country correlation ----------------------------------------------------
## one row per country: most recent year with valid data
cor_rr_country <- data_corr_rr |>
  filter(!is.na(par3_ld), !is.na(par3_rr)) |>
  group_by(country) |>
  slice_max(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(country, year, par3_ld, par3_rr)

cor_rr_country

cor(cor_rr_country$par3_ld, cor_rr_country$par3_rr, method = "pearson")
cor(cor_rr_country$par3_ld, cor_rr_country$par3_rr, method = "spearman")
## there is a slight positive correlation for both!
## Spearman: 0,24 --> weak positive correlation
## Pearson: 0,27 --> slightly positive, linear relationship

# plot
plot_correlation <- ggplot(cor_rr_country, aes(x = par3_ld, y = par3_rr, label = country)) +
  geom_point(size = 3) +
  geom_text(vjust = 2) +
  labs(
    x = "Leave duration (weeks, highest replacement-rate scheme)",
    y = "Replacement rate (%)",
    title = "Replacement rate vs. leave duration across countries"
  )+
  theme_light()
ggsave("Project_files/plots/correlation.png", plot = plot_correlation,  create.dir = TRUE)

## Model
model_rr <- lm(par3_rr ~ par3_ld, data = cor_rr_country)
summary(model_rr) ## doesn't make sense, variance between countries is to big to fit into one model.

