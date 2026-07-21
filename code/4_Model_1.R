################################################################################
# 2)  How is maternity leave going to change in future per country?
################################################################################

#cleaning Data (using mandatory leave because of higher data density and especially stronger political significance )
data_pred <- data |>
  select(country, year,
         mat_m_ld_bb,
         mat_m_ld_ab) |>
  mutate(across(where(is.numeric), ~na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~na_if(.x, -99))) |>
  mutate(
    mat_manditory = rowSums(across(c(mat_m_ld_bb,
                                     mat_m_ld_ab)),
                            na.rm = TRUE)
  )

#create prediction via linear regression for the next 50 years (2075)
prediction <- data_pred |>
  group_by(country) |>
  group_modify(~{
    
    model <- lm(mat_manditory ~ year, data = .x)
    future <- data.frame(
      year = 2024:2075
    )
    future$predicted_leave <-
      predict(model,
              newdata = future)
    future })

#visualizing via line plot for all countries given for better comparision
ggplot(prediction,
       aes(x = year,
           y = predicted_leave,
           colour = country,
           group = country)) +
  geom_line() +
  labs(
    title = "Predicted Mandatory Maternity Leave (2025–2035)",
    x = "Year",
    y = "Predicted leave duration (weeks)"
  ) +
  theme_bw()

ggplot(plot_data,
       aes(x = year,
           y = leave,
           colour = country,
           linetype = type,
           group = interaction(country, type))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Historical and Predicted Mandatory Maternity Leave",
    subtitle = "Germany, France, Italy and Finland (1970–2035)",
    x = "Year",
    y = "Mandatory maternity leave (weeks)",
    colour = "Country",
    linetype = ""
  ) +
  theme_bw()

#different idea: plotting each country separately for a better overview of each
ggplot(prediction,
       aes(x = year,
           y = predicted_leave)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ country) +
  labs(
    title = "Predicted Mandatory Maternity Leave by Country",
    x = "Year",
    y = "Predicted leave duration (weeks)"
  ) +
  theme_bw()

#comparing prediction to historical changes
ggplot() +
  geom_line(data = data_pred,
            aes(x = year,
                y = mat_manditory,
                group = country),
            colour = "grey70") +
  
  geom_line(data = prediction,
            aes(x = year,
                y = predicted_leave,
                group = country),
            colour = "red") +
  
  facet_wrap(~ country) +
  
  labs(
    title = "Historical and Predicted Mandatory Maternity Leave",
    x = "Year",
    y = "Weeks"
  ) +
  theme_bw()

#visualizing via line plot for selected countries
selected_countries <- c("DE","FR","IT","FI")

ggplot(
  prediction |>
    filter(country %in% selected_countries),
  aes(x = year,
      y = predicted_leave,
      colour = country)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Mandatory Maternity Leave (2025–2075)",
    x = "Year",
    y = "Predicted leave duration (weeks)",
    colour = "Country"
  ) +
  theme_bw()

#comparing prediction to historical changes for selected countries

selected_countries <- c("DE", "FR", "IT", "FI")

historical <- data_pred |>
  filter(country %in% selected_countries) |>
  transmute(
    country,
    year,
    leave = mat_manditory,
    type = "Historical"
  )

forecast <- prediction |>
  filter(country %in% selected_countries) |>
  transmute(
    country,
    year,
    leave = predicted_leave,
    type = "Forecast"
  )

plot_data <- bind_rows(historical, forecast)

ggplot(plot_data,
       aes(x = year,
           y = leave,
           colour = country,
           linetype = type,
           group = interaction(country, type))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Historical and Predicted Mandatory Maternity Leave",
    subtitle = "Germany, France, Italy and Finland (1970–2035)",
    x = "Year",
    y = "Mandatory maternity leave (weeks)",
    colour = "Country",
    linetype = ""
  ) +
  theme_bw()

#interpretation
###To answer the predictive question, a separate linear regression model was estimated for each country using the historical data from 1970 to 2024. The models were then used to forecast mandatory maternity leave fot the next 50 years,until 2075. The predictions suggest that countries with historically increasing maternity leave policies, such as  Italy, are expected to continue showing a moderate upward trend. Countries with relatively stable policies, such as Germany, are predicted to remain largely unchanged over the next decades. Overall the forecasts indicate that major policy changes are unlikely in short term and that maternity leave duration will therefore continue gradually rather than dramatically. These predictions should be interpreted with caution because they assume the past trends continue into the future. Legislative reforms, poltical decisions or economic developments cannot be anticipated by a simple linear regression model. 