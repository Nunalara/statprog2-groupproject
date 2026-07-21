################################################################################
# 2)  How has voluntary maternity leave changed compared to mandatory maternity leave across countries?
################################################################################
#data preparation

leave_data <- data |>
  select(country, year,
         mat_m_ld_bb,
         mat_m_ld_ab,
         mat_v_ld_bb,
         mat_v_ld_ab) |>
  mutate(across(where(is.numeric), ~na_if(.x, -98))) |>
  mutate(across(where(is.numeric), ~na_if(.x, -99))) |>
  mutate(
    mandatory = rowSums(across(c(mat_m_ld_bb,
                                 mat_m_ld_ab)),
                        na.rm = TRUE),
    
    voluntary = rowSums(across(c(mat_v_ld_bb,
                                 mat_v_ld_ab)),
                        na.rm = TRUE)
  )

leave_long <- leave_data |>
  pivot_longer(
    cols = c(mandatory, voluntary),
    names_to = "type",
    values_to = "leave_duration"
  )

#creating a linear model
model_general <- lm(
  leave_duration ~ year * type,
  data = leave_long
)

summary(model_general)

#creating a linear model considering the countries
model_countries <- lm(
  leave_duration ~ year * type + country,
  data = leave_long
)

summary(model_countries)

#visualization across countries
leave_long$predicted <- predict(model_countries)
ggplot(leave_long,
       aes(x = year,
           y = leave_duration,
           colour = type)) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(~country) +
  labs(
    title = "Development of Mandatory and Voluntary Maternity Leave by Country",
    x = "Year",
    y = "Leave duration (weeks)",
    colour = "Leave type"
  ) +
  theme_light()


#interpretation
##summary(model_general): A linear regression model was fitted to examine whether the maternity leave duration changed over time and whether this trend differed between mandatory and voluntary leave. The results revealed a significant positive effect of the year (β = 0.053, p< 0.001), indicating that maternity leave duration increased by approximately 0.05 weeks per year. The effect of leave type was not statistically significant (β=9.07, p=0.826), suggesting that, after accounting for the effect of time, there was no statistically significant difference in average leave duration between mandatory and voluntary maternity leave. In addition, the interaction between year and leave type was not statistically significant (β=-0.003, p=0.883). This indicates that mandatory and voluntary maternity leave followed similiar trends over time, with no evidence that one type changed at a significant different rate than the other. Although the overall regression model was statistically significant (F(3, 2306) = 35.64, p < .001), the explanatory power of the model was relatively low (R² = 0.044). This suggests that year and leave type explain only a small proportion of the variation in maternity leave duration. Other factors, such as country-specific legislation, social policies, or economic conditions, are likely to play a more important role.
##summary(model_countries):A multiple linear regression model was used to examine whether maternity leave duration changed over time, differed between mandatory and voluntary leave, and varied across countries. The results showed a significant positive effect of year (β = 0.053, p < .001), indicating that maternity leave duration increased slightly over the study period. However, neither leave type (p = .819) nor the interaction between year and leave type (p = .878) were statistically significant, suggesting that mandatory and voluntary maternity leave did not differ significantly in their average duration or in their development over time. Several country effects were significant, indicating that maternity leave duration varies between countries. The overall model was statistically significant (F(23, 2286) = 14.38, p < .001) and explained approximately 12.6% of the variance in maternity leave duration (R² = 0.126).
