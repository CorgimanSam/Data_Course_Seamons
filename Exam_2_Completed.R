# =====================================================
# UNICEF UNDER-5 MORTALITY RATE (U5MR) ANALYSIS
# =====================================================

library(tidyverse)
unicef_data <- read_csv("unicef-u5mr.csv")
head(unicef_data)
glimpse(unicef_data)
# -----------------------------------------
# CONVERT DATA INTO TIDY FORMAT
# -----------------------------------------

unicef_tidy <- unicef_data %>%
  
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  
  mutate(
    Year = as.numeric(str_remove(Year, "^U5MR\\."))
  ) %>%
  
  filter(!is.na(U5MR)) %>%
  
  arrange(CountryName, Year)

# Examine the tidy data
head(unicef_tidy)

# Check tidy data structure
glimpse(unicef_tidy)


# -----------------------------------------
# PLOT EACH COUNTRY'S U5MR OVER TIME
# -----------------------------------------

plot_1 <- ggplot(
  unicef_tidy,
  aes(
    x = Year,
    y = U5MR,
    group = CountryName
  )
) +
  
  geom_line(color = "steelblue", linewidth = 0.4) +
  
  facet_wrap(
    ~CountryName,
    scales = "free_y"
  ) +
  
  labs(
    title = "Under-5 Mortality Rate Over Time",
    subtitle = "UNICEF Data: 1950-2015",
    x = "Year",
    y = "Under-5 Mortality Rate"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(size = 5),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 16)
  )

# Display Plot 1
plot_1


# -----------------------------------------
# SAVE PLOT 1
# -----------------------------------------

ggsave(
  "SEAMONS_Plot_1.png",
  plot = plot_1,
  width = 16,
  height = 45,
  dpi = 300
)


# -----------------------------------------
# CALCULATE MEAN U5MR BY CONTINENT
# -----------------------------------------

continent_means <- unicef_tidy %>%
  
  group_by(Continent, Year) %>%
  
  summarise(
    Mean_U5MR = mean(U5MR, na.rm = TRUE),
    .groups = "drop"
  )

# Examine continent averages
head(continent_means)


# -----------------------------------------
# PLOT MEAN U5MR BY CONTINENT
# -----------------------------------------

plot_2 <- ggplot(
  continent_means,
  aes(
    x = Year,
    y = Mean_U5MR,
    color = Continent,
    group = Continent
  )
) +
  
  geom_line(linewidth = 1) +
  
  labs(
    title = "Mean Under-5 Mortality Rate by Continent",
    subtitle = "UNICEF Data: 1950-2015",
    x = "Year",
    y = "Mean Under-5 Mortality Rate",
    color = "Continent"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )
# -----------------------------------------
# SAVE PLOT 2
# -----------------------------------------

ggsave(
  "SEAMONS_Plot_2.png",
  plot = plot_2,
  width = 12,
  height = 8,
  dpi = 300
)


# -----------------------------------------
# PREPARE DATA FOR THE THREE MODELS
# -----------------------------------------

# Remove rows with missing model variables
model_data <- unicef_tidy %>%
  
  filter(
    !is.na(U5MR),
    !is.na(Year),
    !is.na(Continent)
  ) %>%
  
  mutate(
    Continent = factor(Continent)
  )


# -----------------------------------------
# 10. CREATE THREE U5MR MODELS
# -----------------------------------------

# MODEL 1:
# U5MR explained by Year only

mod1 <- lm(
  U5MR ~ Year,
  data = model_data
)


# MODEL 2:
# U5MR explained by Year and Continent

mod2 <- lm(
  U5MR ~ Year + Continent,
  data = model_data
)


# MODEL 3:
# U5MR explained by Year, Continent,
# and their interaction

mod3 <- lm(
  U5MR ~ Year * Continent,
  data = model_data
)


# View model summaries
summary(mod1)
summary(mod2)
summary(mod3)


# -----------------------------------------
# COMPARE MODEL PERFORMANCE
# -----------------------------------------

# Calculate AIC, adjusted R-squared, and RMSE

model_comparison <- data.frame(
  
  Model = c("mod1", "mod2", "mod3"),
  
  AIC = c(
    AIC(mod1),
    AIC(mod2),
    AIC(mod3)
  ),
  
  Adjusted_R2 = c(
    summary(mod1)$adj.r.squared,
    summary(mod2)$adj.r.squared,
    summary(mod3)$adj.r.squared
  ),
  
  RMSE = c(
    sqrt(mean(residuals(mod1)^2)),
    sqrt(mean(residuals(mod2)^2)),
    sqrt(mean(residuals(mod3)^2))
  )
)

# Display the comparison table
print(model_comparison)


# Compare the models statistically
anova(mod1, mod2, mod3)


# MODEL COMPARISON:
# Lower AIC and RMSE indicate better performance.
# Higher adjusted R-squared indicates better performance.
#
# I think mod3 is the best model because it accounts
# for Year, Continent, and their interaction.
# This allows each continent to have a different
# U5MR trend over time.


# -----------------------------------------
# CREATE MODEL PREDICTIONS
# -----------------------------------------

# Create a prediction dataset containing
# every year and every continent

prediction_data <- expand_grid(
  
  Year = seq(
    min(model_data$Year),
    max(model_data$Year),
    by = 1
  ),
  
  Continent = levels(model_data$Continent)
)

# Predict values using Model 1
prediction_data$mod1 <- predict(
  mod1,
  newdata = prediction_data
)

# Predict values using Model 2
prediction_data$mod2 <- predict(
  mod2,
  newdata = prediction_data
)

# Predict values using Model 3
prediction_data$mod3 <- predict(
  mod3,
  newdata = prediction_data
)


# -----------------------------------------
# CONVERT PREDICTIONS INTO TIDY FORMAT
# -----------------------------------------

prediction_tidy <- prediction_data %>%
  
  pivot_longer(
    cols = c(mod1, mod2, mod3),
    names_to = "Model",
    values_to = "Predicted_U5MR"
  ) %>%
  
  mutate(
    Model = factor(
      Model,
      levels = c("mod1", "mod2", "mod3")
    )
  )

# Examine the predictions
head(prediction_tidy)


# -----------------------------------------
# PLOT THE THREE MODEL PREDICTIONS
# -----------------------------------------

plot_3 <- ggplot(
  prediction_tidy,
  aes(
    x = Year,
    y = Predicted_U5MR,
    color = Continent,
    group = Continent
  )
) +
  
  # Draw a separate line for each continent
  geom_line(linewidth = 1) +
  
  # Put each model in its own panel
  facet_wrap(
    ~Model,
    nrow = 1
  ) +
  
  labs(
    title = "Model predictions",
    x = "Year",
    y = "Predicted U5MR",
    color = "Continent"
  ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "right"
  )

# Display Plot 3
plot_3


# -----------------------------------------
# 15. SAVE PLOT 3
# -----------------------------------------

ggsave(
  "SEAMONS_Plot_3.png",
  plot = plot_3,
  width = 14,
  height = 8,
  dpi = 300
)


# -----------------------------------------
# SAVE THE TIDY DATA AND MODEL RESULTS
# -----------------------------------------

# Save the tidy dataset
write_csv(
  unicef_tidy,
  "UNICEF_U5MR_Tidy.csv"
)

# Save the model comparison table
write_csv(
  model_comparison,
  "UNICEF_Model_Comparison.csv"
)