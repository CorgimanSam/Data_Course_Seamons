library(tidyverse)
# ------------------------------------------------------------
# Question 1) Loading the data
# ------------------------------------------------------------
Cleaned_Covid_19_data <- read.csv("cleaned_covid_data.csv")

# ------------------------------------------------------------
# Question 2) Subset the data
# ------------------------------------------------------------

# Keep only states whose names begin with "A"
# and save the subset as A_states
A_states <- Cleaned_Covid_19_data %>%
  filter(str_starts(Cleaned_Covid_19_data$Province_State, "A"))

# Checking the subset
print(A_states)
# Question 3) Scatterplot creation
deaths_plot <- ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(
    title = "COVID-19 Deaths Over Time for States Starting with 'A'",
    x = "Date",
    y = "Deaths"
  ) +
  theme_minimal()

print(deaths_plot)

# Question 4; finding Peak Case_Fatality_Ratio per state
state_max_fatality_rate <- Cleaned_Covid_19_data %>%
  group_by(Province_State) %>%
  summarize(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(Maximum_Fatality_Ratio)) %>%
  arrange(desc(Maximum_Fatality_Ratio))
print(state_max_fatality_rate, n=51)
# Task V: Bar plot of Maximum_Fatality_Ratio ordered descending
fatality_plot <- state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State, levels = Province_State)) %>%
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Maximum Case Fatality Ratio by State/Province",
    x = "Province / State",
    y = "Maximum Fatality Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(fatality_plot)

# Question 5: Bar plot of Maximum_Fatality_Ratio ordered descending
fatality_plot <- state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State, levels = Province_State)) %>%
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Maximum Case Fatality Ratio by State/Province",
    x = "Province / State",
    y = "Maximum Fatality Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
# Question 6

plot(x=Cleaned_Covid_19_data$Confirmed,y=Cleaned_Covid_19_data$Deaths)
