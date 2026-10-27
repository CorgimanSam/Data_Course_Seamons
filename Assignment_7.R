library(scales)
library(corrplot)
library(tidyverse)
Utah_Religions_per_County <- read.csv("Utah_Religions_by_County.csv")

# Standardize column names
county_data <- Utah_Religions_per_County %>%
  rename_with(~ str_replace_all(
    str_to_lower(.x),
    "[^a-z0-9]+", "_"
  ))

# Identify religious-group count columns
religion_cols <- setdiff(
  names(county_data),
  c("county", "population")
)
religion_cols

# Convert group counts and proportions into long format
county_tidy <- county_data %>%
  pivot_longer(
    cols = all_of(religion_cols),
    names_to = "religion",
    values_to = "count"
  )  # %>%
County_clean <- mutate(county_tidy
                       , proportion = county_tidy$count / county_data$pop_2010,
                       religion = str_remove(religion, "^count_")
)

# Inspect the cleaned data
glimpse(county_tidy)
head(county_tidy)
# Question 1
plot_1 <- ggplot(County_clean, aes(count,proportion))+geom_point()
plot_1 +ylim(c(0, 55)) + xlim(0,500000)+geom_smooth(method = lm)
#Question 2
ggplot(county_data, aes(religious,non_religious))+geom_point()+geom_smooth(method = lm)