library(tidyverse)
library(broom)
Grad_Admins <- read.csv("GradSchool_Grad_Admins.csv")
#Exploring the Dataset
head(Grad_Admins)
str(Grad_Admins)
summary(Grad_Admins)
#Checking missing values
colSums(is.na(Grad_Admins))
# Calculate admission percentages
prop.table(table(Grad_Admins$admit)) * 100
# Compare GRE and GPA by admission outcome
Grad_Admins %>%
  group_by(admit) %>%
  summarise(
    Mean_GRE = mean(gre),
    Mean_GPA = mean(gpa),
    Number_of_Applicants = n()
  )

# Visualize GRE scores
ggplot(Grad_Admins, aes(x = gre, fill = factor(admit))) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(
    title = "GRE Scores by Admission Outcome",
    x = "GRE Score",
    y = "Number of Applicants",
    fill = "Admitted"
  ) +
  theme_minimal()

# Visualize GPA
ggplot(Grad_Admins, aes(x = gpa, fill = factor(admit))) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(
    title = "GPA by Admission Outcome",
    x = "GPA",
    y = "Number of Applicants",
    fill = "Admitted"
  ) +
  theme_minimal()

# Admission rates by institution rank
rank_summary <- Grad_Admins %>%
  group_by(rank) %>%
  summarise(
    Applicants = n(),
    Accepted = sum(admit),
    Admission_Rate = mean(admit) * 100
  )

print(rank_summary)

# Plot admission rates by rank
ggplot(rank_summary,
       aes(x = factor(rank), y = Admission_Rate)) +
  geom_col() +
  labs(
    title = "Admission Rate by Institution Rank",
    x = "Institution Rank",
    y = "Admission Rate (%)"
  ) +
  theme_minimal()

# ==========================================
# LOGISTIC REGRESSION MODEL
# ==========================================

# Convert rank into a categorical variable
Grad_Admins$rank <- as.factor(Grad_Admins$rank)

# Fit logistic regression
mod1 <- glm(
  admit ~ I(gre / 100) + I(gpa * 10) + rank,
  data = Grad_Admins,
  family = binomial
)

# Examine model results
summary(mod1)

# Calculate odds ratios
exp(coef(mod1))

# Calculate confidence intervals
exp(confint(mod1))

# Model predictions
Grad_Admins$predicted_probability <- predict(
  mod1,
  type = "response"
)

# View predicted probabilities
head(Grad_Admins)

# Example applicant
new_applicant <- data.frame(
  gre = 600,
  gpa = 3.5,
  rank = factor(1, levels = levels(Grad_Admins$rank))
)

predict(
  mod1,
  newdata = new_applicant,
  type = "response"
)

# Visualize predicted probabilities
ggplot(Grad_Admins,
       aes(x = predicted_probability,
           fill = factor(admit))) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(
    title = "Predicted Admission Probabilities",
    x = "Predicted Probability of Admission",
    y = "Number of Applicants",
    fill = "Actual Admission"
  ) +
  theme_minimal()
