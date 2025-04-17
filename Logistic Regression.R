# Load necessary libraries
library(tidyverse)
library(broom)

# Read the data
survey_data <- read.csv("SurveyData.csv")

# Create a binary bullying indicator (assuming db variables are bullying-related)
# Here we'll consider if any of db1-db18 are >= 2 as indicating bullying
survey_data <- survey_data %>%
  mutate(bullying_indicator = ifelse(rowSums(select(., starts_with("db")) >= 2, 1, 0))

# Fit logistic regression model
model <- glm(bullying_indicator ~ age + gender, 
            data = survey_data, 
            family = binomial)

# Create prediction data frame
pred_data <- expand.grid(
  age = seq(min(survey_data$age, na.rm = TRUE), 
            max(survey_data$age, na.rm = TRUE), 
            length.out = 100),
  gender = c("F", "M")
)

# Get predicted probabilities
preds <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)
pred_data$prob <- preds$fit
pred_data$lower <- preds$fit - 1.96 * preds$se.fit
pred_data$upper <- preds$fit + 1.96 * preds$se.fit

# Create the plot
ggplot(pred_data, aes(x = age, y = prob, color = gender, fill = gender)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(title = "Probability of Bullying by Age and Gender",
       x = "Age (years)",
       y = "Probability of Bullying",
       color = "Gender",
       fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
