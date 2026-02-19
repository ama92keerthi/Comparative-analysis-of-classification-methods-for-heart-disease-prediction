library(tidyverse)
library(ggplot2)

# Import datset

library(readxl)
heart_dataset <- read_excel("D:/UU/Semester 1/R/Project/heart_dataset.xlsx")
View(heart_dataset)

# Count missing values
colSums(is.na(heart_dataset))

# Class imbalance check
 ggplot(heart_dataset, aes(x = factor(HeartDisease))) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           fill = "orange") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Heart Disease (0 = No, 1 = Yes)",
    y = "Percentage",
    title = "Distribution of Target Variable"
  ) +
  theme_minimal()


prop.table(table(heart_dataset$HeartDisease))

# Convert variables to factors
heart_dataset <- heart_dataset %>%
  mutate(across(where(is.character), as.factor))

#Rename Factor levels
library(forcats)

heart_dataset <- heart_dataset %>%
  mutate(
    HeartDisease = fct_recode(
      factor(HeartDisease),
      "No Disease" = "0",
      "Disease"    = "1"
    )
  )
heart_dataset <- heart_dataset %>%
  mutate(
    Sex = fct_recode(
      factor(Sex),
      "Female" = "F",
      "Male"    = "M"
    )
  )
heart_dataset <- heart_dataset %>%
  mutate(
    ExerciseAngina = fct_recode(
      ExerciseAngina,
      "Yes" = "Y",
      "No"  = "N"
    )
  )

# Summary of numerical variables
heart_dataset %>%
  select_if(is.numeric) %>%
  summary()

# Graphical check of numerical variables
heart_dataset %>% 
  select_if(is.numeric) %>%
  gather() %>%
   ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill="blue", color="white") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()


# Check for categorical variables
library(tidyverse)
heart_dataset %>%
  select(where(is.factor)) %>%
  pivot_longer(
    cols = where(is.factor),
    names_to = "variable",
    values_to = "category"
  ) %>%
  ggplot(aes(x = category, fill = category)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Categorical Features",
       x = NULL, y = "Count")

## Bivariate Analysis
#a) Numerical vs Heart Disease Status
library(tidyverse)

heart_dataset %>%
  pivot_longer(
    cols = where(is.numeric) & !HeartDisease,
    names_to = "feature",
    values_to = "value"
  ) %>%
 ggplot(aes(x = factor(HeartDisease), y = value, fill = factor(HeartDisease))) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ feature, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "HeartDisease",
    y = "Value",
    title = "Numeric Features by Heart Disease Status"
  )

#b) categorical vs Heart Disease Status
heart_dataset %>%
  group_by(HeartDisease, ChestPainType) %>%
  tally() %>%
  ggplot(aes(x=ChestPainType, y=n, fill=factor(HeartDisease))) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heart_dataset %>%
  group_by(HeartDisease, Sex) %>%
  tally() %>%
  ggplot(aes(x=Sex, y=n, fill=factor(HeartDisease))) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


heart_dataset %>%
  group_by(HeartDisease, ExerciseAngina) %>%
  tally() %>%
  ggplot(aes(x=ExerciseAngina, y=n, fill=factor(HeartDisease))) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)

corr <- cor(heart_dataset %>% select_if(is.numeric))
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, title="Correlation Matrix")




