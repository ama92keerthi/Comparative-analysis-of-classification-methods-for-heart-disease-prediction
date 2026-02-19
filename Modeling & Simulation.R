library(tidyverse)
library(caret)
library(randomForest)
library(pROC)

# Load dataset
library(readxl)
heart_dataset <- read_excel("D:/UU/Semester 1/R/Project/heart_dataset.xlsx")
view(heart_dataset)

#----------Data preparation----------#
# Convert categorical variables to factors
heart_dataset <- heart_dataset %>%
  mutate(
    Sex = factor(Sex, levels = c("M","F"), labels = c("Male","Female")),
    ChestPainType = factor(ChestPainType),
    RestingECG = factor(RestingECG),
    ExerciseAngina = factor(ExerciseAngina, levels = c("N","Y"),
                            labels = c("No","Yes")),
    ST_Slope = factor(ST_Slope),
    HeartDisease = factor(HeartDisease, levels = c(0,1),
                          labels = c("No","Yes"))
  )


#Simulate larger datasets with replacements
#Generates a larger dataset by sampling rows with replacement
simulate_data <- function(data, n) {
  data %>% slice_sample(n = n, replace = TRUE)
}

#caret training control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

#Feature selection using RFE
rfe_ctrl <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 5
)

# Function for one simualtion
run_simulation <- function(data) {

  #split train/test set                      
  set.seed(123)
  idx <- createDataPartition(data$HeartDisease, p = 0.7, list = FALSE)
  train <- data[idx, ]
  test  <- data[-idx, ]
  

  #Recursive Feature Elimination for Train Data
  x_train <- train %>% select(-HeartDisease)
  y_train <- train$HeartDisease
  
  rfe_fit <- rfe(
    x = x_train,
    y = y_train,
    sizes = 1:10,
    rfeControl = rfe_ctrl
  )
  
  #Optimal Features
  vars <- rfe_fit$optVariables
  print(vars)
  
  train_sel <- train %>% select(HeartDisease, all_of(vars))
  test_sel  <- test  %>% select(HeartDisease, all_of(vars))
  
  #  Logistic Regression using caret 
  log_model <- train(
    HeartDisease ~ .,
    data = train_sel,
    method = "glm",
    family = binomial,
    metric = "ROC",
    trControl = ctrl
  )
  
  # Random Forest using caret 
  rf_model <- train(
    HeartDisease ~ .,
    data = train_sel,
    method = "rf",
    metric = "ROC",
    trControl = ctrl,
    tuneLength = 5
  )
  
  # Test data predictions 
  log_prob <- predict(log_model, test_sel, type = "prob")[,"Yes"]
  rf_prob  <- predict(rf_model, test_sel, type = "prob")[,"Yes"]
  
  log_pred <- predict(log_model, test_sel)
  rf_pred  <- predict(rf_model, test_sel)
  
  #Store test data predictions in a tibble
  tibble(
    Model = c("Logistic Regression","Random Forest"),
    Accuracy = c(
      mean(log_pred == test_sel$HeartDisease),
      mean(rf_pred == test_sel$HeartDisease)
    ),
    AUC = c(
      auc(roc(test_sel$HeartDisease, log_prob)),
      auc(roc(test_sel$HeartDisease, rf_prob))
    )
  )
}

#Run Simulation for larger samples
sample_sizes <- c(1000, 3000, 5000, 10000)

results <- lapply(sample_sizes, function(n) {
  sim_data <- simulate_data(heart_dataset, n)
  out <- run_simulation(sim_data)
  out$SampleSize <- n
  out
})

final_results <- bind_rows(results)
final_results
summary(final_results)



#Performance comparison
library(dplyr)

performance_summary <- final_results %>%
  group_by(Model) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy   = sd(Accuracy, na.rm = TRUE),
    Mean_AUC      = mean(AUC, na.rm = TRUE),
    SD_AUC        = sd(AUC, na.rm = TRUE)
  )

performance_summary


#Accuracy
plot1 <- ggplot(final_results,
       aes(SampleSize, Accuracy, color = Model)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Accuracy vs Sample Size")
ggsave(
  filename = "plot1.jpg",
  plot = plot1
)
#AUC
plot2 <- ggplot(final_results,
       aes(SampleSize, AUC, color = Model)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "AUC vs Sample Size")
ggsave(
  filename = "plot2.jpg",
  plot = plot2
)
#marginal gains
library(dplyr)

marginal_gains <- final_results %>%
  group_by(Model, SampleSize) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Model, SampleSize) %>%
  group_by(Model) %>%
  mutate(
    Marginal_Gain = Mean_Accuracy - lag(Mean_Accuracy)
  )

marginal_gains

library(ggplot2)

plot3 <- ggplot(marginal_gains, aes(x = SampleSize, y = Marginal_Gain, group = Model)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Marginal Gains in Accuracy with Increasing Sample Size",
    x = "Sample Size",
    y = "Marginal Gain in Accuracy"
  ) +
  theme_minimal()
ggsave(
  filename = "plot3.jpg",
  plot = plot3
)