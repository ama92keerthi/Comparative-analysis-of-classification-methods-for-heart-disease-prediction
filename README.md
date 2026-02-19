Heart disease is one of the leading causes of morbidity and mortality worldwide and continues
to pose a significant burden on healthcare systems. Early identification of individuals at
high risk of heart disease is therefore of considerable importance, as timely intervention and
preventive strategies can improve patient outcomes and reduce healthcare costs. In this
project, the problem of heart disease prediction was investigated using routinely collected
clinical and demographic data, with the aim of comparing the performance of traditional
statistical modeling approaches and modern machine learning methods.
A publicly available heart disease dataset, containing clinical records from 918 patients,
was analyzed within the R statistical computing environment. Exploratory data analysis
was conducted to examine variable distributions, assess class imbalance, and explore
relationships between predictors and heart disease status. Two classification models, logistic
regression and random forest, were subsequently implemented to predict the presence of
heart disease. Logistic regression was used as a baseline model due to its interpretability
and widespread use in clinical research, while random forest was employed to capture
potential nonlinear relationships and interactions among predictors.
Model performance was evaluated using classification accuracy and the area under the
receiver operating characteristic curve (AUC). In addition, a simulation-based framework
was applied to examine how predictive performance changes with increasing sample size
and to assess model stability. The results indicate that both models are capable of
distinguishing between patients with and without heart disease; however, random forest
consistently achieves higher predictive accuracy and AUC across all evaluated sample
sizes. Logistic regression, while less accurate, demonstrates more stable performance and
greater interpretability. These findings highlight an important trade-off between predictive
performance and model transparency in the context of heart disease prediction.
