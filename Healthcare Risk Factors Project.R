#Load data set
t_data = read.csv("dirty_v3_path.csv")

#view data set and data structure
print(head(t_data, 6))
View(t_data)
str(t_data)
print(colnames(t_data))

#load necessary package 
library(tidyverse)

#inspect missing value
colSums(is.na(t_data))
colMeans(is.na(t_data))*100
##visualize missing value
install.packages("Amelia")
library(Amelia)
missmap(t_data, main = "Missing data")

#normality test for numeric variables
install.packages("nortest")
library(nortest)
##Anderson-Darling normality test
ad.test(t_data$Age[!is.na(t_data$Age)])
ad.test(t_data$Glucose[!is.na(t_data$Glucose)])
ad.test(t_data$Blood.Pressure[!is.na(t_data$Blood.Pressure)])
##replace missing value with column median
t_data$Age[is.na(t_data$Age)] = median(t_data$Age, na.rm = TRUE)
t_data$Glucose[is.na(t_data$Glucose)] = median(t_data$Glucose, na.rm = TRUE)
t_data$Blood.Pressure[is.na(t_data$Blood.Pressure)] = median(t_data$Blood.Pressure, na.rm = TRUE)
##verify if missing values still exist
colSums(is.na(t_data))
View(t_data)
str(t_data)
##Gender column
unique(t_data$Gender)
table(t_data$Gender)
t_data$Gender[t_data$Gender==""] = "Unknown"
##medical condition column
unique(t_data$Medical.Condition)
table(t_data$Medical.Condition)
t_data$Medical.Condition[t_data$Medical.Condition==""] = "Unknown"
table(t_data$Stress.Level)

#remove unnecessary columns
t_data = t_data[,c(-19,-20)]

#check for duplicate
sum(duplicated(t_data))
any(duplicated(t_data))
###To remove duplicates
distinct(t_data)

#data type conversion
str(t_data)
##convert binary variables to factor
t_data$Alcohol = factor(t_data$Alcohol, labels = c("No", "Yes"))
t_data$Smoking = factor(t_data$Smoking, labels = c("No", "Yes"))
t_data$Family.History = factor(t_data$Family.History, labels = c("No", "Yes"))
##convert categorical variables
t_data$Medical.Condition = as.factor(t_data$Medical.Condition)
t_data$Gender = as.factor(t_data$Gender)

#detect and handle Outliers
summary(t_data)
##boxplot for key numeric variables
boxplot(t_data[, sapply(t_data, is.numeric)], main = "Boxplot")
boxplot(t_data$Glucose, main = "Glucose")
lower = quantile(t_data$Glucose, 0.01, na.rm = TRUE)
upper = quantile(t_data$Glucose, 0.99, na.rm = TRUE)
t_data$Glucose = pmin(pmax(t_data$Glucose, lower), upper)
 

lower = quantile(t_data$Blood.Pressure, 0.01, na.rm = TRUE)
upper = quantile(t_data$Blood.Pressure, 0.99, na.rm = TRUE)
t_data$Blood.Pressure = pmin(pmax(t_data$Blood.Pressure, lower), upper)
 
 
lower = quantile(t_data$BMI, 0.01, na.rm = TRUE)
upper = quantile(t_data$BMI, 0.99, na.rm = TRUE)
t_data$BMI = pmin(pmax(t_data$BMI, lower), upper)
 
 
lower = quantile(t_data$Triglycerides, 0.01, na.rm = TRUE)
upper = quantile(t_data$Triglycerides, 0.99, na.rm = TRUE)
t_data$Triglycerides = pmin(pmax(t_data$Triglycerides, lower), upper)

par(mfrow = c(2, 2)) # 2x2 grid
boxplot(t_data$Glucose, main = "Glucose")
boxplot(t_data$Blood.Pressure, main = "Blood Pressure")
boxplot(t_data$BMI, main = "BMI")
boxplot(t_data$Triglycerides, main = "Triglycerides")
par(mfrow = c(1, 1)) # Reset
 
lower = quantile(t_data$Cholesterol, 0.01, na.rm = TRUE)
upper = quantile(t_data$Cholesterol, 0.99, na.rm = TRUE)
t_data$Cholesterol = pmin(pmax(t_data$Cholesterol, lower), upper)

lower = quantile(t_data$Oxygen.Saturation, 0.01, na.rm = TRUE)
upper = quantile(t_data$Oxygen.Saturation, 0.99, na.rm = TRUE)
t_data$Oxygen.Saturation = pmin(pmax(t_data$Oxygen.Saturation, lower), upper)

lower = quantile(t_data$HbA1c, 0.01, na.rm = TRUE)
upper = quantile(t_data$HbA1c, 0.99, na.rm = TRUE)
t_data$HbA1c = pmin(pmax(t_data$HbA1c, lower), upper)

lower = quantile(t_data$Sleep.Hours, 0.01, na.rm = TRUE)
upper = quantile(t_data$Sleep.Hours, 0.99, na.rm = TRUE)
t_data$Sleep.Hours = pmin(pmax(t_data$Sleep.Hours, lower), upper)

par(mfrow = c(2, 2)) # 2 rows, 2 cols
boxplot(t_data$Cholesterol, main = "Cholesterol")
boxplot(t_data$Oxygen.Saturation, main = "Oxygen Saturation")
boxplot(t_data$HbA1c, main = "Glycated Haemoglobin")
boxplot(t_data$Sleep.Hours, main = "Sleep Hours")
par(mfrow = c(1, 1)) # Reset
#save cleaned dataset
write.csv(t_data, "cleaned healtcare data.csv")


##Reload dataset
datta = read.csv("cleaned healtcare data.csv")
head(datta)
tail(datta)
datta = filter(datta, Medical.Condition != "Unknown")
View(datta)

#exploratory data analysis (EDA)
summary(datta)
datta = datta[,-1]
str(datta)
num_cols = c("Age", "Glucose", "Blood.Pressure", "BMI", "Oxygen.Saturation",
             "LengthOfStay", "Cholesterol", "Triglycerides", "HbA1c", "Sleep.Hours",
             "Physical.Activity", "Diet.Score", "Stress.Level")
datta_num_cols = datta[num_cols]
car_cols = c("Gender", "Medical.Condition", "Smoking", "Alcohol", "Family.History")
datta_car_cols = datta[car_cols]

##Descriptive summary for non-categorical variab
install.packages("ggcorrplot")
install.packages("janitor")
install.packages("summarytools")
library(DataExplorer)
library(skimr)le
##install and load necessary packages
install.packages("DataExplorer")
install.packages("skimr")
install.packages("corrplot")
library(corrplot)
library(ggcorrplot)
library(janitor)
library(summarytools)
library(patchwork)
##univariate analysis
# Histogram (For Numeric variables)
plt_1 = ggplot(datta, aes(x = BMI)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "BMI")
plt_2 = ggplot(datta, aes(x = Age)) + 
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Age")
plt_3 = ggplot(datta, aes(x = Glucose)) + 
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Glucose")
plt_4 = ggplot(datta, aes(x = Blood.Pressure)) + 
  geom_histogram(fill = "steelblue", bins = 30) + 
  labs(title = "Blood Pressure")
plt_5 = ggplot(datta, aes(x = Oxygen.Saturation)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Oxygen Saturation level")
plt_7 = ggplot(datta, aes(x = Cholesterol)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Cholesterol")
plt_8 = ggplot(datta, aes(x = Triglycerides)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Triglycerides")
plt_9 = ggplot(datta, aes(x = HbA1c)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Glycated Haemoglobin")
plt_10 = ggplot(datta, aes(x = Sleep.Hours)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "sleep hours")
plt_11 = ggplot(datta, aes(x = Physical.Activity)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Physical Activity")
plt_12 = ggplot(datta, aes(x = Diet.Score)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Diet Score")
plt_13 = ggplot(datta, aes(x = Stress.Level)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Stress Level")

(plt_1 + plt_2 + plt_3) / (plt_4 + plt_5 + plt_7)
(plt_8 + plt_9 + plt_10) / (plt_11 + plt_12 + plt_13)

# Barchart(for categorical)
plt_a = ggplot(datta, aes(x = Gender)) + 
  geom_bar(fill = "darkgreen") +
  labs(title = "Gender")
plt_b = ggplot(datta, aes(x = Medical.Condition)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Medical condition")
plt_c = ggplot(datta, aes(x = Smoking)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Smoking")
plt_d = ggplot(datta, aes(x = Alcohol)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Alcohol")
plt_e = ggplot(datta, aes(x = Family.History)) + 
  geom_bar(fill = "darkgreen") +
  labs(title = "Family Diseasse History")

(plt_a  + plt_c) / (plt_d + plt_e)
##Bivariate analysis
#Numerical vs categorical
#BMI across medical conditions
ggplot(datta, aes(x = Medical.Condition, y = BMI, fill = Medical.Condition)) +
  geom_boxplot() +
  labs(title = "BMI by Medical Condition") +
  theme(legend.position = "none")
#Blood pressure across medical conditions
ggplot(datta, aes(x = Medical.Condition, y = Blood.Pressure, fill = Medical.Condition)) +
  geom_boxplot() +
  labs(title = "Blood Pressure by Medical Condition") +
  theme(legend.position = "none")


# Plot
ggplot(datta, aes(x = Medical.Condition, y = Blood.Pressure, fill = Medical.Condition)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Blood Pressure by Medical Condition", x = "Medical Condition", y = "Blood Pressure") +
  theme(legend.position = "none") # Hide legend if redundant


#glucose level across medical condition
ggplot(datta, aes(x = Medical.Condition, y = Glucose, fill = Medical.Condition)) +
  geom_boxplot() +
  labs(title = "Glucose level by Medical Condition") +
  theme(legend.position = "none")
#categorical vs categorical
#Gender on medical condition
ggplot(datta, aes(x = Gender, fill = Medical.Condition)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Health Conditions by Gender", y = "Proportion")
# Smoking on medical condition
ggplot(datta, aes(x = Smoking, fill = Medical.Condition)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Health Conditions by Smoking Status", y = "Proportion")
#Drinking on deical condition
ggplot(datta, aes(x = Alcohol, fill = Medical.Condition)) +
  geom_bar(position = "fill") +
  labs(title = "Propotion of Health condition by Alcohol Consumption Status",
       y = "Proportion")
#Family history on medical condition
ggplot(datta, aes(x = Family.History, fill = Medical.Condition)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Health Condition by Family Health History",
       y = "Proportion")

#Numeric vs Numeric
ggplot(datta, aes(x = Sleep.Hours, y = Stress.Level)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Stress vs Sleep Hours")
#correlation Analysis
corr_matrix = cor(datta[num_cols], use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.9)


####Feature engineering
colnames(datta)
str(datta)
##variable encoding
datta$Gender = as.factor(datta$Gender)
datta$Medical.Condition = as.factor(datta$Medical.Condition)
datta$Smoking = as.factor(datta$Smoking)
datta$Alcohol = as.factor(datta$Alcohol)
datta$Family.History = as.factor(datta$Family.History)
View(datta)
datta$LengthOfStay = as.numeric(datta$LengthOfStay)
##feature creation
#wellness index
datta$Wellness.Index = scale(-datta$BMI) + scale(-datta$Cholesterol) + 
  scale(-datta$Triglycerides) + scale(-datta$Glucose) + 
  scale(-datta$HbA1c)
#Lifestyle score
datta$Lifestyle.Score = scale(as.numeric(datta$Physical.Activity)) + 
  scale(as.numeric(datta$Diet.Score)) + scale(as.numeric(datta$Sleep.Hours)) - 
  scale(as.numeric(datta$Stress.Level))


##feature selection
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
#Feature selection using Boruta model
set.seed(123)
bor.model = Boruta(Medical.Condition ~ ., data = datta, doTrace = 2)
attStats(bor.model)
#Feature selection using Random forest model
feature_forest.model = randomForest(Medical.Condition ~ ., data = datta,
                                    ntree = 500)
importance(feature_forest.model)
varImpPlot(feature_forest.model, main = "Feature Importance Plot")

##features with high importance are: Age, HbA1c, glucose, blood pressure, BMI,
##physical activity. wellness index, lifestyle score, stress level, diet score,
##Oxygen saturation, Triglycerides, cholesterol, sleep hours

#Extract features from dataset
colnames(datta)
selected_features = c("Age", "Glucose", "Blood.Pressure", "BMI", "Oxygen.Saturation",
                      "Cholesterol", "Triglycerides", "HbA1c", "Physical.Activity",
                      "Diet.Score", "Stress.Level", "Sleep.Hours",
                      "Wellness.Index", "Lifestyle.Score")
datta_selected = datta[,c(selected_features, "Medical.Condition")]
str(datta_selected)
colnames(datta_selected)
write.csv(datta_selected, file = "Healthcare selected features.csv")

##train - test split
model_data = read.csv("Healthcare selected features.csv")
model_data$Medical.Condition = as.factor(model_data$Medical.Condition)
set.seed(123) #to ensure reproducibility

train_index = createDataPartition(model_data$Medical.Condition, p = 0.8,
                                  list = FALSE)
train_set = model_data[train_index,]
test_set = model_data[-train_index,]

##predictive modeling on imbalanced dataset
library(rpart)
library(rpart.plot)

#decision tree
tree_model = rpart(Medical.Condition ~ ., data = train_set, method = "class")
rpart.plot(tree_model)
#evaluate performance
tree_model_pred = predict(tree_model, test_set, type = "class")
confusionMatrix(tree_model_pred, test_set$Medical.Condition)


#Random forest
forest_model = randomForest(Medical.Condition ~ ., data = train_set, ntree = 500)
#model evaluation
forest_model_pred = predict(forest_model, test_set)
confusionMatrix(forest_model_pred, test_set$Medical.Condition)


#XGBoost
library(xgboost)
# Create design matrices (remove the target with -1)
train_matrix = model.matrix(Medical.Condition ~ . - 1, data = train_set)
test_matrix  = model.matrix(Medical.Condition ~ . - 1, data = test_set)

# Extract labels
train_labels = train_set$Medical.Condition
train_labels = as.factor(train_labels)
train_labels = as.integer(train_labels) - 1

test_labels  = test_set$Medical.Condition
test_labels = as.factor(test_labels)
test_labels = as.integer(test_labels) - 1


num_class = length(unique(train_labels))
num_class

xgb_model = xgboost(data = train_matrix,label = train_labels,
                    objective = "multi:softmax",num_class = num_class,
                    nrounds = 100,verbose = 0)
preds = predict(xgb_model, test_matrix)
preds_factor = factor(preds, levels = 0:(num_class-1))
test_labels_factor = factor(test_labels, levels = 0:(num_class-1))
confusionMatrix(preds_factor, test_labels_factor)


##Random oversampling
train_balanced = upSample(
x = train_set[, setdiff(names(train_set), "Medical.Condition")],
  y = train_set$Medical.Condition
)

train_balanced$X = NULL
View(train_balanced)
prop.table(table(train_balanced$Class))
#modelling randomly oversampled dataset
#decision tree
sampled_tree_model = rpart(Class ~ ., data = train_balanced, method = "class")
#evaluate performance
sampled_tree_model_pred = predict(sampled_tree_model, test_set, type = "class")
confusionMatrix(sampled_tree_model_pred, test_set$Medical.Condition)


#Random forest
sampled_forest_model = randomForest(Class ~ ., 
                                    data = train_balanced, ntree = 500)
#model evaluation
sampled_forest_model_pred = predict(sampled_forest_model, test_set, type = "class")
confusionMatrix(sampled_forest_model_pred, test_set$Medical.Condition)



#XGBoost
# Create design matrices (remove the target with -1)
train_matrix = model.matrix(Class ~ . - 1, data = train_balanced)

# Extract labels
train_labels = train_balanced$Class
train_labels = as.factor(train_labels)
train_labels = as.integer(train_labels) - 1


num_class = length(unique(train_labels))
num_class

xgb_model = xgboost(data = train_matrix,label = train_labels,
                    objective = "multi:softmax",num_class = num_class,
                    nrounds = 100,verbose = 0)
preds = predict(xgb_model, test_matrix)
preds_factor = factor(preds, levels = 0:(num_class-1))
test_labels_factor = factor(test_labels, levels = 0:(num_class-1))
confusionMatrix(preds_factor, test_labels_factor)



##hyperparameter tuning for random forest and XGBoost model
#Random forest
control = trainControl(method = "cv", number = 5)

rf_grid = expand.grid(
  mtry = c(2, 3, 5, 7)
)

rf_tuned = train(
  Class ~ .,
  data = train_balanced,
  method = "rf",
  trControl = control,
  tuneGrid = rf_grid,
  ntree = 500
)

rf_tuned

# Make predictions on test set
test_set = rename(test_set, Class = Medical.Condition)

rf_preds = predict(rf_tuned, newdata = test_set)
# Step 3: Ensure both predictions and test labels are factors with the same levels
rf_preds = factor(rf_preds, levels = levels(test_set$Class))
test_set$Class = factor(test_set$Class, levels = levels(test_set$Class))

# Step 4: Compute confusion matrix
confusionMatrix(rf_preds, test_set$Class)


#XGBoost

params = list(
  objective = "multi:softprob",
  num_class = num_class,
  eval_metric = "mlogloss",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

cv_model = xgb.cv(
  params = params,
  data = train_matrix,
  label = train_labels,
  nrounds = 300,
  nfold = 5,
  verbose = 0,
  early_stopping_rounds = 20
)

best_nrounds = cv_model$best_iteration
best_nrounds

model_final = xgboost(
  params = params,
  data = train_matrix,
  label = train_labels,
  nrounds = best_nrounds,
  verbose = 0
)



objective = "multi:softprob"
pred_prob = predict(model_final, test_matrix)
pred_matrix = matrix(pred_prob, 
                      nrow = num_class, 
                      ncol = length(pred_prob)/num_class)
pred_matrix = t(pred_matrix)
pred_labels = max.col(pred_matrix) - 1

confusionMatrix(
  factor(pred_labels),
  factor(test_labels)
)
