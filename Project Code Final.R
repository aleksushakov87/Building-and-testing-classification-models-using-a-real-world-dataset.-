## Task 1
# Load libraries, dataset and characterize initial features of the dataset 
# such as number of NA values by variable.
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(rsample)
library(ROSE)
library(randomForest)
library(FSelector)
library(fastDummies)
library(gbm)
library(pROC)
library(e1071)
library(klaR)
library(RSNNS)
library(caret)

# Please specify the directory you're using. 
path <- 'C:/Users/USA/University/BU/III TERM/CS699_Lee/Project/Final report'
setwd(path)
getwd()

# Load the dataset
data_initial <- read.csv("project_data.csv")

# Understand the dataset
str(data_initial)
summary(data_initial)

# Determine NA counts and print output
na_counts_start <- sapply(data_initial, function(x) sum(is.na(x)))
print(na_counts_start)

## Task 2
# Initial preprocessing, remove variables which have been determined to be useless 
# and convert all values in the data which correspond to missing data to NA

# Preprocessing Step 1: Drop useless data
# Useless data is any data that relates to the collection of the data and not the data itself, or are duplicated within other data, the following variables have been deemed useless on this basis
data <- subset(data_initial, select = -c(FMONTH, IDATE, IMONTH, IDAY, IYEAR, SEQNO, QSTVER, QSTLANG, HEIGHT3, WEIGHT2, HTIN4, DRNKANY5))

# FMONTH, IDATE, IMONTH, IDAY, IYEAR,  SEQNO, QSTVER - all removed because they related to data about the datapoint, 
#                                                      rather than data associated with properties of the datapoint
# HEIGHT3, WEIGHT2, and HTIN4 - are all removed because this information is contained in WTKG3 and HTM4
# DRNKANY5, this is duplicative with ALCDAY5 and contains less information since DRNKANY5 is a frequency over an 
#                                           interval of time and ALCDAY5 is just a binary response over an interval of time.

# Proprocessing Step 2: convert 7, 9 and [blank] to NA for variables where this means no response/refused/doesn't know
# Relevant variables: GENHLTH, HLTHPLN1, PERSDOC2, MEDCOST, CHECKUP1, BPHIGH4, CHOLCHK2, TOLDHI2, CVDINFR4, CVDCRHD4, 
#                     CVDSTRK3, ASTHMA3, CHCCOPD2, ADDEPEV3, CHCKDNY2, DIABETE4, HAVARTH4, MARITAL, EDUCA, RENTHOM1, 
#                     CPDEMO1B, VETERAN3, DEAF, BLIND, DECIDE, DIFFWALK, DIFFDRES, DIFFALON, SMOKE100, USENOW3, EXERANY2, 
#                     FLUSHOT7, TETANUS1, PNEUVAC4, HIVTST7, HIVRISK5
# Define columns to modify
columns_to_modify_p2 <- c('GENHLTH', 'HLTHPLN1', 'PERSDOC2', 'MEDCOST', 'CHECKUP1', 'BPHIGH4', 'CHOLCHK2', 'TOLDHI2',
                          'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3', 'CHCCOPD2', 'ADDEPEV3', 'CHCKDNY2', 'DIABETE4',
                          'HAVARTH4', 'MARITAL', 'EDUCA', 'RENTHOM1', 'CPDEMO1B', 'VETERAN3', 'DEAF', 'BLIND', 'DECIDE',
                          'DIFFWALK', 'DIFFDRES', 'DIFFALON', 'SMOKE100', 'USENOW3', 'EXERANY2', 'FLUSHOT7', 'TETANUS1',
                          'PNEUVAC4', 'HIVTST7', 'HIVRISK5')

# Count occurrences of 7, 9, and blanks before modification
count_7_before_p2 <- sum(data[columns_to_modify_p2] == 7, na.rm = TRUE)
count_9_before_p2 <- sum(data[columns_to_modify_p2] == 9, na.rm = TRUE)
count_blank_before_p2 <- sum(is.na(data[columns_to_modify_p2]))

# Replace 7, 9, and blanks with NA
for (col in columns_to_modify_p2) {
  data[[col]][data[[col]] %in% c(7, 9, "")] <- NA
}

# Count occurrences of 7, 9, and blanks after modification
count_7_after_p2 <- sum(data[columns_to_modify_p2] == 7, na.rm = TRUE)
count_9_after_p2 <- sum(data[columns_to_modify_p2] == 9, na.rm = TRUE)
count_blank_after_p2 <- sum(is.na(data[columns_to_modify_p2]))

# Prepare a summary of counts before and after modification
summary_counts_p2 <- data.frame(
  Value = c('7', '9', 'Blank'),
  Count_Before = c(count_7_before_p2, count_9_before_p2, count_blank_before_p2),
  Count_After = c(count_7_after_p2, count_9_after_p2, count_blank_after_p2)
)

# Display the summary counts
print(summary_counts_p2)

# Proprocessing Step 2b: convert 9 and [blank] to NA for variables where this 
# means no response/refused/doesn't know
# Relevant variables: EMPLOY1
columns_to_modify_p2b <- c('EMPLOY1')

# Count occurrences of 9, and blanks before modification
count_9_before_p2b <- sum(data[columns_to_modify_p2b] == 9, na.rm = TRUE)
count_blank_before_p2b <- sum(is.na(data[columns_to_modify_p2b]))

# Replace 9, and blanks with NA
for (col in columns_to_modify_p2b) {
  data[[col]][data[[col]] %in% c(9, "")] <- NA
}

# Count occurrences of 9, and blanks after modification
count_9_after_p2b <- sum(data[columns_to_modify_p2b] == 9, na.rm = TRUE)
count_blank_after_p2b <- sum(is.na(data[columns_to_modify_p2b]))

# Prepare a summary of counts before and after modification
summary_counts_p2b <- data.frame(
  Value = c('9', 'Blank'),
  Count_Before = c(count_9_before_p2b, count_blank_before_p2b),
  Count_After = c(count_9_after_p2b, count_blank_after_p2b)
)

# Display the summary counts
print(summary_counts_p2b)

# Proprocessing Step 3: convert 77, 99 and [blank] to NA for variables where this means no response/refused/doesn't know
# Relevant variables: PHYSHLTH, MENTHLTH, CHILDREN, INCOME2, 
columns_to_modify_p3 <- c('PHYSHLTH', 'MENTHLTH', 'CHILDREN', 'INCOME2')

# Count occurrences of 77, 99, and blanks before modification
count_77_before_p3 <- sum(data[columns_to_modify_p3] == 77, na.rm = TRUE)
count_99_before_p3 <- sum(data[columns_to_modify_p3] == 99, na.rm = TRUE)
count_blank_before_p3 <- sum(is.na(data[columns_to_modify_p3]))

# Replace 77, 99, and blanks with NA
for (col in columns_to_modify_p3) {
  data[[col]][data[[col]] %in% c(77, 99, "")] <- NA
}

# Count occurrences of 7, 9, and blanks after modification
count_77_after_p3 <- sum(data[columns_to_modify_p3] == 77, na.rm = TRUE)
count_99_after_p3 <- sum(data[columns_to_modify_p3] == 99, na.rm = TRUE)
count_blank_after_p3 <- sum(is.na(data[columns_to_modify_p3]))

# Prepare a summary of counts before and after modification
summary_counts_p3 <- data.frame(
  Value = c('77', '99', 'Blank'),
  Count_Before = c(count_77_before_p3, count_99_before_p3, count_blank_before_p3),
  Count_After = c(count_77_after_p3, count_99_after_p3, count_blank_after_p3)
)

# Display the summary counts
print(summary_counts_p3)

# Proprocessing Step 4: convert 777, 999 and [blank] to NA for variables where this means no response/refused/doesn't know
# Relevant variables: ALCDAY5, STRENGTH, FRUIT2, FRUITJU2, FVGREEN1, FRENCHF1, POTATOE1, VEGETAB2, 

columns_to_modify_p4 <- c('ALCDAY5', 'STRENGTH', 'FRUIT2', 'FRUITJU2', 'FVGREEN1', 'FRENCHF1', 'POTATOE1', 'VEGETAB2')

# Count occurrences of 777, 999, and blanks before modification
count_777_before_p4 <- sum(data[columns_to_modify_p4] == 777, na.rm = TRUE)
count_999_before_p4 <- sum(data[columns_to_modify_p4] == 999, na.rm = TRUE)
count_blank_before_p4 <- sum(is.na(data[columns_to_modify_p4]))

# Replace 777, 999, and blanks with NA
for (col in columns_to_modify_p4) {
  data[[col]][data[[col]] %in% c(777, 999, "")] <- NA
}

# Count occurrences of 7, 9, and blanks after modification
count_777_after_p4 <- sum(data[columns_to_modify_p4] == 777, na.rm = TRUE)
count_999_after_p4 <- sum(data[columns_to_modify_p4] == 999, na.rm = TRUE)
count_blank_after_p4 <- sum(is.na(data[columns_to_modify_p4]))

# Prepare a summary of counts before and after modification
summary_counts_p4 <- data.frame(
  Value = c('777', '999', 'Blank'),
  Count_Before = c(count_777_before_p4, count_999_before_p4, count_blank_before_p4),
  Count_After = c(count_777_after_p4, count_999_after_p4, count_blank_after_p4)
)

# Display the summary counts
print(summary_counts_p4)

## Task 3 
# Classify unedited variables, replace all NA values with the mode for that column, 
# provide ~5 visualizations for categorical variables
# Unused variables: SEXVAR, HTIN4, HTM4, WTKG3, Class

unused_vars <- c('DISPCODE', 'SEXVAR', 'HTM4', 'WTKG3', 'Class')

# Validate that all variables are used in preprocessing so far
collections <- list(columns_to_modify_p2, columns_to_modify_p2b, columns_to_modify_p3, columns_to_modify_p4, unused_vars)
total_length <- sum(sapply(collections, length)) + 12 #This is length of removed variables in preprocessing 1
total_length # should be 66, matching distinct variables in dataset

# Observe the impact which replacing missing datapoints with NA has had on overall NA frequency.
na_counts_end <- sapply(data, function(x) sum(is.na(x)))
print(na_counts_end)
print(na_counts_start)

# Here we apply mode-based inputation for all NA in categorical columns. 
# Highest NA counts are Tetanus1 - 958, INCOME2 - 923, PNEUVAC4 - 848. 
calculate_mode <- function(column) {
  unique_vals <- unique(column)
  unique_vals <- unique_vals[!is.na(unique_vals)]
  tab <- tabulate(match(column, unique_vals))
  unique_vals[which.max(tab)]
}

# Define the function to replace NAs with mode values, excluding specified numerical columns
exclude = c('ALCDAY5', 'WTKG3', 'HTM4', 'STRENGTH', 'FRUIT2', 'FRUITJU2', 'FVGREEN1', 'FRENCHF1', 'POTATOE1', 'VEGETAB2')
data <- data %>%
  mutate(across(
    .cols = -all_of(exclude), 
    .fns = ~ {
      mode_value <- calculate_mode(.)
      replace(., is.na(.), mode_value)
    }
  ))


# Apply the function to the dataframe
na_counts_after_mode_imputation <- sapply(data, function(x) sum(is.na(x)))
print(na_counts_after_mode_imputation)

#Select some categorical variables for visialization:
variables_to_plot <- c('HLTHPLN1', 'MARITAL', 'EDUCA', 'EMPLOY1', 'CHILDREN')

# Convert selected variables to factors to treat them as categorical.
# This step may need to be undone, or perhaps also done to all categorical variables. 
data[variables_to_plot] <- lapply(data[variables_to_plot], as.factor)

#create barplot for some small subset of the categorical variables
# Reshape data to long format
data_long1 <- data %>%
  dplyr::select(all_of(variables_to_plot)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot the frequencies using ggplot2
ggplot(data_long1, aes(x = Value)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Categorical Frequencies",
       x = "Category",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) ,
        plot.title = element_text(hjust = 0.5))

## Task 4 
# Normalize the numeric variables - this will be normalizing all of the 
# values to a numeric measure based on the categorical responses
# Since this is the final preprocessing step, we'll also write data to csv file. 

# Normalize numeric variables based on value; normalizing all time based units to 'frequency per year'
# ALCDAY5 - 101-107; days a week, 201 to 230; days a month, 888 = 0
#           normalized via [(101, 107)-100] * 52, [(201, 230)-200] * 12, 888 = 0.
# STRENGTH - 101-199; times per week, 201-299; times per month, 888 = 0
#           normalized via [(101, 199)-100] * 52, [(201,299)-200] * 12, 888 = 0
# FRUIT2 - 101-199; times per day, 201-299; times per week, 300; less than once a month, 301-399; times per month, 555 = never
#           normalized via [(101, 199)-100] * 365, [201, 299)-200] * 52, 300 = 12, [(301, 399) - 300] * 12, 555 = 0
# FRUITJU2 - Identical to FRUIT2
# FVGREEN1 - Identical to FRUIT2
# FRENCHF1 - Identical to FRUIT2
# POTATOE1 - Identical to FRUIT2
# VEGETAB2 - Identical to FRUIT2
# WTKG3, HTM4 - no normalization required, in natural units KG*100 and M*100 respectively

# Normalization functions
normalize_alcday5 <- function(x) {
  ifelse(x >= 101 & x <= 107, (x - 100) * 52,
         ifelse(x >= 201 & x <= 230, (x - 200) * 12, 
                ifelse(x == 888, 0, x)))
}

normalize_strength <- function(x) {
  ifelse(x >= 101 & x <= 199, (x - 100) * 52,
         ifelse(x >= 201 & x <= 299, (x - 200) * 12,
                ifelse(x == 888, 0, x)))
}

normalize_food <- function(x) {
  ifelse(x >= 101 & x <= 199, (x - 100) * 365,
         ifelse(x >= 201 & x <= 299, (x - 200) * 52,
                ifelse(x == 300, 12,
                       ifelse(x >= 301 & x <= 399, (x - 300) * 12, 
                              ifelse(x == 555, 0, x)))))
}

# Apply normalization
data <- data %>%
  mutate(
    ALCDAY5 = normalize_alcday5(ALCDAY5),
    STRENGTH = normalize_strength(STRENGTH),
    FRUIT2 = normalize_food(FRUIT2),
    FRUITJU2 = normalize_food(FRUITJU2),
    FVGREEN1 = normalize_food(FVGREEN1),
    FRENCHF1 = normalize_food(FRENCHF1),
    POTATOE1 = normalize_food(POTATOE1),
    VEGETAB2 = normalize_food(VEGETAB2)
  )

# replace NA for these variables with mean
numeric_variables = c('ALCDAY5', 'WTKG3', 'HTM4', 'STRENGTH', 'FRUIT2', 'FRUITJU2', 'FVGREEN1', 'FRENCHF1', 'POTATOE1', 'VEGETAB2')
data <- data %>%
  mutate(across(
    .cols = all_of(numeric_variables), 
    .fns = ~ if (is.numeric(.)) {
      mean_value <- mean(., na.rm = TRUE)
      replace(., is.na(.), mean_value)
    } else {
# Return the column unchanged if it's not numeric
    }
  ))

# Display NA counts after modificiation
na_counts_after_mean_imputation <- sapply(data, function(x) sum(is.na(x)))
print(na_counts_after_mean_imputation)

# Design numerical visualization
# Reshape data to long format for ggplot
data_long2 <- data %>%
  pivot_longer(cols = numeric_variables, names_to = "Variable", values_to = "Value")

# Have decided just to display two numerical variables as the displays for 10 
# were not scaling well. 
# Calculate the IQR limits for ALCDAY5 and POTATOE1
limits <- data_long2 %>%
  group_by(Variable) %>%
  summarize(
    IQR = IQR(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    LowerLimit = Q1 - 1.5 * IQR,
    UpperLimit = Q3 + 1.5 * IQR
  )

# Filter the data to exclude outliers
data_filtered <- data_long2 %>%
  left_join(limits, by = "Variable") %>%
  filter(Value >= LowerLimit & Value <= UpperLimit)

# Create separate boxplots for ALCDAY5 and POTATOE1
ggplot(data_filtered %>% filter(Variable == "ALCDAY5"), aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of ALCDAY5 (days drink alcohol per year)",
       x = "ALCDAY5",
       y = "Value") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggplot(data_filtered %>% filter(Variable == "POTATOE1"), aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of POTATOE1 (potato based meals per year)",
       x = "POTATOE1",
       y = "Value") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Convert factor columns back to numeric prior to balancing
for (col in variables_to_plot) {
  if (is.factor(data[[col]])) {
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
}

# Specify the file name for preprocessed data
file_name_preproc <- "preprocessed_data.csv"

# Write the dataframe to a CSV file
write_csv(data, file_name_preproc)

## Task 5. 
# Establish two balanced datasets. We select the simple strategies of 
# oversampling and undersampling the dataset.
# split dataset to establish test data and train data prior to balancing
set.seed(67)
data$Class = as.factor(data$Class)
split = initial_split(data, prop = 0.8, strata = Class)
train = training(split)
test = testing(split)

# Specify the file name for training data
file_path_train <- "training_data.csv"

# Write the dataframe to a CSV file
write_csv(train, file_path_train)

# Specify the file path for testing data
file_path_test <- "testing_data.csv"

# Write the dataframe to a CSV file
write_csv(test, file_path_test)

# testing the number of each class before balancing
table(train$Class)

# balancing data (train) (UNDERSAMPLING method is used)
number_of_tuples_under = 2 * sum(train$Class == 'Y')
data_balanced_under = ovun.sample(Class ~ ., data = train, 
                                  method = 'under', N = number_of_tuples_under)$data

# testing the number of each class after balancing
table(data_balanced_under$Class)

# over sample the minority class to establish a balanced dataset ((OVERSAMPLING method is used))
data_balanced_over <- ovun.sample(Class ~ ., data = train, 
                                  method = "over", N = 2*table(train$Class)[1])$data

# testing the number of each class after balancing
table(data_balanced_over$Class)

## Task 6 
## Apply attribute selection methods to balanced datasets
## Subtasks: Chi^2, Recursive Feature Elimination, Gain Ratio

## Subtask 6.1 utilize Chi^2 to select features for both balanced datasets
## chi-square test function
features_chi_sq <- function(data, p_value_threshold) {
# function to return features based on pairwise chi-square testwith defined p_value threshold
# We have decided to ignore the fact that many of our results utilize chi^2 outputs where more
# than 20% of the chi^2 have a value smaller than 5.
# the result of pairwise chi-square test stored in the data.frame
  chi_sq <- data.frame(Feature = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
# Compute chi-squared statistics for each feature
  for (feature in names(data)[names(data) != "Class"]) {
    tbl <- table(data[[feature]], data$Class)
    p_value <- suppressWarnings(chisq.test(tbl)$p.value)
    chi_sq <- rbind(chi_sq, data.frame(Feature = feature, P_Value = p_value))
  }
  
#filter based on significance and display resultant features
  significant_features <- chi_sq %>%
    filter(P_Value < p_value_threshold) %>%    
    arrange(P_Value)
  features_chi_sq <- significant_features$Feature
  return(features_chi_sq)
}

# Chi^2 attribute selection on data_balanced_over
features_chi_sq_over <- features_chi_sq(data_balanced_over, 10e-19) # threshold defined to select 24 variables
features_chi_sq_over
features_chi_sq_over_df <- data_balanced_over[, c(as.character(features_chi_sq_over), "Class")]

# Chi^2 attribute selection on data_balanced_under
features_chi_sq_under <- features_chi_sq(data_balanced_under, 0.01) # traditional threshold for a chi-squared test
features_chi_sq_under
features_chi_sq_under_df <- data_balanced_under[, c(as.character(features_chi_sq_under), "Class")]

## Subtask 6.2 utilize gain ratio to select features for both datasets
# function that returns features after gain.ration attribute selection method is applied

features_gain_ration <- function(data) {
# gain.ratio feature selection on data_balanced_over
  gain_ratio <- gain.ratio(Class ~., data = data)
  
# result is converted to data.frame
  gain_ratio <- cbind(rownames(gain_ratio), data.frame(gain_ratio, row.names=NULL))
  colnames(gain_ratio)[1] <- 'gain_ratio'
  
# sorted by importance in nonincreasing order 
  gain_ratio <- arrange(gain_ratio, desc(attr_importance))
  
# filtered only the variables with importance > 0
  gain_ratio <- gain_ratio[gain_ratio$attr_importance > 0,]
  
# selected features
  features_gain_ratio <- gain_ratio[,1]
  return(features_gain_ratio)
}

# gain.ratio feature selection on data_balanced_over
features_gain_ratio_over <- features_gain_ration(data_balanced_over)
features_gain_ratio_over
features_gain_ratio_over_df <- data_balanced_over[, c(as.character(features_gain_ratio_over), "Class")]

# gain.ratio feature selection on data_balanced_under
features_gain_ratio_under <- features_gain_ration(data_balanced_under)
features_gain_ratio_under
features_gain_ratio_under_df <- data_balanced_under[, c(as.character(features_gain_ratio_under), "Class")]

## Subtask 6.3 utilize recursive feature elimination (RFE) to select features 
## for both balanced datasets
# function that returns features after the rfe method (recursive feature selection) is applied
features_rfe <- function(data) {
  rfe_ctrl <- rfeControl(functions = rfFuncs,  method = "cv", 
                         number = 5, verbose = FALSE) 
  
# RFE attribute selection on data
  rfe <- rfe(x = data[, -ncol(data)], 
             y = data$Class, 
             sizes = seq(5, 30, by = 5),  # Evaluating subsets from 5 to 30 features in steps of 5 
             rfeControl = rfe_ctrl)
  
# Results demonstrate that all variables were kept, mean importance will be used to select attributes
  rfe_imp <- varImp(rfe, scale = FALSE)
  
# Convert importance to a data frame
  rfe_imp_df <- as.data.frame(rfe_imp)
  rfe_imp_df$Feature <- rownames(rfe_imp_df)
  rownames(rfe_imp_df) <- NULL
  
# Define a threshold for feature selection 
  rfe_threshold <- mean(rfe_imp_df$Overall)
  print(rfe_threshold)
  
# Select features with importance scores above the threshold
  features_rfe <- rfe_imp_df %>% filter(Overall > rfe_threshold) 
  features_rfe <-dplyr::select(features_rfe, Feature) %>% pull()
  
  return(features_rfe)
}

# Run the RFE algorithm on data_balanced_over
set.seed(400)
features_rfe_over <- features_rfe(data_balanced_over)
features_rfe_over
features_rfe_over_df <- data_balanced_over[, c(as.character(features_rfe_over), "Class")]

set.seed(42)
# Run the RFE algorithm on data_balanced_under
features_rfe_under <- features_rfe(data_balanced_under)
features_rfe_under
features_rfe_under_df <- data_balanced_under[, c(as.character(features_rfe_under), "Class")]

## Task 7 Define metrics table generation function
# == FUNCTION to fill table with metrics========================================
table_metrics <- function(model, test_data, model_name) {
  
# The function computes all required metrics and plugs them
# into data.frame. Result is written to excel file.
  
# empty data.frame
  table <- data.frame(row.names = c('Class_NO', 'Class_YES','Wt. Avg.'))
  colnames <- c("Model_name", "TPR", "FPR", "Precision", "Recall", "F-measure", "ROC", "MCC", "Kappa", "Min_requir", "Extra_credit")
  table[colnames] <- NA
  
# computing confusion matrix
  classes <- c('Y', 'N')
  test_pred <- predict(model, newdata = test_data)
  cm <- confusionMatrix(test_pred, test_data$Class)
  
# Ensure cm$table is a data frame
  cm_table <- as.data.frame.matrix(cm$table)
  
  
# computing metrics with respective to both classes
  for (class in c('Class_NO', 'Class_YES')) {
    if (class == 'Class_NO') {
      TP = as.numeric(cm$table[1,1])
      TN = as.numeric(cm$table[2,2])
      FP = as.numeric(cm$table[1,2])
      FN = as.numeric(cm$table[2,1])
    } else {
      TP = as.numeric(cm$table[2,2])
      TN = as.numeric(cm$table[1,1])
      FP = as.numeric(cm$table[2,1])
      FN = as.numeric(cm$table[1,2])
    }
    table[class, 'TPR'] = TP/(TP+FN)
    table[class, 'FPR'] = FP/(FP+TN)
    table[class, 'Precision'] = TP/(TP+FP)
    table[class, 'Recall'] = TP/(TP+FN)
    table[class, 'F-measure'] = 2*table[class,3]*table[class,4] / (table[class,3]+table[class,4])
    table[class, 'MCC'] = (TP*TN - FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    table[class, 'Kappa'] = cm$overall[2]
    
  }
# roc-score (auc)
  test_true <- ifelse(test_data$Class == 'Y',1,0)
  test_predict <- predict(model, newdata = test_data, type = 'prob')
  roc <- roc(test_true, test_predict$Y)
  table[,'ROC'] = auc(roc)
  
# weighted average
  w_0 = prop.table(table(test$Class))[1]
  w_1 = prop.table(table(test$Class))[2]
  for (col in colnames(table)) {
    table['Wt. Avg.', col] = w_0*table['Class_NO', col] + w_1*table['Class_YES', col]
  }
# rounding numeric values
  table <- round(table,3)
  
# fill in the model_name
  table[,1] <- model_name
  
# test if min reqirement is met
  if (table['Class_YES', 'TPR']>=0.7 & table[['Class_NO', 'TPR']]>=0.65) {
    table[,'Min_requir'] <- 'YES'
  } else {
    table[,'Min_requir'] <- 'NO'
  }
  
# test if extra credit reqirement is met
  if (table['Class_YES', 'TPR']>=0.75 & table[['Class_NO', 'TPR']]>=0.7) {
    table[,'Extra_credit'] <- 'YES'
  } else {
    table[,'Extra_credit'] <- 'NO'
  }
  
# all tables are saved in the 'Models' folder in your directory
  library(xlsx)
  new_path <- file.path(path, 'Models')
  
# Create the new folder if it doesn't exist
  if (!dir.exists(new_path)) {
    dir.create(new_path, recursive = TRUE)
    message("New folder created: ", new_path)
  } else {
    message("Folder already exists: ", new_path)
  }
  setwd(new_path)
  write.xlsx(table, file = paste('model_', model_name, '.xlsx'))
  
  return(table)
}
## Task 8 apply classification algorithms 
## Subtasks: Logistic Regression, Random Forest, Gradient Boosting Machine, K-Nearest Neighbors, Naive Bayes, XGBoost, and Multilayer Perception
## Subtask 8.1 utilize Logistic Regression to classify the target variable on all six datasets
## Case 8.1.1 Logistic Regression - chi^2- Over
for (i in 1) {
  lr_train_control <- trainControl(method = "cv", number = 5)
  lr_tune_grid <- expand.grid(.parameter = 1)
  
  lr_chi_sq_over_model <- caret::train(Class~., data = features_chi_sq_over_df,
                                 method = 'glm', family = binomial,
                                 trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_chi_sq_over_model # Run to see Model after initial execution
  lr_chi_sq_over_pred <- predict(lr_chi_sq_over_model, newdata = test)
  lr_chi_sq_over_cm <- caret::confusionMatrix(lr_chi_sq_over_pred, test$Class, positive = 'Y')
  lr_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name1 <- '8.1.1 Logistic Regression - chi^2- Over'
  print(table_metrics(lr_chi_sq_over_model, test, model_name1)) # Run to regenerate results metrics
}
## Case 8.1.2 Logistic Regression-chi^2-Under
for (i in 1) {
  lr_chi_sq_under_model <- train(Class~., data = features_chi_sq_under_df,
                    method = 'glm', family = binomial,
                    trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_chi_sq_under_model # Run to see Model after initial execution
  lr_chi_sq_under_pred <- predict(lr_chi_sq_under_model, newdata = test)
  lr_chi_sq_under_cm <- confusionMatrix(lr_chi_sq_under_pred, test$Class, positive = 'Y')
  lr_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name2 <- '8.1.2 Logistic Regression-chi^2-Under'
  print(table_metrics(lr_chi_sq_under_model, test, model_name2)) # Run to regenerate results metrics
}
## Case 8.1.3 Logistic Regression-RFE-Over
for (i in 1) {
  lr_rfe_over_model <- train(Class~.,  data = features_rfe_over_df,
                                method = 'glm', family = binomial,
                                trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_rfe_over_model # Run to see Model after initial execution
  lr_rfe_over_pred <- predict(lr_rfe_over_model, newdata = test)
  lr_rfe_over_cm <- confusionMatrix(lr_rfe_over_pred, test$Class, positive = 'Y')
  lr_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name3 <- '8.1.3 Logistic Regression-RFE-Over'
  print(table_metrics(lr_rfe_over_model, test, model_name3)) # Run to regenerate results metrics
}
## Case 8.1.4 Logistic Regression-RFE-Under
for (i in 1) {
  lr_rfe_under_model <- train(Class~., data = features_rfe_under_df,
                             method = 'glm', family = binomial,
                             trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_rfe_under_model # Run to see Model after initial execution
  lr_rfe_under_pred <- predict(lr_rfe_under_model, newdata = test)
  lr_rfe_under_cm <- confusionMatrix(lr_rfe_under_pred, test$Class, positive = 'Y')
  lr_rfe_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name4 <- '8.1.4 Logistic Regression-RFE-Under'
  print(table_metrics(lr_rfe_under_model, test, model_name4)) # Run to regenerate results metrics
}
## Case 8.1.5 Logistic Regression-Gain_Ratio-Over
for (i in 1) {
  lr_gain_ratio_over_model <- train(Class~., data = features_gain_ratio_over_df,
                                     method = 'glm', family = binomial,
                                     trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_gain_ratio_over_model # Run to see Model after initial execution
  lr_gain_ratio_over_pred <- predict(lr_gain_ratio_over_model, newdata = test)
  lr_gain_ratio_over_cm <- confusionMatrix(lr_gain_ratio_over_pred, test$Class, positive = 'Y')
  lr_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name5 <- '8.1.5 Logistic Regression-Gain_Ratio-Over'
  print(table_metrics(lr_gain_ratio_over_model, test, model_name5)) # Run to regenerate results metrics
}
## Case 8.1.6 Logistic Regression-Gain_Ratio-Under
for(i in 1) {
  lr_gain_ratio_under_model <- train(Class~., data = features_gain_ratio_under_df,
                              method = 'glm', family = binomial,
                              trControl = lr_train_control, tuneGrid = lr_tune_grid)
  
  lr_gain_ratio_under_model # Run to see Model after initial execution
  lr_gain_ratio_under_pred <- predict(lr_gain_ratio_under_model, newdata = test)
  lr_gain_ratio_under_cm <- confusionMatrix(lr_gain_ratio_under_pred, test$Class, positive = 'Y')
  lr_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name6 <- '8.1.6 Logistic Regression-Gain_Ratio-Under'
  print(table_metrics(lr_gain_ratio_under_model, test, model_name6)) # Run to regenerate results metrics
}
## Subtask 8.2 utilize Random Forest to classify the target variable on all six datasets
## Case 8.2.1 Random Forest-chi^2-Over
for(i in 1) {  
  rf_train_control <- trainControl(method = "cv", number = 5)
# tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
# The final value used for the model was mtry = 6.
  rf_chi_sq_over_tune_grid <- expand.grid(mtry = c(2, 6))
  
  set.seed(42)
  rf_chi_sq_over_model <- train(Class~., data = features_chi_sq_over_df,
                                 method = 'rf', trControl = rf_train_control,
                                 tuneGrid = rf_chi_sq_over_tune_grid, importance = T)
  
  rf_chi_sq_over_model # Run to see Model after initial execution
  rf_chi_sq_over_pred <- predict(rf_chi_sq_over_model, newdata = test)
  rf_chi_sq_over_cm <- confusionMatrix(rf_chi_sq_over_pred, test$Class, positive = 'Y')
  rf_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution

  model_name7 <- '8.2.1 Random Forest-chi^2-Over'
  print(table_metrics(rf_chi_sq_over_model, test, model_name7)) # Run to regenerate results metrics
}
## Case 8.2.2 Random Forest-chi^2-Under
for(i in 1) {
#  tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
# The final value used for the model was mtry = 2.
  rf_chi_sq_under_tune_grid <- expand.grid(mtry = c(2, 3))
  
  set.seed(42)
  rf_chi_sq_under_model <- train(Class~., data = features_chi_sq_under_df,
                    method = 'rf', trControl = rf_train_control,
                    tuneGrid = rf_chi_sq_under_tune_grid, importance = T)
  
  rf_chi_sq_under_model # Run to see Model after initial execution
  rf_chi_sq_under_pred <- predict(rf_chi_sq_under_model, newdata = test)
  rf_chi_sq_under_cm <- confusionMatrix(rf_chi_sq_under_pred, test$Class, positive = 'Y')
  rf_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name8 <- '8.2.2 Random Forest-chi^2-Under'
  print(table_metrics(rf_chi_sq_under_model, test, model_name8)) # Run to regenerate results metrics
}
## Case 8.2.3 Random Forest-RFE-Over
for(i in 1) {
# tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
# The final value used for the model was mtry = 3. 
  rf_rfe_over_tune_grid <- expand.grid(mtry = c(2, 3))
  
  set.seed(42)
  rf_rfe_over_model <- train(Class~., data = features_rfe_over_df,
                              method = 'rf', trControl = rf_train_control,
                              tuneGrid = rf_rfe_over_tune_grid, importance = T)
  
  rf_rfe_over_model # Run to see Model after initial execution
  rf_rfe_over_pred <- predict(rf_rfe_over_model, newdata = test)
  rf_rfe_over_cm <- confusionMatrix(rf_rfe_over_pred, test$Class, positive = 'Y')
  rf_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution

  model_name9 <- '8.2.3 Random Forest-RFE-Over'
  print(table_metrics(rf_rfe_over_model, test, model_name9)) # Run to regenerate results metrics
}
## Case 8.2.4 Random Forest-RFE-under
for(i in 1) {
# tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
# The final value used for the model was mtry = 4.
  rf_rfe_under_tune_grid <- expand.grid(mtry = c(3, 4))
  
  set.seed(42)
  rf_rfe_under_model <- train(Class~., data = features_rfe_under_df,
                                 method = 'rf', trControl = rf_train_control,
                                 tuneGrid = rf_rfe_under_tune_grid, importance = T)
  
  rf_rfe_under_model # Run to see Model after initial execution
  rf_rfe_under_pred <- predict(rf_rfe_under_model, newdata = test)
  rf_rfe_under_cm <- confusionMatrix(rf_rfe_under_pred, test$Class, positive = 'Y')
  rf_rfe_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name10 <- '8.2.4 Random Forest-RFE-under'
  print(table_metrics(rf_rfe_under_model, test, model_name10)) # Run to regenerate results metrics
}
## Case 8.2.5 Random Forest-Gain_Ratio_Over
for(i in 1) {
#  tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
#  The final value used for the model was mtry = 3.
  rf_gr_over_tune_grid <- expand.grid(mtry = c(3, 4)) # - switched to this for faster execution but mtry value changes.
#  The final value used for the model was mtry = 4.
  
  set.seed(42)
  rf_gain_ratio_over_model <- train(Class~., data = features_gain_ratio_over_df,
                             method = 'rf', trControl = rf_train_control,
                             tuneGrid = rf_gr_over_tune_grid, importance = T)
  
  rf_gain_ratio_over_model # Run to see Model after initial execution
  rf_gain_ratio_over_pred <- predict(rf_gain_ratio_over_model, newdata = test)
  rf_gain_ratio_over_cm <- confusionMatrix(rf_gain_ratio_over_pred, test$Class, positive = 'Y')
  rf_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution

  model_name11 <- '8.2.5 Random Forest-Gain_Ratio_Over'
  print(table_metrics(rf_gain_ratio_over_model, test, model_name11)) # Run to regenerate results metrics
}
## Case 8.2.6 Random Forest-Gain_Ratio_Under
for(i in 1) {
#  tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7))
#  The final value used for the model was mtry = 2.
   rf_gr_over_tune_grid <- expand.grid(mtry = c(2, 3))
  
  set.seed(42)
  rf_gain_ratio_under_model <- train(Class~., data = features_gain_ratio_under_df,
                                    method = 'rf', trControl = rf_train_control,
                                    tuneGrid = rf_gr_over_tune_grid, importance = T)
  
  rf_gain_ratio_under_model # Run to see Model after initial execution
  rf_gain_ratio_under_pred <- predict(rf_gain_ratio_under_model, newdata = test)
  rf_gain_ratio_under_cm <- confusionMatrix(rf_gain_ratio_under_pred, test$Class, positive = 'Y')
  rf_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution

  model_name12 <- '8.2.6 Random Forest-Gain_Ratio_Under'
  print(table_metrics(rf_gain_ratio_under_model, test, model_name12)) # Run to regenerate results metrics
}
## Subtask 8.3 utilize Gradient Boosting Machine (GBM) to classify the target variable on all six datasets
## Case 8.3.1 GBM-chi^2-Over
for (i in 1) {
  modelLookup('gbm')
  
  train_control <- trainControl(method = "cv", number = 5)
#  tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
#  The final values used for the model were n.trees = 150, interaction.depth = 5, shrinkage = 0.3 and n.minobsinnode = 10.
  tune_grid <- expand.grid(n.trees = c(1, 150), interaction.depth = c(5), shrinkage = c(0.3), n.minobsinnode = c(10))
  
  set.seed(190)
  gb_chi_sq_over_model <- train(Class~., data = features_chi_sq_over_df,
                                 method = "gbm", tuneGrid = tune_grid,
                                 verbose = F, trControl = train_control)
  
  gb_chi_sq_over_model # Run to see Model after initial execution
  gb_chi_sq_over_pred <- predict(gb_chi_sq_over_model, newdata = test)
  gb_chi_sq_over_cm <- confusionMatrix(gb_chi_sq_over_pred, test$Class, positive = 'Y')
  gb_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name13 <- '8.3.1 GBM-chi^2-Over'
  print(table_metrics(gb_chi_sq_over_model, test, model_name13)) # Run to regenerate results metrics
}
## Case 8.3.2 GBM-chi^2-Under
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
  
# tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
# The final values used for the model were n.trees = 150, interaction.depth = 1, shrinkage = 0.3 and n.minobsinnode = 10.
  tune_grid <- expand.grid(n.trees = c(1, 150), interaction.depth = c(1), shrinkage = c(0.3), n.minobsinnode = c(10))
  

  gb_chi_sq_under_model <- train(Class~., data = features_chi_sq_under_df,
                    method = "gbm", tuneGrid = tune_grid,
                    verbose = F, trControl = train_control)

  gb_chi_sq_under_model # Run to see Model after initial execution
  gb_chi_sq_under_pred <- predict(gb_chi_sq_under_model, newdata = test)
  gb_chi_sq_under_cm <- confusionMatrix(gb_chi_sq_under_pred, test$Class, positive = 'Y')
  gb_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name14 <- '8.3.2 GBM-chi^2-Under'
  print(table_metrics(gb_chi_sq_under_model, test, model_name14)) # Run to regenerate results metrics
}
## Case 8.3.3 GBM-RFE-Over
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
  
# tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
# The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.01 and n.minobsinnode = 10.
  tune_grid <- expand.grid(n.trees = c(150), interaction.depth = c(5), shrinkage = c(0.3), n.minobsinnode = c(10, 20))
  
  set.seed(190)
  gb_rfe_over_model <- train(Class~., data = features_rfe_over_df,
                              method = "gbm", tuneGrid = tune_grid,
                              verbose = F, trControl = train_control)
  
  gb_rfe_over_model # Run to see Model after initial execution
  gb_rfe_over_pred <- predict(gb_rfe_over_model, newdata = test)
  gb_rfe_over_cm <- confusionMatrix(gb_rfe_over_pred, test$Class, positive = 'Y')
  gb_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name15 <- '8.3.3 GBM-RFE-Over'
  print(table_metrics(gb_rfe_over_model, test, model_name15)) # Run to regenerate results metrics
}
## Case 8.3.4 GBM-RFE-Under
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
# tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
# The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.01 and n.minobsinnode = 10.
  tune_grid <- expand.grid(n.trees = c(150), interaction.depth = c(3), shrinkage = c(0.1), n.minobsinnode = c(10, 20))
  
  set.seed(190)
  gb_rfe_under_model <- train(Class~., data = features_rfe_under_df,
                                 method = "gbm", tuneGrid = tune_grid,
                                 verbose = F, trControl = train_control)
  
  gb_rfe_under_model # Run to see Model after initial execution
  gb_rfe_under_pred <- predict(gb_rfe_under_model, newdata = test)
  gb_rfe_under_cm <- confusionMatrix(gb_rfe_under_pred, test$Class, positive = 'Y')
  gb_rfe_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name16 <- '8.3.4 GBM-RFE-Under'
  print(table_metrics(gb_rfe_under_model, test, model_name16)) # Run to regenerate results metrics
}

## Case 8.3.5 GBM-Gain-Ratio Over
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
  
# tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
# The final values used for the model were n.trees = 150, interaction.depth = 5, shrinkage = 0.3 and n.minobsinnode = 10.
  tune_grid <- expand.grid(n.trees = c(1, 150), interaction.depth = c(5), shrinkage = c(0.3), n.minobsinnode = c(10))
  
  set.seed(190)
  gb_gain_ratio_over_model <- train(Class~., data = features_gain_ratio_over_df,
                             method = "gbm", tuneGrid = tune_grid,
                             verbose = F, trControl = train_control)
  
  gb_gain_ratio_over_model # Run to see Model after initial execution
  gb_gain_ratio_over_pred <- predict(gb_gain_ratio_over_model, newdata = test)
  gb_gain_ratio_over_cm <- confusionMatrix(gb_gain_ratio_over_pred, test$Class, positive = 'Y')
  gb_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name17 <- '8.3.5 GBM-Gain-Ratio Over'
  print(table_metrics(gb_gain_ratio_over_model, test, model_name17)) # Run to regenerate results metrics
}

## Case 8.3.6 GBM-Gain_Ratio-Under
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
  
# tune_grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = c(1, 3, 5), shrinkage = c(0.01, 0.1, 0.3), n.minobsinnode = c(10, 20))
# The final values used for the model were n.trees = 50, interaction.depth = 1, shrinkage = 0.3 and n.minobsinnode = 20.
  tune_grid <- expand.grid(n.trees = c(50), interaction.depth = c(1), shrinkage = c(0.3), n.minobsinnode = c(10, 20))
  
  set.seed(190)
  gb_gain_ratio_under_model <- train(Class~., data = features_gain_ratio_under_df,
                                    method = "gbm", tuneGrid = tune_grid,
                                    verbose = F, trControl = train_control)
  
  gb_gain_ratio_under_model # Run to see Model after initial execution
  gb_gain_ratio_under_pred <- predict(gb_gain_ratio_under_model, newdata = test)
  gb_gain_ratio_under_cm <- confusionMatrix(gb_gain_ratio_under_pred, test$Class, positive = 'Y')
  gb_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name18 <- '8.3.6 GBM-Gain_Ratio-Under'
  print(table_metrics(gb_gain_ratio_under_model, test, model_name18)) # Run to regenerate results metrics
}

## Subtask 8.4 utilize K-Nearest Neighbors (KNN) to classify the target variable on all six datasets
## Case 8.4.1 KNN-chi^2-Over
for (i in 1) {
  KNN_train_control <- trainControl(method = "cv", number = 5)
  
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 1.
  tune_grid <- expand.grid(k = c(1, 85))
  
  knn_chi_sq_over_model <- train(Class~., data = features_chi_sq_over_df,
                                 method = "knn", tuneGrid = tune_grid,
                                 preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_chi_sq_over_model # Run to see Model after initial execution
  knn_chi_sq_over_pred <- predict(knn_chi_sq_over_model, newdata = test)
  knn_chi_sq_over_cm <- confusionMatrix(knn_chi_sq_over_pred, test$Class, positive = 'Y')
  knn_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name19 <- '8.4.1 KNN-chi^2-Over'
  print(table_metrics(knn_chi_sq_over_model, test, model_name19)) # Run to regenerate results metrics 
}
## Case 8.4.2 KNN-chi^2-Under
for (i in 1) {
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 71.
  tune_grid <- expand.grid(k = c(1, 71))
  
  knn_chi_sq_under_model <- train(Class~., data = features_chi_sq_under_df,
                                  method = "knn", tuneGrid = tune_grid,
                                  preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_chi_sq_under_model # Run to see Model after initial execution
  knn_chi_sq_under_pred <- predict(knn_chi_sq_under_model, newdata = test)
  knn_chi_sq_under_cm <- confusionMatrix(knn_chi_sq_under_pred, test$Class, positive = 'Y')
  knn_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name20 <- '8.4.2 KNN-chi^2-Under'
  print(table_metrics(knn_chi_sq_under_model, test, model_name20)) # Run to regenerate results metrics
}

## Case 8.4.3 KNN-RFE-Over
for (i in 1) {
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 1.
  tune_grid <- expand.grid(k = c(1, 53))
  knn_rfe_over_model <- train(Class~., data = features_rfe_over_df,
                               method = "knn", tuneGrid = tune_grid,
                               preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_rfe_over_model # Run to see Model after initial execution
  knn_rfe_over_pred <- predict(knn_rfe_over_model, newdata = test)
  knn_rfe_over_cm <- confusionMatrix(knn_rfe_over_pred, test$Class, positive = 'Y')
  knn_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name21 <- '8.4.3 KNN-RFE-Over'
  print(table_metrics(knn_rfe_over_model, test, model_name21)) # Run to regenerate results metrics
}
## Case 8.4.4 KNN-RFE-Under
for (i in 1) {
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 53.
  tune_grid <- expand.grid(k = c(1, 53))
  
  knn_rfe_under_model <- train(Class~., data = features_rfe_under_df,
                                  method = "knn", tuneGrid = tune_grid,
                                  preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_rfe_under_model # Run to see Model after initial execution
  knn_rfe_under_pred <- predict(knn_rfe_under_model, newdata = test)
  knn_rfe_under_cm <- confusionMatrix(knn_rfe_under_pred, test$Class, positive = 'Y')
  knn_rfe_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name22 <- '8.4.4 KNN-RFE-Under'
  print(table_metrics(knn_rfe_under_model, test, model_name22)) # Run to regenerate results metrics
}
## Case 8.4.5 KNN-Gain_Ratio-Over
for (i in 1) {
  train_control <- trainControl(method = "cv", number = 5)
  
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 1.
  tune_grid <- expand.grid(k = c(1, 85))
  
  knn_gain_ratio_over_model <- train(Class~., data = features_gain_ratio_over_df,
                                      method = "knn", tuneGrid = tune_grid,
                                      preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_gain_ratio_over_model # Run to see Model after initial execution
  knn_gain_ratio_over_pred <- predict(knn_gain_ratio_over_model, newdata = test)
  knn_gain_ratio_over_cm <- confusionMatrix(knn_gain_ratio_over_pred, test$Class, positive = 'Y')
  knn_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name23 <- '8.4.5 KNN-Gain_Ratio-Over'
  print(table_metrics(knn_gain_ratio_over_model, test, model_name23)) # Run to regenerate results metrics
}
## Case 8.4.6 KNN-Gain_Ratio-Under
for (i in 1) {
  # tune_grid <- expand.grid(k = seq(1, 100, 2))
  # The final value used for the model was k = 85.
  tune_grid <- expand.grid(k = c(1, 85))
  
  knn_gain_ratio_under_model <- train(Class~., data = features_gain_ratio_under_df,
                               method = "knn", tuneGrid = tune_grid,
                               preProcess = c("center", "scale"), trControl = KNN_train_control)
  
  knn_gain_ratio_under_model # Run to see Model after initial execution
  knn_gain_ratio_under_pred <- predict(knn_gain_ratio_under_model, newdata = test)
  knn_gain_ratio_under_cm <- confusionMatrix(knn_gain_ratio_under_pred, test$Class, positive = 'Y')
  knn_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name24 <- '8.4.6 KNN-Gain_Ratio-Under'
  print(table_metrics(knn_gain_ratio_under_model, test, model_name24)) # Run to regenerate results metrics
}

## Subtask 8.5 utilize Naive Bayes to classify the target variable on all six datasets
## Case 8.5.1 Naive Bayes-chi^2-Over
for (i in 1) {
  #Define ctrl and grid parameters, these have not been heavily tuned, but accomplish 5-fold cross validation with reasonable nb settings. 
  nb_train_control <- trainControl(method = "cv", number = 5)
  nb_tune_grid <- expand.grid(fL = 0, usekernel = TRUE, adjust = 1)
  
  nb_chi_sq_over_model <- train(Class ~ ., data = features_chi_sq_over_df,
                    method = "nb", tuneGrid = nb_tune_grid,
                    preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_chi_sq_over_model # Run to see Model after initial execution
  nb_chi_sq_over_pred <- predict(nb_chi_sq_over_model, newdata = test)
  nb_chi_sq_over_cm <- confusionMatrix(nb_chi_sq_over_pred, test$Class, positive = 'Y')
  nb_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name25 <- '8.5.1 Naive Bayes-chi^2-Over'
  print(table_metrics(nb_chi_sq_over_model, test, model_name25)) # Run to regenerate results metrics
}
## Case 8.5.2 Naive Bayes-chi^2-Under
for (i in 1) {
  nb_chi_sq_under_model <- train(Class ~ ., data = features_chi_sq_under_df,
                                method = "nb", tuneGrid = nb_tune_grid,
                                preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_chi_sq_under_model # Run to see Model after initial execution
  nb_chi_sq_under_pred <- predict(nb_chi_sq_under_model, newdata = test)
  nb_chi_sq_under_cm <- confusionMatrix(nb_chi_sq_under_pred, test$Class, positive = 'Y')
  nb_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name26 <- '8.5.2 Naive Bayes-chi^2-Under'
  print(table_metrics(nb_chi_sq_under_model, test, model_name26))
}
## Case 8.5.3 Naive Bayes-RFE-Over
for (i in 1) {
  nb_rfe_over_model <- train(Class ~ ., data = features_rfe_over_df,
                                 method = "nb", tuneGrid = nb_tune_grid,
                                 preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_rfe_over_model # Run to see Model after initial execution
  nb_rfe_over_pred <- predict(nb_rfe_over_model, newdata = test)
  nb_rfe_over_cm <- confusionMatrix(nb_rfe_over_pred, test$Class, positive = 'Y')
  nb_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name27 <- '8.5.3 Naive Bayes-RFE-Over'
  print(table_metrics(nb_rfe_over_model, test, model_name27))
}
## Case 8.5.4 Naive Bayes-RFE-Under
for (i in 1) {
  nb_rfe_under_model <- train(Class ~ ., data = features_rfe_under_df,
                              method = "nb", tuneGrid = nb_tune_grid,
                              preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_rfe_under_model # Run to see Model after initial execution
  nb_rfe_under_pred <- predict(nb_rfe_under_model, newdata = test)
  nb_rfe_under_cm <- confusionMatrix(nb_rfe_under_pred, test$Class, positive = 'Y')
  print(nb_rfe_under_cm)
  
  model_name28 <- '8.5.4 Naive Bayes-RFE-Under'
  print(table_metrics(nb_rfe_under_model, test, model_name28))
}
## Case 8.5.5 Naive Bayes-Gain_Ratio-Over
for (i in 1) {
  # Custom tuned grid utilized to achieve extra credit performance. 
  nb_gr_over_tune_grid <- expand.grid(fL = 0, usekernel = TRUE, adjust = 2)
  nb_gain_ratio_over_model <- train(Class ~ ., data = features_gain_ratio_over_df,
                                    method = "nb", tuneGrid = nb_gr_over_tune_grid,
                                    preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_gain_ratio_over_model # Run to see Model after initial execution
  nb_gain_ratio_over_pred <- predict(nb_gain_ratio_over_model, newdata = test)
  nb_gain_ratio_over_cm <- confusionMatrix(nb_gain_ratio_over_pred, test$Class, positive = 'Y')
  nb_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name29 <- '8.5.5 Naive Bayes-Gain_Ratio-Over'
  print(table_metrics(nb_gain_ratio_over_model, test, model_name29))
}
## Case 8.5.6 Naive Bayes-Gain_Ratio-Under
for (i in 1) {
  nb_gain_ratio_under_model <- train(Class ~ ., data = features_gain_ratio_under_df,
                                     method = "nb", tuneGrid = nb_tune_grid,
                                     preProcess = c("center", "scale"), trControl = nb_train_control)
  
  nb_gain_ratio_under_model # Run to see Model after initial execution
  nb_gain_ratio_under_pred <- predict(nb_gain_ratio_under_model, newdata = test)
  nb_gain_ratio_under_cm <- confusionMatrix(nb_gain_ratio_under_pred, test$Class, positive = 'Y')
  nb_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name30 <- '8.5.6 Naive Bayes-Gain_Ratio-Under'
  print(table_metrics(nb_gain_ratio_under_model, test, model_name30)) # Run to regenerate results metrics
}
## Subtask 8.6 utilize XGBoost to classify the target variable on all six datasets
## Case 8.6.1 XGBoost-chi^2-Over
for (i in 31) {
# we utilize 10 fold cross validation and the default summary
  xgb_control = trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)
  
# xgbGrid <- expand.grid(nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
# The final values used for the model were nrounds = 300, max_depth = 3, eta = 0.3, gamma = 1, colsample_bytree = 1, min_child_weight = 1 and subsample = 0.7.
  xgbGrid <- expand.grid(nrounds = c(1, 300), eta = c(0.3), max_depth = c(3), gamma = c(1), colsample_bytree = 1, min_child_weight = c(1), subsample = c(0.7))
  test_features <- features_chi_sq_over_df[, -ncol(features_chi_sq_over_df)]
  xgb_chi_sq_over_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_chi_sq_over <- caret::train(x = features_chi_sq_over_df[, -ncol(features_chi_sq_over_df)],  y = features_chi_sq_over_df$Class,
                           method = "xgbTree", tuneGrid = xgbGrid,
                           verbose = FALSE, trControl = xgb_control)
  
  xgbModel_chi_sq_over # Run to see Model after initial execution
  xgb_chi_sq_over_pred <- predict(xgbModel_chi_sq_over, newdata = xgb_chi_sq_over_test)
  xgb_chi_sq_over_pred <- factor(xgb_chi_sq_over_pred, levels = levels(features_chi_sq_over_df$Class))
  xgb_chi_sq_over_test$Class <- factor(test$Class, levels = levels(features_chi_sq_over_df$Class))
  xgb_chi_sq_over_cm <- caret::confusionMatrix(xgb_chi_sq_over_pred, test$Class, positive = "Y")
  xgb_chi_sq_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name31 <- '8.6.1 XGBoost-chi^2-Over'
  print(table_metrics(xgbModel_chi_sq_over, xgb_chi_sq_over_test, model_name31))  
}
## Case 8.6.2 XGBoost-chi^2-Under
for (i in 82) {
#  xgbGrid <- expand.grid(nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
# The final values used for the model were nrounds = 100, max_depth = 3, eta = 0.05, gamma = 1, colsample_bytree = 1, min_child_weight = 3 and subsample = 0.7.
  
  xgbGrid <- expand.grid(nrounds = 100, eta = 0.05, max_depth = 3, gamma = 1, colsample_bytree = c(1), min_child_weight = 2, subsample = c(0.7, 1.0))
  test_features <- features_chi_sq_under_df[, -ncol(features_chi_sq_under_df)]
  xgb_chi_sq_under_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_chi_sq_under <- caret::train(x = features_chi_sq_under_df[, -ncol(features_chi_sq_under_df)], y = features_chi_sq_under_df$Class,
                                       method = "xgbTree", tuneGrid = xgbGrid,
                                       verbose = FALSE, trControl = xgb_control)
  
  xgbModel_chi_sq_under # Run to see Model after initial execution
  xgb_chi_sq_under_pred <- predict(xgbModel_chi_sq_under, newdata = xgb_chi_sq_under_test)
  xgb_chi_sq_under_pred <- factor(xgb_chi_sq_under_pred, levels = levels(features_chi_sq_under_df$Class))
  xgb_chi_sq_under_test$Class <- factor(test$Class, levels = levels(features_chi_sq_under_df$Class))
  xgb_chi_sq_under_cm <- caret::confusionMatrix(xgb_chi_sq_under_pred, test$Class, positive = "Y")
  xgb_chi_sq_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name32 <- '8.6.2 XGBoost-chi^2-Under'
  print(table_metrics(xgbModel_chi_sq_under, xgb_chi_sq_under_test, model_name32))
}
## Case 8.6.3 XGBoost-RFE-Over
for (i in 31) {
#  xgbGrid <- expand.grid(nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
# The final values used for the model were nrounds = 300, max_depth = 3, eta = 0.3, gamma = 1, colsample_bytree = 1, min_child_weight = 3 and subsample = 0.7.
  
  xgbGrid <- expand.grid(nrounds = c(1, 300), eta = c(0.3), max_depth = c(3), gamma = c(1), colsample_bytree = 1, min_child_weight = c(3), subsample = c(0.7))
  test_features <- features_rfe_over_df[, -ncol(features_rfe_over_df)]
  xgb_rfe_over_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_rfe_over <- caret::train(x = features_rfe_over_df[, -ncol(features_rfe_over_df)], y = features_rfe_over_df$Class,
                                        method = "xgbTree", tuneGrid = xgbGrid,
                                        verbose = FALSE, trControl = xgb_control)
  
  xgbModel_rfe_over # Run to see Model after initial execution
  xgb_rfe_over_pred <- predict(xgbModel_rfe_over, newdata = xgb_rfe_over_test)
  xgb_rfe_over_pred <- factor(xgb_rfe_over_pred, levels = levels(features_rfe_over_df$Class))
  xgb_rfe_over_test$Class <- factor(test$Class, levels = levels(features_rfe_over_df$Class))
  xgb_rfe_over_cm <- caret::confusionMatrix(xgb_rfe_over_pred, test$Class, positive = "Y")
  xgb_rfe_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name33 <- '8.6.3 XGBoost-RFE-Over'
  print(table_metrics(xgbModel_rfe_over, xgb_rfe_over_test, model_name33))
}
## Case 8.6.4 XGBoost-RFE-Under
for (i in 31) {
#  xgbGrid <- expand.grid(nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
# The final values used for the model were nrounds = 100, max_depth = 1, eta = 0.03, gamma = 2, colsample_bytree = 1, min_child_weight = 3 and subsample = 0.7.
  
  xgbGrid <- expand.grid(nrounds = c(150, 100), eta = c(0.5, 0.3), max_depth = c(1), gamma = c(2), colsample_bytree = 1, min_child_weight = c(3), subsample = c(0.7))
  test_features <- features_rfe_under_df[, -ncol(features_rfe_under_df)]
  xgb_rfe_under_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_rfe_under <- caret::train(x = features_rfe_under_df[, -ncol(features_rfe_under_df)], y = features_rfe_under_df$Class,
                                     method = "xgbTree", tuneGrid = xgbGrid,
                                     verbose = FALSE, trControl = xgb_control)
  
  xgbModel_rfe_under # Run to see Model after initial execution
  xgb_rfe_under_pred <- predict(xgbModel_rfe_under, newdata = xgb_rfe_under_test)
  xgb_rfe_under_pred <- factor(xgb_rfe_under_pred, levels = levels(features_rfe_under_df$Class))
  xgb_rfe_under_test$Class <- factor(test$Class, levels = levels(features_rfe_under_df$Class))
  xgb_rfe_under_cm <- caret::confusionMatrix(xgb_rfe_under_pred, test$Class, positive = "Y")
  xgb_rfe_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name34 <- '8.6.4 XGBoost-RFE-Under'
  print(table_metrics(xgbModel_rfe_under, xgb_rfe_under_test, model_name34))
}

## Case 8.6.5 XGBoost-Gain_Ratio-Over
for (i in 31) {
#  xgbGrid <- expand.grid( nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
# The final values used for the model were nrounds = 300, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 1, min_child_weight = 1 and subsample = 0.7.
  xgbGrid <- expand.grid(nrounds = c(1, 300), eta = c(0.3), max_depth = c(3), gamma = c(0), colsample_bytree = 1, min_child_weight = c(1), subsample = c(0.7))
  test_features <- features_gain_ratio_over_df[, -ncol(features_gain_ratio_over_df)]
  xgb_gain_ratio_over_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_gain_ratio_over <- caret::train(x = features_gain_ratio_over_df[, -ncol(features_gain_ratio_over_df)], y = features_gain_ratio_over_df$Class,
                                    method = "xgbTree", tuneGrid = xgbGrid,
                                    verbose = FALSE, trControl = xgb_control)
  
  xgbModel_gain_ratio_over # Run to see Model after initial execution
  xgb_gain_ratio_over_pred <- predict(xgbModel_gain_ratio_over, newdata = xgb_gain_ratio_over_test)
  xgb_gain_ratio_over_pred <- factor(xgb_gain_ratio_over_pred, levels = levels(features_gain_ratio_over_df$Class))
  xgb_gain_ratio_over_test$Class <- factor(test$Class, levels = levels(features_gain_ratio_over_df$Class))
  xgb_gain_ratio_over_cm <- caret::confusionMatrix(xgb_gain_ratio_over_pred, test$Class, positive = "Y")
  xgb_gain_ratio_over_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name35 <- '8.6.5 XGBoost-Gain_Ratio-Over'
  print(table_metrics(xgbModel_gain_ratio_over, xgb_gain_ratio_over_test, model_name35))
}
## Case 8.6.6 XGBoost-Gain_Ratio-Under
for (i in 1) {
#  xgbGrid <- expand.grid(nrounds = seq(from = 50, to = 300, by = 50), eta = c(0.05, 0.1, 0.3), max_depth = c(1, 2, 3), gamma = c(0, 1, 2), colsample_bytree = 1, min_child_weight = c(1, 3), subsample = c(0.5, 0.7))
#  The final values used for the model were nrounds = 100, max_depth = 3, eta = 0.05, gamma = 2, colsample_bytree = 1, min_child_weight = 3 and subsample = 0.5.
  xgbGrid <- expand.grid(nrounds = c(1, 100), eta = c(0.05), max_depth = c(3), gamma = c(2), colsample_bytree = 1, min_child_weight = c(3),subsample = c(0.5))
  test_features <- features_gain_ratio_under_df[, -ncol(features_gain_ratio_under_df)]
  xgb_gain_ratio_under_test <- test[, colnames(test_features), drop = FALSE]
  
  set.seed(i)
  xgbModel_gain_ratio_under <- caret::train(x = features_gain_ratio_under_df[, -ncol(features_gain_ratio_under_df)], y = features_gain_ratio_under_df$Class,
                                           method = "xgbTree", tuneGrid = xgbGrid,
                                           verbose = FALSE, trControl = xgb_control)
  
  xgbModel_gain_ratio_under # Run to see Model after initial execution
  xgb_gain_ratio_under_pred <- predict(xgbModel_gain_ratio_under, newdata = xgb_gain_ratio_under_test)
  xgb_gain_ratio_under_pred <- factor(xgb_gain_ratio_under_pred, levels = levels(features_gain_ratio_under_df$Class))
  xgb_gain_ratio_under_test$Class <- factor(test$Class, levels = levels(features_gain_ratio_under_df$Class))
  xgb_gain_ratio_under_cm <- caret::confusionMatrix(xgb_gain_ratio_under_pred, test$Class, positive = "Y")
  xgb_gain_ratio_under_cm # Run to see confusion matrix details beyond "table_metrics" after initial execution
  
  model_name36 <- '8.6.6 XGBoost-Gain_Ratio-Under'
  print(table_metrics(xgbModel_gain_ratio_under, xgb_gain_ratio_under_test, model_name36))
}
## Model Summaries - for post execution review. 
## Case 8.1.1 Logistic Regression - chi^2- Over
table_metrics(lr_chi_sq_over_model, test, model_name1) #

## Case 8.1.2 Logistic Regression-chi^2-Under
table_metrics(lr_chi_sq_under_model, test, model_name2) #

## Case 8.1.3 Logistic Regression-RFE-Over
table_metrics(lr_rfe_over_model, test, model_name3) # Minimum Requirement Model 1

## Case 8.1.4 Logistic Regression-RFE-Under
table_metrics(lr_rfe_under_model, test, model_name4) # Minimum Requirement Model 2

## Case 8.1.5 Logistic Regression-Gain_Ratio-Over
table_metrics(lr_gain_ratio_over_model, test, model_name5) # 

## Case 8.1.6 Logistic Regression-Gain_Ratio-Under
table_metrics(lr_gain_ratio_under_model, test, model_name6) # 

## Case 8.2.1 Random Forest-chi^2-Over
table_metrics(rf_chi_sq_over_model, test, model_name7) # 

## Case 8.2.2 Random Forest-chi^2-Under
table_metrics(rf_chi_sq_under_model, test, model_name8) # Minimum Requirement Model 3

## Case 8.2.3 Random Forest-RFE-Over
table_metrics(rf_rfe_over_model, test, model_name9) # 

## Case 8.2.4 Random Forest-RFE-under
table_metrics(rf_rfe_under_model, test, model_name10) # Minimum Requirement Model 4

## Case 8.2.5 Random Forest-Gain_Ratio_Over
table_metrics(rf_gain_ratio_over_model, test, model_name11) # 

## Case 8.2.6 Random Forest-Gain_Ratio_Under
table_metrics(rf_gain_ratio_under_model, test, model_name12) # Minimum Requirement Model 5

## Case 8.3.1 GBM-chi^2-Over
table_metrics(gb_chi_sq_over_model, test, model_name13) # 

## Case 8.3.2 GBM-chi^2-Under 
table_metrics(gb_chi_sq_under_model, test, model_name14) # 

## Case 8.3.3 GBM-RFE-Over
table_metrics(gb_rfe_over_model, test, model_name15) #

## Case 8.3.4 GBM-RFE-Under
table_metrics(gb_rfe_under_model, test, model_name16) # Minimum Requirement Model 6

## Case 8.3.5 GBM-Gain-Ratio Over
table_metrics(gb_gain_ratio_over_model, test, model_name17) # 

## Case 8.3.6 GBM-Gain_Ratio-Under
table_metrics(gb_gain_ratio_under_model, test, model_name18) # Minimum Requirement Model 7

## Case 8.4.1 KNN-chi^2-Over
table_metrics(knn_chi_sq_over_model, test, model_name19) #  

## Case 8.4.2 KNN-chi^2-Under
table_metrics(knn_chi_sq_under_model, test, model_name20) # 

## Case 8.4.3 KNN-RFE-Over
table_metrics(knn_rfe_over_model, test, model_name21) # 

## Case 8.4.4 KNN-RFE-Under
table_metrics(knn_rfe_under_model, test, model_name22) # Minimum Requirement Model 8

## Case 8.4.5 KNN-Gain_Ratio-Over
table_metrics(knn_gain_ratio_over_model, test, model_name23) # 

## Case 8.4.6 KNN-Gain_Ratio-Under
table_metrics(knn_gain_ratio_under_model, test, model_name24) # Minimum Requirement Model 9

## Case 8.5.1 Naive Bayes-chi^2-Over
table_metrics(nb_chi_sq_over_model, test, model_name25) # 

## Case 8.5.2 Naive Bayes-chi^2-Under
table_metrics(nb_chi_sq_under_model, test, model_name26) # 

## Case 8.5.3 Naive Bayes-RFE-Over
table_metrics(nb_rfe_over_model, test, model_name27) # 

## Case 8.5.4 Naive Bayes-RFE-Under
table_metrics(nb_rfe_under_model, test, model_name28) # Minimum Requirement model 10

## Case 8.5.5 Naive Bayes-Gain_Ratio-Over
table_metrics(nb_gain_ratio_over_model, test, model_name29) # Extra Credit Model 1

## Case 8.5.6 Naive Bayes-Gain_Ratio-Under
table_metrics(nb_gain_ratio_under_model, test, model_name30) # 

## XBGoost models required the subset of test variables utilized in the model. These test parameters are directly derived from the test variable. 
## Case 8.6.1 XGBoost-chi^2-Over
table_metrics(xgbModel_chi_sq_over, xgb_chi_sq_over_test, model_name31) #

## Case 8.6.2 XGBoost-chi^2-Under
table_metrics(xgbModel_chi_sq_under, xgb_chi_sq_under_test, model_name32) # Minimum Requirement Model 11

## Case 8.6.3 XGBoost-RFE-Over
table_metrics(xgbModel_rfe_over, xgb_rfe_over_test, model_name33) #

## Case 8.6.4 XGBoost-RFE-Under
table_metrics(xgbModel_rfe_under, xgb_rfe_under_test, model_name34) # Minimum Requirement Model 12

## Case 8.6.5 XGBoost-Gain_Ratio-Over
table_metrics(xgbModel_gain_ratio_over, xgb_gain_ratio_over_test, model_name35) #

## Case 8.6.6 XGBoost-Gain_Ratio-Under'
table_metrics(xgbModel_gain_ratio_under, xgb_gain_ratio_under_test, model_name36) # Minimum Requirement Model 13
