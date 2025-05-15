# Project Overview

This project focuses on **predictive modeling using a 2019 Behavioral Risk Factor Surveillance System (BRFSS) Survey Data**. The goal is to preprocess complex health survey data, balance the dataset, select optimal features, and apply a suite of machine learning classification models. Models are evaluated and compared using various performance metrics.
Performance criteria
o Minimum requirement
 class Y TPR >= 70% AND class N TPR >= 65%
o If the minimum requirement is not met, 10% will be deducted.
o If performance satisfies the following criterion, extra credit of 20% will be given.
 class Y TPR >= 75% AND class N TPR >= 70%
o If the performance of the model of a team cannot be replicated:
 20% will be deducted.
 If the team received any extra credit, the extra credit is revoked (in addition
to 20% deduction) 

## Workflow Summary

### 1. Data Preprocessing

* Loaded BRFSS survey data.
* Removed irrelevant or duplicate variables.
* Replaced survey-specific missing codes (e.g., 7, 9, 77, 99, 777, 999) with `NA`.
* Imputed missing values using **mode for categorical variables** and **mean for numeric variables**.
* Normalized numeric variables (e.g., alcohol consumption, fruit intake) to annual frequencies.

### 2. Exploratory Data Analysis (EDA)

* Generated visualizations for categorical and numeric variables.
* Addressed outliers and transformed data for consistent scaling.

### 3. Data Balancing

* Split the dataset into **training** (80%) and **testing** (20%) sets.
* Created balanced versions using:

  * **Oversampling**
  * **Undersampling**

### 4. Feature Selection

Applied 3 feature selection methods:

* **Chi-square test**
* **Gain Ratio**
* **Recursive Feature Elimination (RFE)**

Applied these to both oversampled and undersampled datasets.

### 5. Model Training & Evaluation

Trained the following classifiers on each balanced dataset:

* Logistic Regression
* Random Forest
* Gradient Boosting Machine (GBM)
* K-Nearest Neighbors (KNN)
* Naive Bayes
* XGBoost
* Multilayer Perceptron (MLP)

### 6. Model Evaluation

* Generated confusion matrices and ROC curves.
* Computed metrics: TPR, FPR, Precision, Recall, F1, MCC, Kappa, and AUC.
* Assessed models against **minimum requirements** and **extra credit thresholds**.

### 7. Outputs

* Preprocessed data, balanced datasets, and feature-selected datasets saved as CSV.
* Metrics saved as Excel files per model in the `Models` directory.

---

## Project Structure

```
├── CS699 - Project Code Final+.R   # Main R project code
├── project_data.csv                # Raw dataset (expected)
├── preprocessed_data.csv           # Preprocessed dataset
├── training_data.csv               # Training data
├── testing_data.csv                # Testing data
└── Models/                         # Folder containing model metrics Excel files
```

---

## Requirements

* **R (>= 4.0.0)**
* Required packages:

  * `dplyr`
  * `tidyr`
  * `ggplot2`
  * `readr`
  * `rsample`
  * `ROSE`
  * `randomForest`
  * `FSelector`
  * `fastDummies`
  * `gbm`
  * `pROC`
  * `e1071`
  * `klaR`
  * `RSNNS`
  * `caret`
  * `xlsx`

---

## Usage

1. Place the raw dataset `project_data.csv` in the working directory.
2. Run the script `CS699 - Project Code Final+.R`.
3. Outputs will be saved in the specified files and folders.
4. Review model evaluation metrics in the `Models/` directory.

---

## Notes

* **Tuning of models** was performed using cross-validation.
* **Data balancing and feature selection** were tested across multiple strategies to ensure robustness.
* **All metrics and results are reproducible** by rerunning the script.

---

Would you also like me to prepare a **requirements file (`requirements.R`) for the necessary packages** to automate installation?
If yes, just say "**Yes, requirements.**".
