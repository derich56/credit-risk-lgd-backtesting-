# 📊 Credit Risk LGD Model Selection

> LGD modeling project focused on backtesting, stability analysis, and business-driven model selection

---

## 📌 Overview
This project develops and evaluates multiple models to estimate **Loss Given Default (LGD)**.

The goal is not only to achieve high predictive performance, but to select a model that is **stable, interpretable, and suitable for real-world credit risk applications**.

---

## 📂 Dataset

This project uses publicly available loan data from Kaggle:

- 📌 Dataset: Loan Data for Credit Risk Modeling  
- 🔗 https://www.kaggle.com/datasets/shawnysun/loan-data-for-credit-risk-modeling  

The dataset includes borrower characteristics, loan details, repayment behavior, and recovery information, which are used to construct the **Loss Given Default (LGD)** target.

### 📌 Data Usage
- File used: `loan_data_defaults.csv`
- LGD derived using discounted recovery cash flows
- Feature engineering applied (e.g., utilization, zip grouping)

### ⚠️ Disclaimer
- This dataset is publicly available on Kaggle  
- Used for educational and modeling purposes only  

---

## ⚙️ Project Workflow

### 1. Data Preparation
- Construct LGD using discounted recovery cash flows
- Clean and preprocess data
- Feature engineering (e.g. utilization, zip grouping)

---

### 2. Exploratory Data Analysis (EDA)
- Analyze distributions and relationships
- Identify key drivers of LGD
- Detect missing values and outliers

---

### 3. Model Development
Three models were implemented:

- **Linear Regression** → baseline model  
- **WOE Regression** → interpretable, credit-risk standard  
- **LightGBM** → machine learning approach  

---

### 4. Backtesting Framework
- Time-based train-test split
- Out-of-Time (OOT) validation
- Shuffle validation

---

### 5. Model Evaluation

Models were evaluated on:

- **Accuracy**
  - RMSE / MAE / MAPE  

- **Discrimination**
  - ROC AUC, KS, Gini  

- **Stability & Risk**
  - Bias
  - Error distribution
  - PSI (Population Stability Index)
  - Tail risk (P95 error)

---

### 6. Calibration
- Bias correction applied to improve prediction accuracy
- Ensures alignment between predicted and actual LGD

---

## 🏆 Final Model Selection

### ✅ Selected Model: **WOE Regression**

Although LightGBM achieved higher predictive performance, the WOE model was selected due to its:

- Superior **stability**
- Better performance under data shifts (**PSI**)
- Strong consistency in **Out-of-Time (OOT)** testing
- High **interpretability**

> In credit risk modeling, stability and interpretability are prioritized over marginal gains in accuracy.

---

## 📈 Business Impact
- Provides reliable LGD estimates
- Supports:
  - Risk-based pricing
  - Credit decision-making
  - Capital allocation
- Reduces model risk under changing economic conditions

---

## 🛠️ Tools & Technologies
- R
- data.table, dplyr
- scorecard (WOE)
- lightgbm
- ggplot2

---

## ⚠️ Notes
- Dataset is available via Kaggle (link above)
- Update file path before running the script
- Project is intended for learning and demonstration purposes

---

## 👤 Author
Dylan Richard
