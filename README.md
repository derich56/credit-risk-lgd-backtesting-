# 📊 Credit Risk LGD Model Selection
This project demonstrates how stability and interpretability can outweigh raw accuracy in real-world credit risk modeling.

---

## 📌 Overview
This project develops and evaluates multiple models to estimate **Loss Given Default (LGD)**.

The goal is not only to achieve high predictive performance, but to select a model that is **stable, interpretable, and suitable for real-world credit risk applications**.

---

## 📂 Dataset

This project uses publicly available loan data from Kaggle:

- 📌 Dataset: Loan Data for Credit Risk Modeling  
- 🔗 https://www.kaggle.com/datasets/shawnysun/loan-data-for-credit-risk-modeling  

The dataset includes borrower characteristics, loan details, repayment behavior, and recovery information, which are used to construct the **Loss Given Default (LGD)** target. In addition to the original source on Kaggle, the dataset is also included in this repository in ZIP format. This allows users to easily access and reproduce the analysis without needing to download the data separately from Kaggle.

### 📌 Data Usage
- File used: `loan_data_defaults.csv`
- LGD derived using discounted recovery cash flows
- Feature engineering applied (e.g., utilization, zip grouping)

### ⚠️ Disclaimer
- This dataset is publicly available on Kaggle  
- Used for educational and modeling purposes only  

---



## ⚙️ Project Workflow
![image alt](https://github.com/derich56/credit-risk-lgd-backtesting/blob/6307d951baea8b9143e10ee6b71f9fdedfbe77f7/Project_Workflow.png)
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
- **WOE Logistic Regression** → interpretable, credit-risk standard  
- **LightGBM** → machine learning approach  

---

### 4. Model Validation

#### A. Backtesting Framework
* **Time-based train-test split**
* **Out-of-Time (OOT) validation**
* **Shuffle validation**

#### B. Model Evaluation Metrics
* **Accuracy:** RMSE, MAE, MAPE
* **Discrimination:** ROC AUC, KS, Gini
* **Stability & Risk:** Bias, Error distribution, Population Stability Index (PSI), P95 tail risk

#### C. Calibration
* **Bias Correction**
* **LGD Alignment**

---

### 5. Insights & Business Action
* Actionable Recommendations
* Optimized Strategy
* Continuous Improvement
* Data-Driven Impact

---

## 🏆 Final Model Selection
 The table below summarizes model performance after data cleaning and out-of-time (OOT) validation.
### 📊 Model Performance Summary
| Metric          | Linear Regression | WOE Logistic Regression | LightGBM |
|----------------|------------------|----------------|----------|
| **RMSE (OOT)** | 0.0771           | **0.0762 ✅**  | 0.0785   |
| **MAE (OOT)**  | 0.0589           | **0.0583 ✅**  | 0.0598   |
| **MAPE (OOT)** | 7.46%            | **7.39% ✅**   | 7.57%    |
| **AUC (OOT)**  | 0.667            | **0.675 ✅**   | 0.668    |
| **Gini (Test)**| 0.3355           | **0.3519 ✅**  | 0.3360   |
| **KS Statistic**| 0.2577          | 0.2595         | **0.2628** |
| **PSI**        | 0.1836 ⚠️        | **0.1132 ✅**  | 0.1787 ⚠️ |

---

### ✅ Selected Model: **WOE Logistic Regression**

After evaluating predictive performance, discriminatory power, and model stability, WOE Logistic Regression was identified as the most suitable model for LGD estimation.

The model consistently outperformed the alternatives across key Out-of-Time (OOT) validation metrics, including RMSE, MAE, MAPE, AUC, and Gini. In addition, it achieved the lowest PSI value, indicating stronger robustness against population shifts and better long-term reliability.

Given its combination of predictive strength, stability, and interpretability, WOE Logistic Regression was selected as the final model for deployment and business decision-making.

# 📊 Credit Risk LGD Analysis & Business Insights

## 📊 Key Takeaways

- The portfolio shows a **high Loss Given Default (~68.9%)**, indicating significant loss severity.  
- The **recovery rate is low (~31.1%)**, meaning most defaulted exposure is not recovered.  
- From total exposure of **111 million**, approximately **77 million is expected to be lost**.  
- Losses are concentrated in **mid-to-high credit grades (C–E)**.  
- **Debt consolidation loans dominate loss contribution**, making them a major risk driver.  
- LGD increases consistently across credit grades, confirming strong **risk differentiation**.  
- The portfolio is heavily concentrated in **high-risk segments (~99%)**, indicating concentration risk.  
- A small number of **grade-purpose combinations drive a disproportionate share of losses**.  

---

## 📊 Business Summary

The portfolio is exposed to **substantial credit risk**, driven by high loss severity and weak recovery performance.

With expected losses approaching **70% of total exposure**, the portfolio is highly sensitive to default events.

## Key Implications:

- **High Loss Severity**  
  Defaults result in significant unrecovered exposure  

- **Low Recovery Efficiency**  
  Recovery processes are not sufficient to offset losses  

- **Risk Concentration**  
  Losses are concentrated in specific credit grades and loan purposes  

- **Portfolio Imbalance**  
  Overexposure to high-risk segments reduces resilience  

Overall, the portfolio requires **strategic adjustments** to improve profitability and reduce risk.

---

## 💼 Business Recommendations

### 1. Strengthen Risk-Based Pricing
- Adjust pricing for high LGD segments  
- Align returns with risk exposure  
- Integrate LGD into pricing frameworks  

---

### 2. Tighten Credit Underwriting
- Apply stricter criteria for:
  - Grades C–E  
  - High-risk loan purposes  
- Strengthen income and DTI validation  
- Limit exposure for high-risk borrowers  

---

### 3. Enhance Recovery Strategy
- Improve post-default collection processes  
- Focus on high-exposure accounts  
- Use analytics to optimize recovery prioritization  

---

### 4. Reduce Portfolio Concentration Risk
- Rebalance toward lower-risk segments (A–B)  
- Diversify loan purposes  
- Limit concentration in high-loss categories  

---

### 5. Implement Ongoing Risk Monitoring
- Track LGD and expected loss by segment  
- Build early warning indicators  
- Monitor portfolio risk trends  

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
- scorecard (WOE Logistic Regression)
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
