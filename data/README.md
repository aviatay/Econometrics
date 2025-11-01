# 📂 Data Files

This folder contains both the **raw** and **processed** datasets used in the Advanced Econometrics project.

---

## 🌐 Data Sources

- **Real Estate Data (Germany)** — [Download from Kaggle](https://www.kaggle.com/datasets/corrieaar/apartment-rental-offers-in-germany)  
  Used in Sections 1, 2, 4, and 5 (Linear, Quantile, Tobit/TruncReg, Logit/Probit).  
  Includes rental listings with variables such as `totalRent`, `baseRent`, `livingSpace`, and `heatingCosts`.

- **CAMP Asthma Clinical Data** — [CAMP Study (NHLBI Biologic Specimen and Data Repository)](https://biolincc.nhlbi.nih.gov/studies/camp/)  
  Used in Section 3 (Panel Data).  
  Contains longitudinal clinical measurements for children with asthma, including `POSFEV`, `PREFEV`, and demographic variables.

- **Financial Market Data (Apple, NASDAQ, S&P500)** — [Yahoo Finance API](https://finance.yahoo.com/quote/AAPL/history/)  
  Used in Section 6 (Time Series).  
  Daily adjusted closing prices retrieved via `yfinance`.

---

## 📊 Files

- `AAPL_with_SP500_NASDAQ_exogs.csv` — Stock and index data (Apple, NASDAQ, S&P500) used for time series modeling.

---

## 🧭 Notes

- The **raw data** for each dataset is hosted externally due to licensing and file size limitations.  
- All CSVs in this folder are **preprocessed subsets** suitable for replication and analysis.  

---
