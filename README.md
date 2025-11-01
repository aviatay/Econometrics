# 📊 Advanced Econometrics Project

---

## 🧠 Overview

This project explores multiple econometric modeling approaches applied to diverse datasets.
Each section demonstrates a distinct statistical framework, including linear regression, quantile regression, panel data analysis, censored/truncated models, binary choice models, and time series forecasting.

---

## 🧩 Sections and Datasets

### 1. Linear Regression (Germany Real Estate Data)

* **Dataset:** 268,850 records of rental listings (Germany, 2018–2019).
* **Source:** [Immo Data on Kaggle](https://www.kaggle.com/datasets/corrieaar/apartment-rental-offers-in-germany)
* **Dependent variable:** `totalRent`
* **Independent variables:** housing characteristics (e.g., `livingSpace`, `baseRent`, `serviceCharge`, `heatingCosts`, etc.)
* **Preprocessing:**

  * Removed NA values → 3,181 valid observations.
  * Selected 17 numeric and 4 categorical predictors.
  * Log-transformation (`ln`) to improve normality.

**Model Comparison:**

| Model | Transformations | Variables | R²    | MSE    | RSE   |
| ----- | --------------- | --------- | ----- | ------ | ----- |
| 1     | Raw             | 5         | 0.881 | 0.0385 | 0.145 |
| 2     | log(x), log(y)  | 4         | 0.991 | 0.0048 | 0.018 |
| 3     | Raw (full)      | 6         | 0.882 | 0.0382 | 0.144 |
| 4     | log(x), log(y)  | 7         | 0.980 | 0.0037 | 0.014 |

✅ **Best model:** Model 4 (log–log specification)
✅ **Key predictors:** `log(serviceCharge)`, `log(baseRent)`, `log(noRooms)`, `log(heatingCosts)`
✅ **Post-FGLS correction:** solved heteroskedasticity (White test p=0.073)
✅ **Final R² = 0.99**

---

### 2. Quantile Regression

* **Purpose:** capture non-uniform effects of predictors across rental price distribution.
* **Quantiles tested:** τ = {0.1, 0.25, 0.5, 0.75, 0.9}
* **Findings:**

  * `log(baseRent)` and `log(serviceCharge)` have consistent positive influence across quantiles.
  * High quantiles (0.75–0.9) overestimate rents, reflecting high-end market segments.
  * `log(heatingCosts)` remains significant in all quantiles.

✅ **Insight:** Quantile regression uncovers heterogeneous effects missed by OLS — useful for identifying market asymmetry in rent determinants.

---

### 3. Panel Data (CAMP Asthma Clinical Study)

* **Dataset:** 9,907 observations from 692 children (2–18 clinic visits each).
* **Source:** [CAMP Asthma Dataset (NHLBI Study)](https://biolincc.nhlbi.nih.gov/studies/camp/)
* **Dependent variable:** `POSFEV` – post-bronchodilator lung function.
* **Predictors:** `PREFEV`, `PREFVCPP`, `visitc`, demographic covariates, and treatment groups.

**Analysis summary:**
A comparison between **Fixed Effects (FE)** and **Random Effects (RE)** models was conducted.
The **Hausman test (p < 2.2e-16)** indicated that the **Fixed Effects** model is preferred, confirming correlation between individual effects and covariates.
Although the RE model had a slightly higher adjusted R² (0.970 vs. 0.968), the FE specification was selected for consistency and unbiased estimation.

**Key findings:**

* `PREFEV` has a strong positive influence on `POSFEV` (β ≈ 0.9, p < 2.2e-16).
* `PREFVCPP` shows a small but significant negative effect (β ≈ –0.001, p = 0.00076).
* There is a clear improvement trend across visits (`visitc` effect positive).
* Clustered standard errors removed most heteroskedasticity.

---

### 4. Censored & Truncated Models

* **Context:** rental prices with extreme upper tail (using Tukey cutoff U=1,553.31 EUR).
* **Models:** Tobit vs. Truncated Regression
* **Comparison:**

| Model    | AIC       | BIC       | logLik     | Notes                   |
| -------- | --------- | --------- | ---------- | ----------------------- |
| Tobit    | 94,018.29 | 94,087.13 | -46,999.14 | Includes censored tails |
| TruncReg | 94,018.29 | 91,155.57 | -45,533.71 | Better fit              |

✅ **Preferred model:** Truncated Regression
✅ **Main predictors:** `livingSpaceRange`, `electricityBasePrice`, `noRoomsRange`
✅ **Interpretation:** Larger area and higher electricity price → higher rent.

---

### 5. Binary Dependent Variable: Balcony Presence

* **Dependent variable:** `balcony` (0/1)
* **Sample size:** 4,999 records
* **Approach:** Logit & Probit models, with ROC–AUC and cutoff optimization.
* **Best cutoff:** Logit=0.605, Probit=0.595

**Performance:**

| Metric      | Logit | Probit |
| ----------- | ----- | ------ |
| AUC         | 0.745 | 0.746  |
| Sensitivity | 69.6% | 70.6%  |
| Specificity | 68.7% | 67.4%  |
| Accuracy    | 69.3% | 69.5%  |

✅ **Key variables:** `serviceCharge`, `livingSpace`, `lift`, `yearConstructedRange`
✅ **Interaction:** `livingSpace × serviceCharge` improves AIC by –97 points.
✅ **Interpretation:** larger apartments with higher service charges → more likely to include balconies.

---

### 6. Time Series Analysis – Apple Stock

* **Dataset:** 1,254 daily closing prices (5 years)
* **Source:** [Apple Stock & Market Indexes (Yahoo Finance)](https://finance.yahoo.com/quote/AAPL/history/)
* **Variables:** `Apple`, `S&P500`, `NASDAQ` (from *yfinance*)
* **Methodology:** ADF test, differencing, ACF/PACF diagnostics, ADL model.

**Results:**

* All series non-stationary → differencing achieved stationarity.
* **Final model:** ΔApple_t = α + β₁ΔApple_{t-1} + β₂ΔS&P500_t + β₃ΔNASDAQ_t + ε_t
* **Findings:**

  * Positive autocorrelation lag(1) in Apple.
  * NASDAQ changes → strongest predictor of Apple returns.
  * Ljung–Box p=0.7693 → residuals ≈ white noise.

---

## 🧠 Key Takeaways

* **Model diversity:** demonstrated advanced econometric methods across multiple data domains.
* **Analytical insight:** understanding when to use log-transformation, FGLS, quantile regression, or Tobit/Probit models.
* **Result stability:** cross-validation, residual diagnostics, and robustness checks ensured model reliability.

---

## 🧭 Summary

> The project systematically compares econometric modeling techniques across linear, nonlinear, panel, and time-series frameworks — demonstrating both theoretical depth and applied insight into data-driven economic modeling.
