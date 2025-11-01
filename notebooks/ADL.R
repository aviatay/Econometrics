# Load necessary libraries
library(tidyverse)
library(lmtest)
library(tseries)
library(urca)

df = read.csv("C:/Users/97250/OneDrive - Technion/לימודים/סמסטר 10/אקונומטריקה/Econometrics Course/AAPL_with_SP500_NASDAQ_exogs.csv")
y = df$Close_AAPL
X1 = df$Close_SP500
X2 = df$Close_NASDAQ

# ACF/PACF
par(mfrow = c(1,2), mar = c(4,4,2,1))
acf(y)
pacf(y)

acf(X1)
pacf(X1)

acf(X2)
pacf(X2)
# Testing for stationary

### Augmented Dickey Fuller test ###

# The ADF test with "type = "none", tests for unit root. The Null hypothesis 
# H0: There is Unit root (Non - stationary series), H1: Stationary Series

summary(ur.df(X1, type = "none", lags = 3))  # urca package

# The p-value indicates that we reject The Null hypothesis i.e. There is no unit root and
# THE SERIES IS STATIONARY.

#####

# The ADF test with "type = "drift", tests for unit root and a drift. i.e. The Null hypothesis 
# H0: There is unit root and no drift at the same time, H1: Otherwise.

summary(ur.df(X1, type = "drift", lags = 3))  

# The p-value indicates that we reject the null hypothesis i.e. one of the following   
# happens: (I) There is no unit root, (II) There is a drift (III) Both (I) and (II) happen. 
# In the output you may observe two statistic values that correspond to the following two tests:
# (1) The first statistic is the value corresponding to the unit root test only
# (H0: There is Unit root (Non - stationary series), H1: Stationary Series). It should 
# be compared with tau2 critical table value. If the absolute value  of this statistic is larger
# than the absolute value of tau2, we reject the null hypothesis.
# (2) The second statistic is the value corresponding to the ADF test with a drift. It should 
# be compared with phi1 critical table value. If the absolute value  of this statistic is larger
# than the absolute value of phi1, we reject the null hypothesis.
# Since rejecting the null hypothesis in (2) may lead to 3 different conclusions (I,II,III).
# We should observe the regression output and conclude according to the significant coefficients.
# In this example, we conclude that we should reject both hypothesis, and the regression
# output yields that there is a significant lag of X1.

#####

# The ADF test with "type = "trend", tests for unit root, drift and time trend. 
# i.e. The Null hypothesis: 
# H0: There is unit root, no drift and no time trend at the same time, H1: Otherwise.

summary(ur.df(X1, type = "trend", lags = 3))  

# This output shows the results of  three tests:
# The first two tests are similar to the previous output, the third test is for unit root, drift
# and time trend simultaneously. The critical values are: (1) tau3 for the unit root test only, 
# (2) Phi2 for the unit root and a drift test, (3) phi3 for the unit root, drift and a time trend test.
# Obviously, this output leads to the same conclusions as explained in the previous test.

summary(ur.df(X2, type = "trend", lags = 3))

# The ADF test shows that the series X2 is stationary.

summary(ur.df(y, type = "trend", lags = 3))  # urca package

# The ADF test shows that the series Y is Non stationary.

# First difference X1

diff_X1 = diff(X1)
summary(ur.df(diff_X1, type = "trend", lags = 3)) 

diff_X2 = diff(X2)
summary(ur.df(diff_X2, type = "trend", lags = 3)) 

diff_y = diff(y)
summary(ur.df(diff_y, type = "trend", lags = 3)) 

# ACF/PACF
par(mfrow = c(1,2), mar = c(4,4,2,1))
acf(diff_y)
pacf(diff_y)

acf(diff_X1)
pacf(diff_X1)

acf(diff_X2)
pacf(diff_X2)

data = data.frame(diff_y, diff_X1, diff_X2)

# Plotting the data

plot(diff_X1, type = "l")
plot(diff_X2, type = "l")
plot(diff_y, type = "l")

# Estimate the ARDL model

model = lm(diff_y ~ lag(diff_y, 1) + lag(diff_y, 2) + diff_X1 + lag(diff_X1, 1) + lag(diff_X1, 2) + diff_X2 + lag(diff_X2, 1)  + lag(diff_X2, 2), data = data)

summary(model)

model = lm(diff_y ~ lag(diff_y, 1) + lag(diff_y, 2) + diff_X1 + lag(diff_X1, 1) + lag(diff_X1, 2) + diff_X2 + lag(diff_X2, 1), data = data)

summary(model)

model = lm(diff_y ~ lag(diff_y, 1) + lag(diff_y, 2) + diff_X1 + lag(diff_X1, 1) + lag(diff_X1, 2) + diff_X2, data = data)

summary(model)

model = lm(diff_y ~ lag(diff_y, 1) + diff_X1 + lag(diff_X1, 1) + lag(diff_X1, 2) + diff_X2, data = data)

summary(model)

# Ljung-Box test for white noise

ljung_test = Box.test(model$residuals, lag = 20, type = "Ljung-Box")
cat("Ljung-Box test p-value:", ljung_test$p.value, "\n")

# Meaning that the residual is a white noise.





