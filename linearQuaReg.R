install.packages("MASS")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("fitdistrplus")
install.packages("aods3")
install.packages("tseries")
install.packages("crayon")
install.packages("nlme")
install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(ggplot2)
library(tseries)
library(car)
library(MASS)
library(corrplot)
library(PerformanceAnalytics)
library(fitdistrplus)
library(aods3)
library(dplyr)
library(crayon)
library(nlme)     # For the gls function
library(ggplot2)
library(lmtest)

### Import Data ###
rent = read.csv("C:/Users/97250/OneDrive - Technion/לימודים/סמסטר 10/אקונומטריקה/Econometrics Course/immo_data.csv")
#rent = rent[c("electricityBasePrice", "thermalChar", "noRooms", "baseRentRange", "livingSpace", "yearConstructedRange", "telekomUploadSpeed", "pricetrend", "serviceCharge", "noParkSpaces", "baseRent", "totalRent", "balcony")]
rent = rent[c("serviceCharge", "picturecount", "pricetrend", "telekomUploadSpeed", "yearConstructedRange", "baseRent", "livingSpace", "baseRentRange", "noRooms", "thermalChar", "floor", "numberOfFloors", "noRoomsRange", "livingSpaceRange", "heatingCosts", "electricityBasePrice", "electricityKwhPrice","totalRent", "balcony", "hasKitchen", "lift", "garden")]

clean_rent = na.omit(rent)
clean_rent = clean_rent[apply(clean_rent[, !(names(clean_rent) %in% c("balcony", "hasKitchen", "lift", "garden"))], 1, function(row) all(row > 0)), ]

### Correlation Matrix ###

par(mfrow=c(1,1))

# Pearson correlation

clean_rent.cor = cor(clean_rent)

corrplot(clean_rent.cor, type = "upper", method = "number", number.cex = 0.6, tl.cex = 0.8, mar = c(1, 1, 1, 1))
#corrplot(clean_rent.cor, type = "full", method = "number")


# Spearman correlation

clean_rent.cor_S = cor(clean_rent, method = "spearman")

corrplot(clean_rent.cor_S, type = "upper", method = "number", number.cex = 0.6, tl.cex = 0.8, mar = c(1, 1, 1, 1))
#corrplot(clean_rent.cor_S, type = "full", method = "number")

# Graphs #
names(clean_rent)
clean_rent_sub = clean_rent[,c(1, 2, 3, 6, 7, 9, 15, 18)]

chart.Correlation(clean_rent_sub, histogram = T, method = "pearson")

# Distribution of totalRent #

hist(clean_rent_sub$totalRent, breaks = 100)
qqnorm(clean_rent_sub$totalRent)
qqline(clean_rent_sub$totalRent)

d = descdist((clean_rent$totalRent))
d$skewness
d$kurtosis

jarque.bera.test(sqrt(clean_rent$totalRent))

# ln() transformation

clean_rent$totalRent = log(clean_rent$totalRent)

jarque.bera.test(clean_rent$totalRent)

descdist(clean_rent$totalRent)

# Box - Cox Transformatin

reg_bc = boxcox(totalRent ~ . -balcony -hasKitchen -lift -garden , data = clean_rent)
lambda = reg_bc$x[which.max(reg_bc$y)]

# Linear model with the Box - Cox transformation

y_bc = ((clean_rent$totalRent^lambda-1)/lambda)

descdist(y_bc)

### Train and Test ###

set.seed(125)
train.size = floor(nrow(clean_rent_sub)*0.7)
train.index = sample(1:nrow(clean_rent_sub),train.size, replace = F)
train.set = clean_rent_sub[train.index,]
test.set = clean_rent_sub[-train.index,]

### Linear Regression ###
## model 1 ##
clean_rent_sub_reg = lm(totalRent ~ . , train.set)

s = summary(clean_rent_sub_reg)
par(mfrow=c(2,2))
plot(clean_rent_sub_reg)

#remove row
train.set$original_index <- as.integer(rownames(train.set))
rows_to_remove = c(176648, 4186)
train.set2 <- train.set[!train.set$original_index %in% rows_to_remove, ]
train.set$original_index <- NULL
train.set2$original_index <- NULL

clean_rent_sub_filt_reg = lm(totalRent ~ . , train.set2)
s1 = summary(clean_rent_sub_filt_reg)
plot(clean_rent_sub_filt_reg)

# P- values
s1$coefficients[,4]  

# Dropping insignificant explanatory variables
sig1 = s1$coefficients[,4]<0.05
names(sig1[sig1==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig1[sig1==TRUE]))

rent_sig1 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig1)

train.set_sig1 = rent_sig1[train.index,]
test.set_sig1 = rent_sig1[-train.index,]

clean_rent_sub_reg1 = lm(totalRent ~ . , train.set_sig1)
s1 = summary(clean_rent_sub_reg1)
plot(clean_rent_sub_reg1)

#remove row
train.set_sig1$original_index <- as.integer(rownames(train.set_sig1))
rows_to_remove = c(176648, 261125)#, 71461, 145571)
train.set_sig1 <- train.set_sig1[!train.set_sig1$original_index %in% rows_to_remove, ]
train.set_sig1$original_index <- NULL

clean_rent_sub_filt_reg1 = lm(totalRent ~ . , train.set_sig1)
s1 = summary(clean_rent_sub_filt_reg1)
plot(clean_rent_sub_filt_reg1)

# Dropping insignificant explanatory variables
sig1 = s1$coefficients[,4]<0.05
names(sig1[sig1==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig1[sig1==TRUE]))

rent_sig1 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig1)

train.set_sig1 = rent_sig1[train.index,]
test.set_sig1 = rent_sig1[-train.index,]

clean_rent_sub_reg1 = lm(totalRent ~ . , train.set_sig1)
s1 = summary(clean_rent_sub_reg1)
plot(clean_rent_sub_reg1)

#remove row
train.set_sig1$original_index <- as.integer(rownames(train.set_sig1))
rows_to_remove = c(176648, 261125)#, 71461, 145571)
train.set_sig1 <- train.set_sig1[!train.set_sig1$original_index %in% rows_to_remove, ]
train.set_sig1$original_index <- NULL

clean_rent_sub_filt_reg1 = lm(totalRent ~ . , train.set_sig1)
s1 = summary(clean_rent_sub_filt_reg1)
plot(clean_rent_sub_filt_reg1)


## model 2 ##
par(mfrow=c(2,2))
clean_rent_reg2 = lm(totalRent ~ log(serviceCharge) + log(pricetrend) + log(noRooms) + log(baseRent) + log(livingSpace) + log(picturecount) + log(heatingCosts) , train.set)
s2 = summary(clean_rent_reg2)
plot(clean_rent_reg2)

#remove row
train.set$original_index <- as.integer(rownames(train.set))
rows_to_remove = c(176648, 261125)
train.set3 <- train.set[!train.set$original_index %in% rows_to_remove, ]
train.set$original_index <- NULL
train.set3$original_index <- NULL

clean_rent_sub_filt_reg2 = lm(totalRent ~ log(serviceCharge) + log(pricetrend) + log(noRooms) + log(baseRent) + log(livingSpace) + log(picturecount) + log(heatingCosts), train.set3)
s2 = summary(clean_rent_sub_filt_reg2)
plot(clean_rent_sub_filt_reg2)

# Dropping insignificant explanatory variables
sig2 = s2$coefficients[,4]<0.05
names(sig2[sig2==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig2[sig2==TRUE]))

rent_sig2 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig2)

train.set3 = rent_sig2[train.index,]
test.set3 = rent_sig2[-train.index,]

clean_rent_sub_filt_reg2 = lm(totalRent ~ log(serviceCharge) + log(pricetrend) + log(noRooms) + log(baseRent) + log(heatingCosts), train.set3)
s2 = summary(clean_rent_sub_filt_reg2)
plot(clean_rent_sub_filt_reg2)

#remove row
train.set3$original_index <- as.integer(rownames(train.set3))
rows_to_remove = c(176648, 61981, 11107, 234770, 165755, 4037, 216971, 147248) # 57593
train.set3 <- train.set3[!train.set3$original_index %in% rows_to_remove, ]
train.set3$original_index <- NULL

clean_rent_sub_filt_reg2 = lm(totalRent ~ log(serviceCharge) + log(pricetrend) + log(noRooms) + log(baseRent) + log(heatingCosts), train.set3)
s2 = summary(clean_rent_sub_filt_reg2)
plot(clean_rent_sub_filt_reg2)

# Dropping insignificant explanatory variables
sig2 = s2$coefficients[,4]<0.05
names(sig2[sig2==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig2[sig2==TRUE]))

rent_sig2 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig2)

train.set3 = rent_sig2[train.index,]
test.set3 = rent_sig2[-train.index,]

clean_rent_sub_filt_reg2 = lm(totalRent ~ log(serviceCharge) + log(noRooms) + log(baseRent) + log(heatingCosts), train.set3)
s2 = summary(clean_rent_sub_filt_reg2)
plot(clean_rent_sub_filt_reg2)

#remove row
train.set3$original_index <- as.integer(rownames(train.set3))
rows_to_remove = c(176648, 61981, 11107, 234770, 165755, 4037, 216971, 147248) # 57593
train.set3 <- train.set3[!train.set3$original_index %in% rows_to_remove, ]
train.set3$original_index <- NULL

clean_rent_sub_filt_reg2 = lm(totalRent ~ log(serviceCharge) + log(noRooms) + log(baseRent) + log(heatingCosts), train.set3)
s2 = summary(clean_rent_sub_filt_reg2)
plot(clean_rent_sub_filt_reg2)

## All Variables - model 3 ##

clean_rent$balcony = factor(clean_rent$balcony)
clean_rent$hasKitchen = factor(clean_rent$hasKitchen)
clean_rent$lift = factor(clean_rent$lift)
clean_rent$garden = factor(clean_rent$garden)

train.set_full = clean_rent[train.index,]
test.set_full = clean_rent[-train.index,]

clean_rent_full_reg = lm(totalRent ~ . -baseRentRange -noRoomsRange -livingSpaceRange, train.set_full)
s3 = summary(clean_rent_full_reg)
plot(clean_rent_full_reg)

#remove row
train.set_full$original_index <- as.integer(rownames(train.set_full))
rows_to_remove = c(176648, 261125)
train.set_full1 <- train.set_full[!train.set_full$original_index %in% rows_to_remove, ]
train.set_full$original_index <- NULL
train.set_full1$original_index <- NULL

clean_rent_full_reg = lm(totalRent ~ . -baseRentRange -noRoomsRange -livingSpaceRange, train.set_full1)
s3 = summary(clean_rent_full_reg)
plot(clean_rent_full_reg)

# P- values
s3$coefficients[,4]  

# Dropping insignificant explanatory variables
sig = s$coefficients[,4]<0.05
names(sig[sig==TRUE])

rent_sig = clean_rent[names(clean_rent) %in% c(names(sig[sig==TRUE]),"totalRent")]
names(rent_sig)

train.set_sig = rent_sig[train.index,]
test.set_sig = rent_sig[-train.index,]

rent_sig_reg = lm(totalRent ~ ., train.set_sig)

s_sig = summary(rent_sig_reg)
plot(rent_sig_reg)

#remove exception obs
train.set_sig$original_index <- as.integer(rownames(train.set_sig))
rows_to_remove = c(176648, 261125)
train.set_sig <- train.set_sig[!train.set_sig$original_index %in% rows_to_remove, ]
train.set_sig$original_index <- NULL

rent_sig_filt_reg = lm(totalRent ~ ., train.set_sig)
s_sig_filt = summary(rent_sig_filt_reg)
plot(rent_sig_filt_reg)

# P- values
s_sig_filt$coefficients[,4]  

# Dropping insignificant explanatory variables
sig = s_sig_filt$coefficients[,4]<0.05
names(sig[sig==TRUE])

rent_sig = clean_rent[names(clean_rent) %in% c(names(sig[sig==TRUE]),"totalRent")]
names(rent_sig)

train.set_sig = rent_sig[train.index,]
test.set_sig = rent_sig[-train.index,]

rent_sig_reg = lm(totalRent ~ ., train.set_sig)

s_sig = summary(rent_sig_reg)
plot(rent_sig_reg)

#remove exception obs
train.set_sig$original_index <- as.integer(rownames(train.set_sig))
rows_to_remove = c(176648, 261125)
train.set_sig <- train.set_sig[!train.set_sig$original_index %in% rows_to_remove, ]
train.set_sig$original_index <- NULL

rent_sig_filt_reg = lm(totalRent ~ ., train.set_sig)
s_sig_filt = summary(rent_sig_filt_reg)
plot(rent_sig_filt_reg)


# Confidence intervals #

confint(rent_sig_filt_reg, level = 0.95)

## All variables + ln on both sides 

clean_rent_full_ln_reg = lm(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(telekomUploadSpeed) + log(yearConstructedRange) + log(baseRent) + log(livingSpace) + log(noRooms) + log(thermalChar) + log(floor) + log(numberOfFloors) + log (heatingCosts) + log(electricityBasePrice) + log(electricityKwhPrice) + balcony + hasKitchen + lift + garden, train.set_full)
s_ln = summary(clean_rent_full_ln_reg)
plot(clean_rent_full_ln_reg)

#remove exception obs
train.set_full$original_index <- as.integer(rownames(train.set_full))
rows_to_remove = c(176648)
train.set_full2 <- train.set_full[!train.set_full$original_index %in% rows_to_remove, ]
train.set_full$original_index <- NULL
train.set_full2$original_index <- NULL

clean_rent_full_ln_reg = lm(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(telekomUploadSpeed) + log(yearConstructedRange) + log(baseRent) + log(livingSpace) + log(noRooms) + log(thermalChar) + log(floor) + log(numberOfFloors) + log (heatingCosts) + log(electricityBasePrice) + log(electricityKwhPrice) + balcony + hasKitchen + lift + garden, train.set_full2)
s_ln = summary(clean_rent_full_ln_reg)
plot(clean_rent_full_ln_reg)

# P- values
s_ln$coefficients[,4]  

# Dropping insignificant explanatory variables
sig2 = s_ln$coefficients[,4]<0.05
names(sig2[sig2==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig2[sig2==TRUE]))

rent_sig2 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig2)

train.set_sig2 = rent_sig2[train.index,]
test.set_sig2 = rent_sig2[-train.index,]

rent_sig_reg2 = lm(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(thermalChar) + log(heatingCosts), train.set_sig2)

s_sig2 = summary(rent_sig_reg2)
plot(rent_sig_reg2)

#remove exception obs
train.set_sig2$original_index <- as.integer(rownames(train.set_sig2))
rows_to_remove = c(176648)#, 61981, 11107, 165755, 216971, 234770, 4037, 147248, 57593)
train.set_sig2 <- train.set_sig2[!train.set_sig2$original_index %in% rows_to_remove, ]
train.set_sig2$original_index <- NULL

clean_rent_full_ln_reg2 = lm(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(thermalChar) + log(heatingCosts), train.set_sig2)
s_sig_filt2 = summary(clean_rent_full_ln_reg2)
plot(clean_rent_full_ln_reg2)

# P- values
s_sig_filt2$coefficients[,4]  

# Dropping insignificant explanatory variables
sig3 = s_sig_filt2$coefficients[,4]<0.05
names(sig3[sig3==TRUE])
clean_names = gsub("^log\\((.*)\\)$", "\\1", names(sig3[sig3==TRUE]))

rent_sig3 = clean_rent[names(clean_rent) %in% c(clean_names,"totalRent")]
names(rent_sig3)

train.set_sig3 = rent_sig3[train.index,]
test.set_sig3 = rent_sig3[-train.index,]

rent_sig_reg3 = lm(totalRent ~  log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), train.set_sig3)

s_sig3 = summary(rent_sig_reg3)
plot(rent_sig_reg3)

#remove exception obs
train.set_sig3$original_index <- as.integer(rownames(train.set_sig3))
rows_to_remove = c(176648) #, 61981, 11107, 165755, 216971, 234770, 4037, 147248, 57593)
train.set_sig3 <- train.set_sig3[!train.set_sig3$original_index %in% rows_to_remove, ]
train.set_sig3$original_index <- NULL

clean_rent_full_ln_reg3 = lm(totalRent ~  log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), train.set_sig3)
s_sig_filt3 = summary(clean_rent_full_ln_reg3)
plot(clean_rent_full_ln_reg3)


### Prediction ###

yhat.test = predict(clean_rent_sub_filt_reg1, test.set_sig1) # predicted totalRent in testing set
e = test.set_sig1$totalRent - yhat.test
test.mse = mean(e^2)
rse = sum(e^2)/sum((test.set_sig1$totalRent-mean(test.set_sig1$totalRent))^2)
test.mse
rse

yhat.test2 = predict(clean_rent_sub_filt_reg2, test.set3) # predicted totalRent in testing set
e = test.set3$totalRent - yhat.test2
test.mse2 = mean(e^2)
rse2 = sum(e^2)/sum((test.set3$totalRent-mean(test.set3$totalRent))^2)
test.mse2
rse2

yhat.test3 = predict(rent_sig_filt_reg, test.set_sig) # predicted totalRent in testing set
e = test.set_sig$totalRent - yhat.test3
test.mse3 = mean(e^2)
rse3 = sum(e^2)/sum((test.set_sig$totalRent-mean(test.set_sig$totalRent))^2)
test.mse3
rse3

yhat.test4 = predict(clean_rent_full_ln_reg3, test.set_sig3) # predicted totalRent in testing set
e = test.set_sig3$totalRent - yhat.test4
test.mse4 = mean(e^2)
rse4 = sum(e^2)/sum((test.set_sig3$totalRent-mean(test.set_sig3$totalRent))^2)
test.mse4
rse4

# best model plot & cor
par(mfrow=c(1,1))
plot(test.set_sig3$totalRent ~ yhat.test4)
abline(0,1)
cor(test.set_sig3$totalRent , yhat.test4)



### Heteroskedasticity Testing ###

# Run the chosen model
model = clean_rent_full_ln_reg3
summary(model)

# Plot residuals vs fitted values
plot(model$fitted.values, resid(model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# -------------------------------
# 2. White's Test (via bptest with squares and interactions)
# -------------------------------
# More general than BP; detects non-linear forms.

white_test <- bptest(model, ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts)
, data = train.set_sig3)
print(white_test)

# Interpretation: Similar to BP. Small p-value indicates heteroskedasticity.


### FGLS ###
# Perform FGLS using the gls function from the nlme package
ols_residuals <- residuals(clean_rent_full_ln_reg3)
variance_model <- lm(I(ols_residuals^2) ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), data = train.set_sig3)
fitted_variances <- fitted(variance_model)

fgls_model <- gls(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), data = train.set_sig3, weights = varFixed(~fitted_variances))
summary(fgls_model)

# Iterate FGLS: Update variance model and re-estimate FGLS until convergence

tolerance <- 1e-6
max_iterations <- 10
iteration <- 1
converged <- FALSE

while (iteration <= max_iterations & !converged) {
  # Update residuals
  fgls_residuals <- residuals(fgls_model)
  
  # Re-estimate the variance model using updated residuals
  variance_model <- lm(I(fgls_residuals^2) ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), data = train.set_sig3)
  new_fitted_variances <- fitted(variance_model)
  
  # Perform FGLS again with updated variance estimates
  new_fgls_model <- gls(totalRent ~ log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), data = train.set_sig3, weights = varFixed(~new_fitted_variances))
  
  # Check for convergence
  if (sum((fitted(new_fgls_model) - fitted(fgls_model))^2) < tolerance) {
    converged <- TRUE
  }
  
  # Update model and iteration
  fgls_model <- new_fgls_model
  fitted_variances <- new_fitted_variances
  iteration <- iteration + 1
}

# Final FGLS model summary
summary(fgls_model)


## All variables + ln on both sides -- After GLS -- remove pricetrend % picturecount

clean_rent_full_ln_reg4 = lm(totalRent ~  log(serviceCharge) + log(picturecount) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), train.set_sig3)
s_sig_filt4 = summary(clean_rent_full_ln_reg4)
par(mfrow=c(2,2))
plot(clean_rent_full_ln_reg4)

#remove exception obs
train.set_full$original_index <- as.integer(rownames(train.set_full))
rows_to_remove = c(61981, 11107, 165755, 176648, 216971, 234770, 4037, 147248)
train.set_sig3 <- train.set_full[!train.set_full$original_index %in% rows_to_remove, ]
train.set_full$original_index <- NULL
train.set_sig3$original_index <- NULL

clean_rent_full_ln_reg4 = lm(totalRent ~  log(serviceCharge) + log(picturecount) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), train.set_sig3)
s_sig_filt4 = summary(clean_rent_full_ln_reg4)
plot(clean_rent_full_ln_reg4)

# remove picturecount
clean_rent_full_ln_reg4 = lm(totalRent ~  log(serviceCharge) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts), train.set_sig3)
s_sig_filt4 = summary(clean_rent_full_ln_reg4)
plot(clean_rent_full_ln_reg4)

# remove livingSpace
clean_rent_full_ln_reg4 = lm(totalRent ~  log(serviceCharge) + log(baseRent) + log(noRooms) + log(heatingCosts), train.set_sig3)
s_sig_filt4 = summary(clean_rent_full_ln_reg4)
plot(clean_rent_full_ln_reg4)

# White's Test (via bptest with squares and interactions)
# -------------------------------
# More general than BP; detects non-linear forms.

model = clean_rent_full_ln_reg4
white_test <- bptest(model, ~ log(serviceCharge) + log(baseRent) + log(noRooms) + log(heatingCosts)
                     , data = train.set_sig3)
print(white_test)




yhat.test5 = predict(clean_rent_full_ln_reg4, test.set_sig3) # predicted totalRent in testing set
e = test.set_sig3$totalRent - yhat.test5
test.mse5 = mean(e^2)
rse5 = sum(e^2)/sum((test.set_sig3$totalRent-mean(test.set_sig3$totalRent))^2)

# best model plot & cor
par(mfrow=c(1,1))
plot(test.set_sig3$totalRent ~ yhat.test5)
abline(0,1)
cor(test.set_sig3$totalRent , yhat.test5)


#####################################
######## Quantile Regression ########
#####################################

# Define the formula for the regression model ---> according to clean_rent_full_ln_reg3 (before FGLS)
formula <- log(totalRent) ~  log(serviceCharge) + log(picturecount) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts)

# Fit the OLS model
ols_model <- lm(formula, data = clean_rent)
summary(ols_model)

# Remove insignificant explanatory - picturecount
formula <- log(totalRent) ~  log(serviceCharge) + log(pricetrend) + log(baseRent) + log(livingSpace) + log(noRooms) + log(heatingCosts)

# Fit the OLS model
ols_model <- lm(formula, data = clean_rent)
summary(ols_model)


# Fit quantile regression models for different quantiles

tau_values <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_models <- lapply(tau_values, function(tau) rq(formula, data = clean_rent, tau = tau))

# Display the summary of each quantile regression model

qr_summaries <- lapply(qr_models, function(model) summary(model, se = "boot"))
names(qr_summaries) <- paste0("tau_", tau_values)
qr_summaries

# Extract OLS coefficients and convert to a data frame with a 'tau' column
ols_coef <- coef(ols_model)
ols_coef_df <- as.data.frame(t(ols_coef))
ols_coef_df$tau <- "OLS"

# Extract quantile regression coefficients and convert to a data frame with a 'tau' column
qr_coefs <- lapply(qr_models, coef)
qr_coef_df <- do.call(rbind, lapply(1:length(qr_coefs), function(i) {
  data.frame(t(qr_coefs[[i]]), tau = tau_values[i])
}))
colnames(qr_coef_df) <- c(names(ols_coef), "tau")

# Combine OLS and quantile regression coefficients for comparison
coef_comparison <- rbind(ols_coef_df, qr_coef_df)

# Melt the data for plotting
coef_melt <- melt(coef_comparison, id.vars = "tau")

# Plot the coefficients
ggplot(coef_melt, aes(x = tau, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "OLS vs Quantile Regression Coefficients", x = "Quantile/OLS", y = "Coefficient") +
  theme_minimal()

# Explanation:
# This plot compares the coefficients of the OLS model with those from the quantile regression models across different quantiles.
# Each line represents the change in the coefficient of a specific predictor across the quantiles.

# Calculate residuals for the OLS model
ols_residuals <- resid(ols_model)

# Calculate residuals for the median regression model
qr_residuals <- resid(qr_models[[3]])

# Plot residuals for OLS
ggplot(clean_rent, aes(x = predict(ols_model), y = ols_residuals)) +
  geom_point() +
  labs(title = "OLS Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Explanation:
# This plot shows the residuals of the OLS model against the fitted values.
# Ideally, the residuals should be randomly scattered around zero, indicating a good fit.

# Plot residuals for quantile regression
ggplot(clean_rent, aes(x = predict(qr_models[[3]]), y = qr_residuals)) +
  geom_point() +
  labs(title = "Quantile Regression Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Explanation:
# This plot shows the residuals of the median quantile regression model against the fitted values.
# Similar to the OLS residual plot, a good fit is indicated by residuals scattered randomly around zero.

# Predict values for OLS and quantile regression
ols_predictions <- predict(ols_model)
qr_predictions <- sapply(qr_models, predict)

# Combine predictions with the original data
clean_rent_pred <- cbind(clean_rent, ols_predictions, qr_predictions)
colnames(clean_rent_pred)[(ncol(clean_rent)+1):(ncol(clean_rent)+length(tau_values)+1)] <- c("OLS", paste0("Q", tau_values))

# Melt the data for plotting
clean_rent_melt <- melt(clean_rent_pred, id.vars = colnames(clean_rent))
clean_rent_melt$value <- exp(clean_rent_melt$value) 

#clean_rent_melt_filtered <- subset(clean_rent_melt, totalRent < 5000) ##########

#clean_rent_filtered <- clean_rent_melt_filtered %>% ##############
#  filter(value < 10000)

# Plot predictions
ggplot(clean_rent_melt, aes(x = totalRent, y = value, color = variable)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("OLS" = "blue", "Q0.1" = "red", "Q0.25" = "orange", "Q0.5" = "green", "Q0.75" = "purple", "Q0.9" = "brown")) +
  labs(title = "OLS vs Quantile Regression Predictions", x = "Actual Values", y = "Predicted Values", color = "Model") +
  theme_minimal()

## Remove exception values
clean_rent_melt_filtered <- subset(clean_rent_melt, totalRent < 5000) 

clean_rent_filtered <- clean_rent_melt_filtered %>% 
  filter(value < 10000)

# Plot predictions
ggplot(clean_rent_filtered, aes(x = totalRent, y = value, color = variable)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("OLS" = "blue", "Q0.1" = "red", "Q0.25" = "orange", "Q0.5" = "green", "Q0.75" = "purple", "Q0.9" = "brown")) +
  labs(title = "OLS vs Quantile Regression Predictions", x = "Actual Values", y = "Predicted Values", color = "Model") +
  theme_minimal()


# Explanation:
# This plot compares the actual values of the response variable (medv) with the predicted values from 
# both the OLS and quantile regression models. It helps to visualize the performance of the models.

# Perform Wald tests for quantile regression coefficients
qr_tests <- lapply(qr_models, function(model) {
  summary(model, se = "boot")$coefficients
})

# Combine the coefficients and p-values into a data frame
qr_pvals_df <- do.call(rbind, lapply(1:length(qr_tests), function(i) {
  coefs <- qr_tests[[i]]
  data.frame(tau = tau_values[i], variable = rownames(coefs), coef = coefs[, 1], p_value = coefs[, 4])
}))

# Plot the p-values
ggplot(qr_pvals_df, aes(x = tau, y = p_value, color = variable, group = variable)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(title = "P-values of Quantile Regression Coefficients", x = "Quantile", y = "P-value") +
  theme_minimal()

# Explanation:
# This plot shows the p-values of the quantile regression coefficients across different quantiles.
# The dashed line at y = 0.05 indicates the common significance level. 
# P-values below this line suggest that the coefficients are statistically significant at that quantile.
