# Load necessary libraries
install.packages("plm")
install.packages("tidyr")

library(plm)
library(ggplot2)
library(lmtest)
library(dplyr)
library(tidyr)
library(car)
library(corrplot)

########################
###### Panel Data ######
########################

##############################
### Fixed (within) Effects ###
##############################

data = read.csv("C:/Users/97250/OneDrive - Technion/לימודים/סמסטר 10/אקונומטריקה/Econometrics Course/camp_teach.csv")

# 2 or more rows for each id
filtered_data <- data %>%
  group_by(id) %>%
  filter(n() >= 2) %>%
  ungroup()

### Fixed Effects ###
filtered_data$TG <- factor(filtered_data$TG)
filtered_data$ETHNIC <- factor(filtered_data$ETHNIC)
filtered_data$GENDER <- factor(filtered_data$GENDER)
filtered_data$visitc <- as.factor(filtered_data$visitc)

par(mfrow=c(1,1))

# Pearson correlation
filtered_data_cor = filtered_data[c("age_rz", "PREFEV", "PREFVC", "PREFF", "PREPF", "POSFVC", "POSFF", "POSPF", "PREFEVPP", "PREFVCPP", "POSFEVPP", "POSFVCPP", "fdays" , "visitc", "POSFEV")]
filtered_data_cor.cor = cor(filtered_data_cor, use = "pairwise.complete.obs")

corrplot(filtered_data_cor.cor, type = "upper", method = "number", number.cex = 0.6, tl.cex = 0.8, mar = c(1, 1, 1, 1))

# Spearman correlation

filtered_data_cor.cor_S = cor(filtered_data_cor, method = "spearman", use = "pairwise.complete.obs")
corrplot(filtered_data_cor.cor_S, type = "upper", method = "number", number.cex = 0.6, tl.cex = 0.8, mar = c(1, 1, 1, 1))

filtered_data$visitc <- as.factor(filtered_data$visitc)

# Define the model formula including time dummies

formula <- POSFEV ~ age_rz + PREFEV + PREFVCPP + TG + ETHNIC + GENDER + visitc

# Estimate the fixed effects model

fixed_effects_model <- plm(formula, data = filtered_data, index = c("id", "visitc"), model = "within")

# Summary of the fixed effects model to view coefficients and statistical significance

s = summary(fixed_effects_model)
s

linearHypothesis(fixed_effects_model, 
                 c("visitc2 = 0", "visitc4 = 0", "visitc12 = 0", 
                   "visitc16 = 0", "visitc24 = 0", "visitc28 = 0", 
                   "visitc36 = 0", "visitc40 = 0", "visitc44 = 0", 
                   "visitc48 = 0", "visitc52 = 0", "visitc56 = 0", 
                   "visitc60 = 0", "visitc64 = 0", "visitc72 = 0", 
                   "visitc84 = 0", "visitc96 = 0", "visitc108 = 0", 
                   "visitc120 = 0"))


# Statistical Test for Fixed Effects
# Conduct the Hausman test
# First, estimate the random effects model for comparison

model_rand <- plm(formula, data = filtered_data, model = "random", index = c("id", "visitc"))
summary(model_rand)

to_remove <- c("age_rz", "TG","ETHNIC", "GENDER")

new_formula <- as.formula(
  paste("POSFEV ~", 
        paste(setdiff(attr(terms(formula), "term.labels"), to_remove), collapse = " + "))
)

model_rand_reduced <- plm(new_formula, 
                          data = filtered_data, 
                          model = "random", 
                          index = c("id", "visitc"))

summary(model_rand_reduced)


linearHypothesis(model_rand_reduced, 
                 c("visitc2 = 0", "visitc4 = 0", "visitc12 = 0", 
                   "visitc16 = 0", "visitc24 = 0", "visitc28 = 0", 
                   "visitc36 = 0", "visitc40 = 0", "visitc44 = 0", 
                   "visitc48 = 0", "visitc52 = 0", "visitc56 = 0", 
                   "visitc60 = 0", "visitc64 = 0", "visitc72 = 0", 
                   "visitc84 = 0", "visitc96 = 0", "visitc108 = 0", 
                   "visitc120 = 0"), test = "F")



# Perform the Hausman test to compare fixed and random effects models

hausman_test <- phtest(fixed_effects_model, model_rand_reduced)

# Display the results of the Hausman test

hausman_test

# The Hausman test compares the fixed effects model with the random effects model to determine which is more appropriate.
# If the p-value is small (typically < 0.05), the fixed effects model is preferred, indicating that individual effects are correlated with the explanatory variables.


# Generate Graphs
# Extract fixed effects (individual-specific effects) from the fixed effects model

fixed_effects <- fixef(fixed_effects_model)

# Convert the fixed effects to a data frame for plotting

fixed_effects_df <- data.frame(Patience = names(fixed_effects), Effect = as.numeric(fixed_effects))

# Plot the fixed effects by patience to visualize the individual-specific effects

ggplot(fixed_effects_df, aes(x = reorder(Patience, Effect), y = Effect)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Fixed Effects by Patience", x = "Patience", y = "Fixed Effect")

# Plot residuals to check the distribution of residuals from the fixed effects model

residuals <- resid(fixed_effects_model)
residuals_df <- data.frame(Residuals = residuals)
ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

par(mfrow=c(1,1))
x <- as.numeric(model.frame(fixed_effects_model)$POSFEV)
y <- as.numeric(resid(fixed_effects_model))
graphics::plot.default(
  x, y,
  main = "Scatter plot of Residuals from Fixed Effects Model",
  xlab = "POSFEV", ylab = "Residuals",
  pch = 20
)
abline(h=0, col="red")


# plm to lm
model_fixed_lm <- lm(formula(fixed_effects_model), data = model.frame(fixed_effects_model))
par(mfrow = c(2, 2))
plot(model_fixed_lm)


pdat <- pdata.frame(filtered_data, index = c("id","visitc"))

plm::plmtest(
  formula, data = pdat,
  effect = "individual",    
  type   = "bp"
)
pbgtest(fixed_effects_model)


coeftest(fixed_effects_model, vcov = vcovHC(fixed_effects_model, type = "HC1", cluster = "group"))


