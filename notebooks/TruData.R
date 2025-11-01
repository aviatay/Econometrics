# ---- START EXPORT ----
sink("analysis_part1_output.txt")

needed <- c("dplyr","VGAM","truncreg","ggplot2","broom")
new_pkgs <- needed[!(needed %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies=TRUE)

library(dplyr)
library(VGAM)       # ל־vglm::tobit
library(truncreg)   # ל־truncreg
library(ggplot2)
library(broom)

#Read and clean data
df <- read.csv("immo_data.csv")
vars <- c(
  "totalRent",
  "livingSpaceRange", "noRoomsRange",
  "serviceCharge", "heatingCosts", "electricityBasePrice",
  "numberOfFloors", "balcony", "garden"
)
df_clean <- df %>%
  select(all_of(vars)) %>%
  drop_na()

# Determine U
Q1      <- quantile(df_clean$totalRent, 0.25)
Q3      <- quantile(df_clean$totalRent, 0.75)
IQR_val <- IQR(df_clean$totalRent)
U       <- Q3 + 1.5 * IQR_val

cat(
  "TotalRent distribution:\n",
  sprintf("  Q1 = %.2f\n", Q1),
  sprintf("  Q3 = %.2f\n", Q3),
  sprintf("  IQR = %.2f\n", IQR_val),
  sprintf("  Threshold U = %.2f\n\n", U)
)

# EDA
png("part1_totalRent_hist.png", width=800, height=500)
hist(df_clean$totalRent,
     breaks=50,
     main="Distribution of totalRent with Threshold U",
     xlab="totalRent",
     col="lightblue",
     border="white")
abline(v = U, col = "red", lwd = 2)
text(x = U, y = par("usr")[4]*0.9,
     labels = paste0("U = ", round(U,1)),
     col = "red", pos = 4)
dev.off()

# 4. Tobit model with VGAM::vglm
model_tobit <- vglm(
  totalRent ~ livingSpaceRange + noRoomsRange +
              serviceCharge + heatingCosts + electricityBasePrice +
              numberOfFloors + balcony + garden,
  tobit(Lower = 0, Upper = U),
  data = df_clean
)
cat("--- Tobit (vglm) Summary ---\n")
print(summary(model_tobit))

# 5. Truncated regression with truncreg
df_trunc <- df_clean %>% filter(totalRent < U)

model_trunc <- truncreg(
  totalRent ~ livingSpaceRange + noRoomsRange +
              serviceCharge + heatingCosts + electricityBasePrice +
              numberOfFloors + balcony + garden,
  point     = U,
  direction = "right",
  data      = df_trunc
)
cat("\n--- Truncated Regression Summary ---\n")
print(summary(model_trunc))


# Compare (logLik, AIC, BIC)
metrics <- data.frame(
  Model     = c("Tobit", "TruncReg"),
  logLik    = c(as.numeric(logLik(model_tobit)), as.numeric(logLik(model_trunc))),
  AIC       = c(AIC(model_tobit), AIC(model_trunc)),
  BIC       = c(BIC(model_tobit), BIC(model_trunc))
)
cat("\n--- Model Fit Metrics ---\n")
print(metrics)

# Residuals vs Fitted + QQ-plots
pdf("part1_diagnostics.pdf", width = 10, height = 8)
par(mfrow = c(2, 2))

# Tobit: Residuals vs Fitted (location μ)
mu_hat   <- predict(model_tobit, type="response")
resid_mu <- df_clean$totalRent - mu_hat
plot(
  mu_hat, resid_mu,
  main = "Tobit: Residuals vs Fitted (μ)",
  xlab = "Predicted μ (response)", ylab = "Residuals (y - μ̂)"
)
abline(h = 0, lty = 2)

# Tobit: QQ-plot of those residuals
qqnorm(resid_mu, main = "Tobit: QQ-Plot of Response Residuals")
qqline(resid_mu)

# TruncReg: Residuals vs Fitted
plot(
  fitted(model_trunc), residuals(model_trunc),
  main = "TruncReg: Residuals vs Fitted",
  xlab = "Fitted values", ylab = "Residuals"
)
abline(h = 0, lty = 2)

# TruncReg: QQ-plot of residuals
qqnorm(residuals(model_trunc), main = "TruncReg: QQ-Plot of Residuals")
qqline(residuals(model_trunc))

#  Tobit: Residuals vs Fitted for all data
pdf("part1_fullsample_residuals.pdf", width=6, height=4)
plot(
  mu_hat,            
  resid_mu,          
  main = "Tobit: Full-sample Residuals vs Fitted",
  xlab = "Predicted u (response)",
  ylab = "Residuals (y - u^)"
)
abline(h = 0, lty = 2)
dev.off()

dev.off()

# ---- END EXPORT ----
sink()

