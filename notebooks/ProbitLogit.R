# ---- DEPENDENCY INSTALLATION ----
needed <- c("dplyr","tidyr","ggplot2","broom","car","pROC","margins","plyr")
new_pkgs <- needed[!(needed %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies=TRUE)

# ---- LOAD LIBRARIES & DATA ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(car)
library(pROC)
library(margins)

vars <- c(
  "balcony","hasKitchen","lift","garden",
  "serviceCharge","picturecount","pricetrend","telekomUploadSpeed",
  "yearConstructedRange","baseRent","livingSpace","baseRentRange",
  "noRooms","thermalChar","floor","numberOfFloors","noRoomsRange",
  "livingSpaceRange","heatingCosts","electricityBasePrice",
  "electricityKwhPrice"
)
df <- read.csv("immo_data.csv")[, vars] %>% drop_na()

cat("Sample size:", nrow(df), "\n")
cat("Balcony = 1:", sum(df$balcony==1), 
    "(", round(mean(df$balcony==1)*100,1), "%)\n")
cat("Balcony = 0:", sum(df$balcony==0), 
    "(", round(mean(df$balcony==0)*100,1), "%)\n\n")

df$balcony    <- as.integer(df$balcony)
df$hasKitchen <- as.integer(df$hasKitchen)
df$lift       <- as.integer(df$lift)
df$garden     <- as.integer(df$garden)

# ---- START EXPORT ----

sink("analysis_part2_output.txt")

# EDA:
pdf("EDA_plots.pdf", width=7, height=5)


print(
  ggplot(df, aes(x = serviceCharge, y = balcony)) +
    geom_jitter(width = 0, height = 0.05, alpha = 0.3) +
    geom_smooth(method="loess", se=FALSE, color="darkblue") +
    labs(
      title="Service Charge vs. Balcony",
      x="Service Charge", y="Balcony (0/1)"
    ) +
    theme_minimal()
)
print(
  ggplot(df, aes(x = factor(balcony), y = livingSpace)) +
    geom_boxplot(fill="lightgreen") +
    scale_x_discrete(labels=c("No","Yes")) +
    labs(
      title="Living Space by Balcony",
      x="Balcony", y="Living Space"
    ) +
    theme_minimal()
)

dev.off()

stats_tbl <- df %>%
  group_by(balcony) %>%
  summarise(
    mean_SC = mean(serviceCharge),
    median_SC = median(serviceCharge),
    sd_SC = sd(serviceCharge),
    mean_LS = mean(livingSpace),
    median_LS = median(livingSpace),
    sd_LS = sd(livingSpace)
  )
print(stats_tbl)


# 2. Correlation & Multicollinearity
cont_vars <- c(
  "serviceCharge","picturecount","pricetrend","telekomUploadSpeed",
  "yearConstructedRange","baseRent","livingSpace","baseRentRange",
  "noRooms","thermalChar","floor","numberOfFloors","noRoomsRange",
  "livingSpaceRange","heatingCosts","electricityBasePrice",
  "electricityKwhPrice"
)
corr_mat <- cor(df[cont_vars])
print(round(corr_mat, 2))

vif_mod <- glm(
  balcony ~ .,
  family = binomial(link="logit"),
  data   = df
)
print(round(vif(vif_mod), 2))

# Stepwise selection by AIC
full_mod <- glm(balcony ~ ., family=binomial, data=df)
null_mod <- glm(balcony ~ 1, family=binomial, data=df)

step_mod <- step(
  null_mod,
  scope     = list(lower=null_mod, upper=full_mod),
  direction = "both",
  trace     = FALSE
)
cat("Final formula:", deparse(formula(step_mod)), "\n")

# Fit Logit & Probit models
mod_logit <- glm(
  formula(step_mod),
  family = binomial(link="logit"),
  data   = df
)
cat("\n--- Logit Coefficients ---\n")
print(tidy(mod_logit, conf.int=TRUE))

mod_probit <- glm(
  formula(step_mod),
  family = binomial(link="probit"),
  data   = df
)
cat("\n--- Probit Coefficients ---\n")
print(tidy(mod_probit, conf.int=TRUE))

# Marginal Effects
cat("\n--- Marginal Effects (Logit) ---\n")
print(summary(margins(mod_logit)))

cat("\n--- Marginal Effects (Probit) ---\n")
print(summary(margins(mod_probit)))

# ROC, AUC & Optimal Cutoff
df$pred_logit  <- predict(mod_logit,  type="response")
df$pred_probit <- predict(mod_probit, type="response")

roc_l <- roc(df$balcony, df$pred_logit)
roc_p <- roc(df$balcony, df$pred_probit)

cat("Logit AUC:  ", round(auc(roc_l),3), "\n")
cat("Probit AUC: ", round(auc(roc_p),3), "\n")

plot(roc_l, col="blue", lwd=2, main="ROC Curves")
lines(roc_p, col="red", lwd=2)
legend("bottomright", c("Logit","Probit"), col=c("blue","red"), lwd=2)

opt_l <- as.numeric(coords(roc_l,  "best", ret="threshold"))
opt_p <- as.numeric(coords(roc_p, "best", ret="threshold"))

cat("Optimal cutoff Logit:  ", round(opt_l,3), "\n")
cat("Optimal cutoff Probit:", round(opt_p,3), "\n")

# Confusion Matrix & Accuracy
df$pred_class_l <- ifelse(df$pred_logit  >= opt_l, 1, 0)
df$pred_class_p <- ifelse(df$pred_probit >= opt_p, 1, 0)

cm_l <- table(Pred=df$pred_class_l, Act=df$balcony)
cm_p <- table(Pred=df$pred_class_p, Act=df$balcony)

cat("\n--- Confusion Matrix (Logit) ---\n");  print(cm_l)
cat("Accuracy Logit:  ", round(mean(df$pred_class_l==df$balcony),3), "\n")

cat("\n--- Confusion Matrix (Probit) ---\n"); print(cm_p)
cat("Accuracy Probit: ", round(mean(df$pred_class_p==df$balcony),3), "\n")


# Sensitivity analysis for cutoff
cutoffs <- seq(0.1, 0.9, by=0.05)
sens_spec <- t(sapply(cutoffs, function(cut) {
  pred <- ifelse(df$pred_logit >= cut, 1, 0)
  cm   <- table(Pred=pred, Act=df$balcony)
  sens <- cm["1","1"] / sum(cm[,"1"])
  spec <- cm["0","0"] / sum(cm[,"0"])
  c(sensitivity=sens, specificity=spec)
}))
# print(data.frame(cutoff=cutoffs, sens_spec))
print("=== Sensitivity/Specificity by cutoff ===")
print(sens_spec)

# Interactions
mod_logit_int <- glm(
  balcony ~ . + livingSpace:serviceCharge,
  family = binomial(link="logit"),
  data   = df
)
# cat("\n--- Logit + Interaction livingSpace:serviceCharge ---\n")
# print(tidy(mod_logit_int, conf.int=TRUE))
#  cat("AIC (base) =", AIC(mod_logit),
#     " vs AIC (int) =", AIC(mod_logit_int), "\n")
#########
cat(enc2utf8("=== Logit with livingSpace\u00D7serviceCharge interaction ===\n"))

out <- broom::tidy(mod_logit_int, conf.int = TRUE)
print.data.frame(as.data.frame(out), row.names = FALSE)
#########
cat("AIC (base) =", AIC(mod_logit),
    " vs AIC (with int) =", AIC(mod_logit_int), "\n")

# ---- END EXPORT ----
sink()
