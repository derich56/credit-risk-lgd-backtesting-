############################################################
## ✅ STEP 0 — LIBRARY
############################################################
library(data.table)
library(dplyr)
library(scorecard)
library(lightgbm)
library(pROC)
library(broom)
library(lmtest)
library(sandwich)
library(ggplot2)
library(forecast)
library(car)
library(gridExtra)
library(ResourceSelection)
set.seed(123)
############################################################
## ✅ STEP 1 — DATA PREPARATION
############################################################
loan_data <- fread(file.choose())
names(loan_data) <- tolower(names(loan_data))

MRP <- 36

loan_data[, int_rate := int_rate/100]
loan_data[, discount_factor := (1 + int_rate/12)^MRP]

loan_data[, recovery_cf :=
            recoveries + collection_recovery_fee]

loan_data[, recovery_cf :=
            recoveries + collection_recovery_fee +
            total_rec_prncp + total_rec_int + total_rec_late_fee]

loan_data[, recovery_cf_disc := recovery_cf / discount_factor]

loan_data[, lgd := (funded_amnt - recovery_cf_disc)/funded_amnt]
loan_data[, lgd := fifelse(is.na(lgd)|lgd>1,1,fifelse(lgd<0,0,lgd))]

loan_data[, utilization := revol_bal/total_rev_hi_lim]
loan_data[, utilization := fifelse(utilization>1,1,utilization)]
dim(loan_data)
############################################################
## ✅ STEP 2 — FEATURE SELECTION
############################################################
data <- loan_data[,.(issue_d_date,
                     ead=funded_amnt,
                     zip_code,
                     grade,
                     utilization,
                     annual_inc,
                     purpose,
                     inq_last_6mths,
                     mths_since_last_delinq,
                     dti,
                     lgd)]

setorder(data,issue_d_date)

############################################################
## ✅ STEP 3 — TRAIN TEST SPLIT
############################################################
cut <- floor(0.8*nrow(data))
cut_date <- data[cut, issue_d_date]

train <- data[issue_d_date <= cut_date]
test  <- data[issue_d_date > cut_date]

############################################################
## ✅ STEP 4 — CLEAN + BASE DATA
############################################################
train_clean <- train %>%
  mutate(zip_group = substr(zip_code,1,2)) %>%
  select(-zip_code,-issue_d_date)

test_clean <- test %>%
  mutate(zip_group = substr(zip_code,1,2)) %>%
  select(-zip_code,-issue_d_date)

train_base <- na.omit(train_clean)
test_base  <- na.omit(test_clean)

y_train <- train_base$lgd
y_test  <- test_base$lgd

############################################################
## ✅ STEP 5 — LINEAR MODEL
############################################################
X_train <- model.matrix(lgd~.-1,data=train_base)
X_test  <- model.matrix(lgd~.-1,data=test_base)

missing_cols <- setdiff(colnames(X_train),colnames(X_test))
for(col in missing_cols){
  X_test <- cbind(X_test,data.frame(0))
  colnames(X_test)[ncol(X_test)] <- col
}
X_test <- X_test[,colnames(X_train),drop=FALSE]

train_df <- as.data.frame(X_train); train_df$lgd <- y_train
test_df  <- as.data.frame(X_test)

model_lr <- lm(lgd~.,data=train_df)

robust_test <- coeftest(model_lr, vcov=vcovHC(model_lr,"HC1"))

pval_table <- data.table(
  Variable = rownames(robust_test),
  p_value  = robust_test[,4]
)

sig_vars <- pval_table[
  p_value < 0.05 & Variable != "(Intercept)",
  Variable
]

train_pars <- train_df[, c("lgd", sig_vars), drop=FALSE]
test_pars  <- test_df[, sig_vars, drop=FALSE]

model_pars <- lm(lgd ~ ., data=train_pars)

pred_pars <- predict(model_pars, test_pars)
pred_pars[is.na(pred_pars)] <- mean(y_train)
pred_pars <- pmin(pmax(pred_pars,0),1)

############################################################
## ✅ STEP 6 — WOE MODEL (FIXED)
############################################################
features_selected <- c("ead","grade","utilization","annual_inc",
                       "purpose","inq_last_6mths",
                       "mths_since_last_delinq","dti","zip_group")

train_woe_input <- train_base[, c(features_selected,"lgd"),with=FALSE]
test_woe_input  <- test_base[,  c(features_selected,"lgd"),with=FALSE]

threshold <- quantile(train_woe_input$lgd,0.7)
train_woe_input[, lgd_bin := ifelse(lgd>=threshold,1,0)]


set.seed(123)
bins <- woebin(
  train_woe_input,
  y="lgd_bin",
  x=features_selected,
  method="tree",
  stop_limit=0.05,
  min_perc_fine_bin=0.02,
  min_perc_coarse_bin=0.05
)

train_woe <- woebin_ply(train_woe_input,bins)
test_woe  <- woebin_ply(test_woe_input,bins)

woe_vars <- paste0(features_selected,"_woe")

model_woe <- lm(
  as.formula(paste("lgd ~",paste(woe_vars,collapse="+"))),
  data=train_woe
)

pred_woe <- predict(model_woe,test_woe)
pred_woe[is.na(pred_woe)] <- mean(train_woe$lgd)
pred_woe <- pmin(pmax(pred_woe,0),1)

iv <- sapply(bins, function(x) sum(x$bin_iv))
print(iv)

############################################################
## ✅ STEP 7 — LIGHTGBM (FIXED)
############################################################
train_lgb <- copy(train_base)
test_lgb  <- copy(test_base)

cat_cols <- c("grade","purpose","zip_group")

for (col in cat_cols) {
  train_lgb[[col]] <- as.integer(as.factor(train_lgb[[col]]))
  test_lgb[[col]]  <- as.integer(factor(test_lgb[[col]],
                                        levels=levels(as.factor(train_base[[col]]))))
}

train_lgb[is.na(train_lgb)] <- 0
test_lgb[is.na(test_lgb)]  <- 0

X_train_lgb <- as.matrix(train_lgb[,..features_selected])
X_test_lgb  <- as.matrix(test_lgb[,..features_selected])

y_train_lgb <- train_lgb$lgd
y_test_lgb  <- test_lgb$lgd

epsilon <- 1e-5
y_train_logit <- log(pmin(pmax(y_train_lgb,epsilon),1-epsilon)/(1-pmin(pmax(y_train_lgb,epsilon),1-epsilon)))
y_test_logit  <- log(pmin(pmax(y_test_lgb,epsilon),1-epsilon)/(1-pmin(pmax(y_test_lgb,epsilon),1-epsilon)))

dtrain <- lgb.Dataset(X_train_lgb,label=y_train_logit)

model_lgb <- lgb.train(
  params=list(objective="regression",metric="rmse",
              learning_rate=0.05,num_leaves=30, 
              seed=123,                 
              feature_fraction_seed=123,
              bagging_seed=123
  ),
  data=dtrain,nrounds=200,verbose=-1
)

pred_lgb <- predict(model_lgb,X_test_lgb)
pred_lgb <- exp(pred_lgb)/(1+exp(pred_lgb))
pred_lgb <- pmin(pmax(pred_lgb,0),1)

############################################################
## ✅ STEP 8 — Regression METRIC
############################################################
rmse <- function(a,p) sqrt(mean((a-p)^2, na.rm=TRUE))
mae  <- function(a,p) mean(abs(a-p), na.rm=TRUE)
mape <- function(a,p){
  idx <- which(a >= 0.05 & is.finite(a) & is.finite(p))
  mean(abs((a[idx] - p[idx]) / a[idx]), na.rm=TRUE) * 100
}

pred_pars_train  <- pmin(pmax(predict(model_pars, train_df), 0), 1)
pred_woe_train <- pmin(pmax(predict(model_woe, train_woe), 0), 1)

pred_logit_train <- predict(model_lgb, X_train_lgb)
pred_lgb_train <- exp(pred_logit_train)/(1+exp(pred_logit_train))
pred_lgb_train <- pmin(pmax(pred_lgb_train, 0), 1)

result_metrics <- data.table(
  Model = c("Linear","WOE","LightGBM"),
  
  RMSE_Train = c(
    rmse(y_train, pred_pars_train),
    rmse(y_train, pred_woe_train),
    rmse(y_train, pred_lgb_train)
  ),
  
  RMSE_Test = c(
    rmse(y_test, pred_pars),
    rmse(y_test, pred_woe),
    rmse(y_test, pred_lgb)
  ),
  
  MAE_Train = c(
    mae(y_train, pred_pars_train),
    mae(y_train, pred_woe_train),
    mae(y_train, pred_lgb_train)
  ),
  
  MAE_Test = c(
    mae(y_test, pred_pars),
    mae(y_test, pred_woe),
    mae(y_test, pred_lgb)
  ),
  
  MAPE_Train = c(
    mape(y_train, pred_pars_train),
    mape(y_train, pred_woe_train),
    mape(y_train, pred_lgb_train)
  ),
  
  MAPE_Test = c(
    mape(y_test, pred_pars),
    mape(y_test, pred_woe),
    mape(y_test, pred_lgb)
  )
)

cat("\n=== REGRESSION METRICS (BEFORE CALIBRATION) ===\n")
print(result_metrics)

error_test <- rbindlist(list(
  data.table(Model="Linear",   Error=y_test - pred_pars),
  data.table(Model="WOE",      Error=y_test - pred_woe),
  data.table(Model="LightGBM", Error=y_test - pred_lgb)
))

error_test <- error_test[is.finite(Error)]

error_summary_test <- error_test[, .(
  Mean_Error = mean(Error, na.rm=TRUE),
  SD_Error   = sd(Error, na.rm=TRUE),
  P95_AbsErr = quantile(abs(Error), 0.95, na.rm=TRUE)
), by = Model]

error_summary_test[, Bias :=
                     fifelse(abs(Mean_Error) <= 0.01, "✅ No Bias",
                             fifelse(abs(Mean_Error) <= 0.03, "⚠ Minor Bias", "❌ Bias"))]

error_summary_test[, Stability :=
                     fifelse(SD_Error >= 0.05 & SD_Error <= 0.20, "✅ Stable", "⚠ Check")]

error_summary_test[, TailRisk :=
                     fifelse(P95_AbsErr <= 0.20, "✅ Safe",
                             fifelse(P95_AbsErr <= 0.30, "⚠ Medium", "❌ High Risk"))]

cat("\n=== ERROR BEFORE CALIBRATION ===\n")
print(error_summary_test)

bias_lr  <- mean(y_test - pred_pars, na.rm=TRUE)
bias_woe <- mean(y_test - pred_woe, na.rm=TRUE)
bias_lgb <- mean(y_test - pred_lgb, na.rm=TRUE)

cat("\n=== BIAS ===\n")
print(data.table(Model=c("Linear","WOE","LightGBM"),
                 Bias=c(bias_lr,bias_woe,bias_lgb)))

pred_pars_train_cal  <- pmin(pmax(pred_pars_train  + bias_lr,  0), 1)
pred_woe_train_cal <- pmin(pmax(pred_woe_train + bias_woe, 0), 1)
pred_lgb_train_cal <- pmin(pmax(pred_lgb_train + bias_lgb, 0), 1)

pred_pars_cal  <- pmin(pmax(pred_pars  + bias_lr, 0), 1)
pred_woe_cal <- pmin(pmax(pred_woe + bias_woe, 0), 1)
pred_lgb_cal <- pmin(pmax(pred_lgb + bias_lgb, 0), 1)

result_metrics_cal <- data.table(
  Model = c("Linear","WOE","LightGBM"),
  
  RMSE_Train = c(
    rmse(y_train, pred_pars_train_cal),
    rmse(y_train, pred_woe_train_cal),
    rmse(y_train, pred_lgb_train_cal)
  ),
  
  RMSE_Test = c(
    rmse(y_test, pred_pars_cal),
    rmse(y_test, pred_woe_cal),
    rmse(y_test, pred_lgb_cal)
  ),
  
  MAE_Train = c(
    mae(y_train, pred_pars_train_cal),
    mae(y_train, pred_woe_train_cal),
    mae(y_train, pred_lgb_train_cal)
  ),
  
  MAE_Test = c(
    mae(y_test, pred_pars_cal),
    mae(y_test, pred_woe_cal),
    mae(y_test, pred_lgb_cal)
  ),
  
  MAPE_Train = c(
    mape(y_train, pred_pars_train_cal),
    mape(y_train, pred_woe_train_cal),
    mape(y_train, pred_lgb_train_cal)
  ),
  
  MAPE_Test = c(
    mape(y_test, pred_pars_cal),
    mape(y_test, pred_woe_cal),
    mape(y_test, pred_lgb_cal)
  )
)

cat("\n=== METRICS AFTER CALIBRATION ===\n")
print(result_metrics_cal)

error_cal <- rbindlist(list(
  data.table(Model="Linear",   Error=y_test - pred_pars_cal),
  data.table(Model="WOE",      Error=y_test - pred_woe_cal),
  data.table(Model="LightGBM", Error=y_test - pred_lgb_cal)
))

error_cal <- error_cal[is.finite(Error)]

error_summary_cal <- error_cal[, .(
  Mean_Error = mean(Error, na.rm=TRUE),
  SD_Error   = sd(Error, na.rm=TRUE),
  P95_AbsErr = quantile(abs(Error), 0.95, na.rm=TRUE)
), by = Model]

error_summary_cal[, Bias :=
                    fifelse(abs(Mean_Error) <= 0.01, "✅ No Bias", "❌ Bias")]

error_summary_cal[, Stability :=
                    fifelse(SD_Error >= 0.05 & SD_Error <= 0.20, "✅ Stable", "⚠ Check")]

error_summary_cal[, TailRisk :=
                    fifelse(P95_AbsErr <= 0.20, "✅ Safe", "⚠ Risk")]

cat("\n=== ERROR AFTER CALIBRATION ===\n")
print(error_summary_cal)

############################################################
## ✅ STEP  9 —  Classification Metrics
############################################################
cutoff <- quantile(y_train, 0.7)

y_train_cls <- as.integer(y_train >= cutoff)
y_test_cls  <- as.integer(y_test  >= cutoff)

auc_compare <- data.table(
  Model = c("Linear","WOE","LightGBM"),
  
  AUC_Train = c(
    as.numeric(auc(y_train_cls, pred_pars_train)),
    as.numeric(auc(y_train_cls, pred_woe_train)),
    as.numeric(auc(y_train_cls, pred_lgb_train))
  ),
  
  AUC_Test = c(
    as.numeric(auc(y_test_cls, pred_pars)),
    as.numeric(auc(y_test_cls, pred_woe)),
    as.numeric(auc(y_test_cls, pred_lgb))
  )
)

confusion_metrics <- function(y,p){
  threshold <- quantile(p, 0.7)
  yhat <- ifelse(p>=threshold,1,0)
  
  TP <- sum(y==1 & yhat==1)
  TN <- sum(y==0 & yhat==0)
  FP <- sum(y==0 & yhat==1)
  FN <- sum(y==1 & yhat==0)
  
  Total <- TP+TN+FP+FN
  
  data.table(
    Accuracy  = (TP+TN)/Total,
    Precision = TP/(TP+FP),
    Recall    = TP/(TP+FN),
    TP=TP/Total, TN=TN/Total, FP=FP/Total, FN=FN/Total
  )
}

conf_table <- rbindlist(list(
  cbind(Model="Linear", confusion_metrics(y_test_cls, pred_pars)),
  cbind(Model="WOE", confusion_metrics(y_test_cls, pred_woe)),
  cbind(Model="LightGBM", confusion_metrics(y_test_cls, pred_lgb))
))

############################################################
## ✅ STEP 10 —  Final Output
############################################################
cat("\n================ LINEAR COEFFICIENT ================\n")
print(data.table(
  Variable = names(coef(model_pars)),
  Coefficient = coef(model_pars)
))

cat("\n================ WOE COEFFICIENT ================\n")
print(data.table(
  Variable = names(coef(model_woe)),
  Coefficient = coef(model_woe)
))

cat("\n================ REGRESSION METRICS ================\n")
print(result_metrics_cal)

cat("\n================ ROC AUC ================\n")
print(auc_compare)


cat("\n================ CONFUSION METRICS ================\n")
print(conf_table)

cat("\n================ ERROR DISTRIBUTION (AFTER CALIBRATION) ================\n")
print(error_summary_cal)

############################################################
## ✅ STEP 11—  Shuffle, OOT, Baseline Test 
############################################################
set.seed(123)

n <- nrow(data)

cut_valid <- floor(0.8 * n)
date_valid <- data[cut_valid, "issue_d_date"]

oot_st <- data[data$issue_d_date > date_valid, ]

data_non_oot <- data[data$issue_d_date <= date_valid, ]

idx <- sample(1:nrow(data_non_oot))

train_idx <- idx[1:floor(0.75 * length(idx))]
valid_idx <- idx[(floor(0.75 * length(idx)) + 1):length(idx)]

train_st <- data_non_oot[train_idx, ]
valid_st <- data_non_oot[valid_idx, ]

clean_func <- function(df){
  df %>%
    mutate(zip_group = substr(zip_code,1,2)) %>%
    select(-zip_code, -issue_d_date) %>%
    na.omit()
}

train_st <- clean_func(train_st)
valid_st <- clean_func(valid_st)
oot_st   <- clean_func(oot_st)

y_train <- train_st$lgd
y_valid <- valid_st$lgd
y_oot   <- oot_st$lgd

X_train <- model.matrix(lgd ~ . -1, data = train_st)
X_valid <- model.matrix(lgd ~ . -1, data = valid_st)
X_oot   <- model.matrix(lgd ~ . -1, data = oot_st)

add_missing_cols <- function(X_ref, X_target){
  miss <- setdiff(colnames(X_ref), colnames(X_target))
  for(col in miss){
    X_target <- cbind(X_target, 0)
    colnames(X_target)[ncol(X_target)] <- col
  }
  X_target <- X_target[, colnames(X_ref), drop=FALSE]
  return(X_target)
}

X_valid <- add_missing_cols(X_train, X_valid)
X_oot   <- add_missing_cols(X_train, X_oot)

train_df <- as.data.frame(X_train)
valid_df <- as.data.frame(X_valid)
oot_df   <- as.data.frame(X_oot)

sig_vars_fix <- intersect(sig_vars, colnames(train_df))

if(length(sig_vars_fix) == 0){
  sig_vars_fix <- colnames(train_df)
}

train_pars <- train_df[, sig_vars_fix, drop=FALSE]
valid_pars <- valid_df[, sig_vars_fix, drop=FALSE]
oot_pars   <- oot_df[, sig_vars_fix, drop=FALSE]

model_pars <- lm(y_train ~ ., data = train_pars)

pred_valid <- predict(model_pars, valid_pars)
pred_oot   <- predict(model_pars, oot_pars)

pred_valid <- pmin(pmax(pred_valid, 0), 1)
pred_oot   <- pmin(pmax(pred_oot, 0), 1)

cat("\n================ RMSE SHUFFLE VALIDATION ================\n")
print(rmse(y_valid, pred_valid))

cat("\n================ RMSE OOT TEST ================\n")
print(rmse(y_oot, pred_oot))

baseline_pred <- rep(mean(y_train), length(y_oot))

cat("\n================ RMSE BASELINE (OOT) ================\n")
print(rmse(y_oot, baseline_pred))

############################################################
## ✅ STEP 12 — FINAL MODEL VALIDATION + ASSUMPTION
############################################################

cat("\n================ FINAL MODEL VALIDATION ================\n")

############################
## ✅ 1. KS STATISTIC (DISCRIMINATION)
############################
ks_stat <- function(y, p){
  df <- data.frame(y, p)
  df <- df[order(df$p), ]
  df$cum_good <- cumsum(df$y==0)/sum(df$y==0)
  df$cum_bad  <- cumsum(df$y==1)/sum(df$y==1)
  max(abs(df$cum_good - df$cum_bad))
}

ks_results <- data.table(
  Model = c("Linear","WOE","LightGBM"),
  KS = c(
    ks_stat(y_test_cls, pred_pars),
    ks_stat(y_test_cls, pred_woe),
    ks_stat(y_test_cls, pred_lgb)
  )
)

cat("\n=== KS STATISTIC ===\n")
print(ks_results)

############################
## ✅ 2. GINI (MODEL POWER)
############################
gini <- function(auc) 2*auc - 1

gini_table <- data.table(
  Model = auc_compare$Model,
  Gini_Train = gini(auc_compare$AUC_Train),
  Gini_Test  = gini(auc_compare$AUC_Test)
)

cat("\n=== GINI ===\n")
print(gini_table)

############################
## ✅ 3. PSI (STABILITY)
############################
psi <- function(train, test, buckets=10){
  breaks <- quantile(train, probs=seq(0,1,1/buckets), na.rm=TRUE)
  
  train_bin <- cut(train, breaks, include.lowest=TRUE)
  test_bin  <- cut(test, breaks, include.lowest=TRUE)
  
  train_dist <- prop.table(table(train_bin))
  test_dist  <- prop.table(table(test_bin))
  
  sum((train_dist - test_dist) * log(train_dist / test_dist))
}

psi_results <- data.table(
  Model = c("Linear","WOE","LightGBM"),
  PSI = c(
    psi(pred_pars_train, pred_pars),
    psi(pred_woe_train, pred_woe),
    psi(pred_lgb_train, pred_lgb)
  )
)

cat("\n=== PSI ===\n")
print(psi_results)

############################
## ✅ 4. CALIBRATION (FIXED)
############################
plot_calib <- function(actual, pred, title){
  
  df <- data.frame(actual = actual, pred = pred)
  
  breaks <- unique(quantile(df$pred,
                            probs = seq(0, 1, 0.1),
                            na.rm = TRUE))
  
  if(length(breaks) < 5){
    breaks <- seq(min(df$pred),
                  max(df$pred),
                  length.out = 11)
  }
  
  df$bin <- cut(df$pred,
                breaks = breaks,
                include.lowest = TRUE)
  
  calib <- aggregate(cbind(actual, pred) ~ bin, df, mean)
  
  #############################################
  ## ✅ FIX SCALE BIAR GARIS MERAH BENAR
  #############################################
  
  min_val <- min(c(calib$pred, calib$actual))
  max_val <- max(c(calib$pred, calib$actual))
  
  #############################################
  ## ✅ PLOT
  #############################################
  
  ggplot(calib, aes(x = pred, y = actual)) +
    
    geom_point(size = 3, color = "black") +
    
    geom_line(color = "blue", linewidth = 1) +
    
    geom_abline(slope = 1,
                intercept = 0,
                color = "red",
                linewidth = 1.5) +
    
    coord_equal(xlim = c(min_val, max_val),
                ylim = c(min_val, max_val)) +
    
    labs(title = title,
         x = "Predicted",
         y = "Actual") +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

############################################################
## ✅ CREATE PLOTS
############################################################
p1 <- plot_calib(y_test, pred_pars_cal, "Linear Calibration")
p2 <- plot_calib(y_test, pred_woe_cal,  "WOE Calibration")
p3 <- plot_calib(y_test, pred_lgb_cal,  "LightGBM Calibration")
############################################################
## ✅ DISPLAY (1 PANEL)
############################################################

grid.arrange(p1, p2, p3, ncol = 3)


############################################################
## ✅ 5. ASSUMPTION TEST 
############################################################

cat("\n================ ASSUMPTION TEST (PER MODEL) ================\n")

############################################################
## ✅ LINEAR
############################################################
cat("\n========== LINEAR REGRESSION ==========\n")

dev.new(); par(mfrow=c(2,2))

res_lr <- residuals(model_pars)
fit_lr <- fitted(model_pars)

plot(fit_lr, res_lr); abline(h=0,col="red")
qqnorm(res_lr); qqline(res_lr,col="red")
hist(res_lr)
plot(model_pars, which=4)

set.seed(123)
res_sample <- sample(res_lr, min(5000,length(res_lr)))

shapiro_lr <- shapiro.test(res_sample)
bp_lr <- bptest(model_pars)
vif_lr <- vif(model_pars)
dw_lr <- dwtest(model_pars)

cat("\nShapiro:\n"); print(shapiro_lr)
cat("\nBP TEST:\n"); print(bp_lr)
cat("\nVIF:\n"); print(vif_lr)
cat("\nDW TEST:\n"); print(dw_lr)

############################################################
## ✅ WOE
############################################################
cat("\n========== WOE MODEL ==========\n")

dev.new(); par(mfrow=c(2,2))

res_woe <- residuals(model_woe)
fit_woe <- fitted(model_woe)

plot(fit_woe, res_woe); abline(h=0,col="blue")
qqnorm(res_woe); qqline(res_woe,col="blue")
hist(res_woe)
plot(model_woe, which=4)

res_sample_woe <- sample(res_woe, min(5000,length(res_woe)))

shapiro_woe <- shapiro.test(res_sample_woe)
bp_woe <- bptest(model_woe)
vif_woe <- vif(model_woe)
dw_woe <- dwtest(model_woe)

cat("\nShapiro:\n"); print(shapiro_woe)
cat("\nBP TEST:\n"); print(bp_woe)
cat("\nVIF:\n"); print(vif_woe)
cat("\nDW TEST:\n"); print(dw_woe)

############################################################
## ✅ LIGHTGBM
############################################################
cat("\n========== LIGHTGBM ==========\n")

dev.new(); par(mfrow=c(2,2))

res_lgb <- y_test - pred_lgb

plot(pred_lgb, res_lgb); abline(h=0,col="green")
qqnorm(res_lgb); qqline(res_lgb,col="green")
hist(res_lgb)
plot(pred_lgb, y_test); abline(0,1,col="red")

cat("\nRMSE:\n"); print(rmse(y_test, pred_lgb))
cat("\nFeature Importance:\n"); print(lgb.importance(model_lgb))

imp <- lgb.importance(model_lgb)

imp_top <- head(imp, 15)

ggplot(imp_top, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  coord_flip() +
  labs(
    title = "Feature Importance (LightGBM)",
    x = "",
    y = "Importance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )



