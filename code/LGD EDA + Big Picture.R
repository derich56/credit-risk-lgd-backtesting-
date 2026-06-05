############################################################
## ✅ 0. LIBRARY
############################################################
library(data.table)
library(ggplot2)
library(dplyr)

############################################################
## ✅ 1. LOAD DATA
############################################################
loan_data <- fread(file.choose())
names(loan_data) <- tolower(names(loan_data))

############################################################
## ✅ 2. FEATURE ENGINEERING (CORE)
############################################################

MRP <- 36

loan_data[, int_rate := int_rate/100]
loan_data[, discount_factor := (1 + int_rate/12)^MRP]

loan_data[, recovery_cf :=
            recoveries + collection_recovery_fee +
            total_rec_prncp + total_rec_int + total_rec_late_fee]

loan_data[, recovery_cf_disc := recovery_cf / discount_factor]

## ✅ LGD (CORE VARIABLE)
loan_data[, lgd := fifelse(funded_amnt > 0,
                           (funded_amnt - recovery_cf_disc)/funded_amnt,
                           NA_real_)]

loan_data[, lgd := fifelse(is.na(lgd)|lgd>1,1,
                           fifelse(lgd<0,0,lgd))]

## ✅ UTILIZATION (BEHAVIOR)
loan_data[, utilization :=
            fifelse(total_rev_hi_lim > 0,
                    revol_bal/total_rev_hi_lim,
                    NA_real_)]

loan_data[, utilization := fifelse(utilization>1,1,utilization)]

############################################################
## ✅ 3. BASE DATA (ONLY IMPORTANT FEATURES)
############################################################
eda_data <- loan_data[,.(issue_d_date,
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

## ✅ CORE METRIC (WAJIB)
eda_data[, expected_loss := ead * lgd]

############################################################
## ✅ 4. KEY EDA (ONLY IMPORTANT)
############################################################

## ✅ 1. LGD DISTRIBUTION
ggplot(eda_data, aes(x = lgd)) +
  geom_histogram(fill = "#2C7FB8", bins = 30) +
  labs(title = "LGD Distribution") +
  theme_minimal()

## ✅ 2. LGD BY GRADE (STRONGEST DRIVER)
ggplot(eda_data, aes(x = grade, y = lgd)) +
  geom_boxplot(fill = "#41B6C4") +
  labs(title = "LGD by Grade") +
  theme_minimal()

## ✅ 3. LGD VS DTI (DEBT BURDEN)
ggplot(eda_data[dti <= quantile(dti, 0.99, na.rm=TRUE)],
       aes(x = dti, y = lgd)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "red") +
  theme_minimal()

############################################################
## ✅ 5. LOSS ANALYSIS (MOST IMPORTANT)
############################################################

cat("\n========== BIG PICTURE ==========\n")

cat("Total Loan:", nrow(eda_data), "\n")
cat("Average LGD:", mean(eda_data$lgd, na.rm=TRUE), "\n")

## ✅ TOTAL LOSS
cat("Total Expected Loss:",
    sum(eda_data$expected_loss, na.rm=TRUE), "\n")

## ✅ TOP LOSS DRIVER (MOST IMPORTANT INSIGHT)
top_loss <- eda_data[order(-expected_loss)][1:20]

cat("\nTop 20 Loss Drivers:\n")
print(top_loss)

############################################################
## ✅ 6. PARETO (LOSS CONCENTRATION)
############################################################

eda_data[, el := expected_loss]
setorder(eda_data, -el)

eda_data[, pct_cum := cumsum(el)/sum(el)]

cat("\nLoans driving 80% loss:",
    nrow(eda_data[pct_cum <= 0.8]), "\n")

## ✅ VISUAL
ggplot(eda_data, aes(x = seq_along(pct_cum), y = pct_cum)) +
  geom_line(color="red") +
  geom_hline(yintercept = 0.8, linetype="dashed", color="blue") +
  labs(title = "Loss Concentration (Pareto)") +
  theme_minimal()

############################################################
## ✅ 7. SEGMENTATION (DECILE)
############################################################

eda_data[, decile := ntile(lgd, 10)]

cat("\nLGD by Decile:\n")
print(eda_data[,.(avg_lgd=mean(lgd),
                  total_loss=sum(el)),
               by=decile][order(-decile)])

############################################################
## ✅ 8. FEATURE IMPORTANCE (KEEP SIMPLE)
############################################################

model <- lm(lgd ~ utilization + log(annual_inc+1) + dti + grade,
            data = eda_data)

importance <- as.data.table(summary(model)$coefficients,
                            keep.rownames = "Feature")

setnames(importance, c("Feature","Estimate","StdError","t_value","p_value"))

cat("\nFeature Importance:\n")
print(importance[order(-abs(t_value))])

############################################################
## ✅ 9.  LGD  By  Purpose
############################################################
ggplot(eda_data, aes(x = reorder(purpose, lgd, FUN = median), y = lgd)) +
  geom_boxplot(fill = "#7FCDBB") +
  coord_flip() +
  labs(title = "LGD by Purpose") +
  theme_minimal()



############################################################
## ✅ END
############################################################
