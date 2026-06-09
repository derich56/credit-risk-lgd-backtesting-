############################################################
## ✅ 0. LIBRARY
############################################################
library(data.table)
library(ggplot2)
library(dplyr)

############################################################
## ✅ 1. LOAD DATA
############################################################
file_path <- file.choose()

loan_data <- fread(
  file_path,
  sep = ",",
  quote = "",
  fill = Inf
)

names(loan_data) <- tolower(names(loan_data))

############################################################
## ✅ 2. CLEAN & CONVERT DATA
############################################################

# numeric conversion (robust)
num_cols <- c(
  "recoveries","collection_recovery_fee",
  "total_rec_prncp","total_rec_int","total_rec_late_fee",
  "funded_amnt","int_rate","revol_bal","total_rev_hi_lim",
  "annual_inc","inq_last_6mths","mths_since_last_delinq","dti"
)

num_cols <- intersect(num_cols, names(loan_data))

loan_data[, (num_cols) := lapply(.SD, function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}), .SDcols=num_cols]

############################################################
## ✅ 3. FILTER CORRUPTED ROWS (CRITICAL FIX)
############################################################

loan_data <- loan_data[
  !is.na(funded_amnt) & funded_amnt > 0 &
    !is.na(int_rate) &
    !is.na(dti) & dti >= 0 & dti <= 60 &
    !is.na(inq_last_6mths) & inq_last_6mths >= 0 & inq_last_6mths <= 20 &
    !is.na(mths_since_last_delinq) & mths_since_last_delinq >= 0 & mths_since_last_delinq <= 200 &
    !is.na(annual_inc) & annual_inc > 0 & annual_inc <= 1e6 &
    grepl("^[0-9]{3}xx$", zip_code)
]

############################################################
## ✅ 4. KEY FEATURE ENGINEERING
############################################################

# ✅ IMPORTANT: NO DISCOUNT (FIX LGD BIAS)
loan_data[, recovery_cf :=
            total_rec_prncp +
            total_rec_int +
            total_rec_late_fee +
            recoveries +
            collection_recovery_fee]

loan_data[, lgd :=
            (funded_amnt - recovery_cf) / funded_amnt]

loan_data[, lgd :=
            fifelse(is.na(lgd) | lgd > 1, 1,
                    fifelse(lgd < 0, 0, lgd))]

loan_data[, utilization := revol_bal / total_rev_hi_lim]
loan_data[, utilization := pmin(utilization, 1)]

# date
loan_data[, issue_d_date := as.Date(paste0("01-", issue_d), format="%d-%b-%y")]

############################################################
## ✅ 5. BASE DATA
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

# remove NA
eda_data <- na.omit(eda_data)

# expected loss
eda_data[, expected_loss := ead * lgd]

############################################################
## ✅ 6. KEY EDA VISUAL
############################################################

# LGD distribution
ggplot(eda_data, aes(x = lgd)) +
  geom_histogram(fill = "#2C7FB8", bins = 30) +
  labs(title = "LGD Distribution") +
  theme_minimal()

# LGD by grade
ggplot(eda_data, aes(x = grade, y = lgd)) +
  geom_boxplot(fill = "#41B6C4") +
  labs(title = "LGD by Grade") +
  theme_minimal()

# LGD vs DTI
ggplot(eda_data[dti <= quantile(dti, 0.99)],
       aes(x = dti, y = lgd)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "red") +
  theme_minimal()

############################################################
## ✅ 7. LOSS ANALYSIS
############################################################

cat("\n========== BIG PICTURE ==========\n")

cat("Total Loan:", nrow(eda_data), "\n")
cat("Average LGD:", mean(eda_data$lgd), "\n")

cat("Total Expected Loss:",
    sum(eda_data$expected_loss), "\n")

############################################################
## ✅ 8. TOP LOSS DRIVER (FIXED OUTPUT)
############################################################

top_loss <- eda_data[order(-expected_loss)][1:20]

cat("\nTop 20 Loss Drivers:\n")
print(top_loss[, .(
  issue_d_date, ead, grade, utilization,
  annual_inc, purpose, zip_code,
  inq_last_6mths, dti, lgd, expected_loss
)])

############################################################
## ✅ 9. PARETO (LOSS CONCENTRATION)
############################################################

eda_data[, el := expected_loss]
setorder(eda_data, -el)

eda_data[, pct_cum := cumsum(el)/sum(el)]

cat("\nLoans driving 80% loss:",
    nrow(eda_data[pct_cum <= 0.8]), "\n")

ggplot(eda_data, aes(x = seq_along(pct_cum), y = pct_cum)) +
  geom_line(color="red") +
  geom_hline(yintercept = 0.8,
             linetype="dashed", color="blue") +
  labs(title = "Loss Concentration (Pareto)") +
  theme_minimal()

############################################################
## ✅ 10. SEGMENTATION (DECILE)
############################################################

eda_data[, decile := ntile(lgd, 10)]

cat("\nLGD by Decile:\n")
print(
  eda_data[,.(avg_lgd=mean(lgd),
              total_loss=sum(el)),
           by=decile][order(-decile)]
)

############################################################
## ✅ 11. FEATURE IMPORTANCE (CLEAN)
############################################################

model <- lm(lgd ~ utilization + log(annual_inc+1) + dti + grade,
            data = eda_data)

importance <- as.data.table(summary(model)$coefficients,
                            keep.rownames = "Feature")

setnames(importance,
         c("Feature","Estimate","StdError","t_value","p_value"))

cat("\nFeature Importance:\n")
print(importance[order(-abs(t_value))])

############################################################
## ✅ 12. LGD BY PURPOSE (FIXED)
############################################################

# filter valid purpose only
valid_purpose <- c(
  "car","credit_card","debt_consolidation",
  "home_improvement","small_business"
)

eda_data_clean <- eda_data[
  purpose %in% valid_purpose
]

ggplot(eda_data_clean,
       aes(x = reorder(purpose, lgd, median),
           y = lgd)) +
  geom_boxplot(fill = "#7FCDBB") +
  coord_flip() +
  labs(title = "LGD by Purpose") +
  theme_minimal()

############################################################
## ✅ END
############################################################