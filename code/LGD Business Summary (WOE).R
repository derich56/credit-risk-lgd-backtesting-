###########################################################
## ✅ LIBRARY
############################################################
library(data.table)
library(dplyr)
library(scorecard)
library(ggplot2)
library(scales)
library(here)

set.seed(123)

############################################################
## ✅ STEP 1 — LOAD DATA (STABLE VERSION)
############################################################

# file path (use dialog only once)
file_path <- file.choose()

loan_data <- fread(
  file_path,
  sep = ",",
  quote = "",
  fill = Inf
)

names(loan_data) <- tolower(names(loan_data))

loan_data[, issue_d_date := as.Date(paste0("01-", issue_d), format="%d-%b-%y")]

MRP <- 36

############################################################
## ✅ CLEAN NUMERIC
############################################################
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
## ✅ LGD CALCULATION
############################################################
loan_data[, int_rate := int_rate / 100]
loan_data[, discount_factor := (1 + int_rate / 12)^MRP]

loan_data[, recovery_cf :=
            recoveries + collection_recovery_fee +
            total_rec_prncp + total_rec_int + total_rec_late_fee]

loan_data[, recovery_cf_disc := recovery_cf / discount_factor]

loan_data[, lgd := (funded_amnt - recovery_cf_disc) / funded_amnt]

loan_data[, lgd := fifelse(is.na(lgd) | lgd > 1, 1,
                           fifelse(lgd < 0, 0, lgd))]

loan_data[, utilization := revol_bal / total_rev_hi_lim]
loan_data[, utilization := pmin(utilization, 1)]

############################################################
## ✅ SELECT FEATURES
############################################################
data <- loan_data[, .(
  issue_d_date,
  ead = funded_amnt,
  zip_code,
  grade,
  utilization,
  annual_inc,
  purpose,
  inq_last_6mths,
  mths_since_last_delinq,
  dti,
  lgd
)]

############################################################
## ✅ SPLIT
############################################################
idx <- sample(1:nrow(data))

train <- data[idx[1:floor(0.8*nrow(data))]]
test  <- data[idx[(floor(0.8*nrow(data))+1):nrow(data)]]

############################################################
## ✅ CLEAN MODEL INPUT
############################################################
clean_func <- function(df){
  df %>%
    mutate(zip_group = substr(zip_code,1,1)) %>%
    select(-zip_code,-issue_d_date)
}

train_base <- clean_func(train)
test_base  <- clean_func(test)

top_purpose <- names(sort(table(train_base$purpose), decreasing = TRUE))[1:5]

train_base[, purpose := ifelse(purpose %in% top_purpose, purpose, "other")]
test_base[, purpose  := ifelse(purpose %in% top_purpose, purpose, "other")]

num_cols2 <- names(train_base)[sapply(train_base, is.numeric)]

for(col in num_cols2){
  train_base[[col]][is.na(train_base[[col]])] <- 0
  test_base[[col]][is.na(test_base[[col]])]  <- 0
}

############################################################
## ✅ WOE MODEL
############################################################
features <- c(
  "ead","grade","utilization","annual_inc",
  "purpose","inq_last_6mths","mths_since_last_delinq",
  "dti","zip_group"
)

train_woe_input <- train_base[, c(features,"lgd"), with=FALSE]
test_woe_input  <- test_base[,  c(features,"lgd"), with=FALSE]

threshold <- quantile(train_woe_input$lgd, 0.7)

train_woe_input[, lgd_bin := ifelse(lgd >= threshold, 1, 0)]

bins <- woebin(train_woe_input[1:min(5000,.N)],
               y="lgd_bin", x=features,
               method="chimerge", stop_limit=0.05)

train_woe <- woebin_ply(train_woe_input, bins)
test_woe  <- woebin_ply(test_woe_input, bins)

woe_vars <- paste0(features,"_woe")

model_woe <- lm(
  as.formula(paste("lgd ~", paste(woe_vars, collapse="+"))),
  data=train_woe
)

############################################################
## ✅ PREDICTION
############################################################
pred_woe <- predict(model_woe, test_woe)

pred_woe[is.na(pred_woe)] <- mean(train_woe$lgd)
pred_woe <- pmin(pmax(pred_woe,0),1)

############################################################
## ✅ BUSINESS SUMMARY
############################################################
test_result <- copy(test_base)

test_result[, issue_d_date := test$issue_d_date]

test_result[, pred_lgd := pred_woe]
test_result[, loss := pred_lgd * ead]

############################################################
## ✅ SAVE OUTPUT (STABLE PATH)
############################################################
dir.create(here("outputs"), showWarnings = FALSE)

saveRDS(test_result, here("outputs","test_result.rds"))

cat("✅ File saved at:", here("outputs","test_result.rds"), "\n")

############################################################
## ✅ QUICK CHECK
############################################################
print(head(test_result))

