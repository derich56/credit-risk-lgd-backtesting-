############################################################
## ✅ LIBRARY
############################################################
library(data.table)
library(dplyr)
library(scorecard)
library(ggplot2)
library(scales)
set.seed(123)

############################################################
## ✅ STEP 1 — LOAD & PREPARE DATA
############################################################
loan_data <- fread(file.choose())
names(loan_data) <- tolower(names(loan_data))

MRP <- 36

loan_data[, int_rate := int_rate/100]
loan_data[, discount_factor := (1 + int_rate/12)^MRP]

loan_data[, recovery_cf :=
            recoveries + collection_recovery_fee +
            total_rec_prncp + total_rec_int + total_rec_late_fee]

loan_data[, recovery_cf_disc := recovery_cf / discount_factor]

loan_data[, lgd := (funded_amnt - recovery_cf_disc)/funded_amnt]

loan_data[, lgd := fifelse(is.na(lgd) | lgd > 1, 1,
                           fifelse(lgd < 0, 0, lgd))]

loan_data[, utilization := revol_bal/total_rev_hi_lim]
loan_data[, utilization := fifelse(utilization > 1, 1, utilization)]

############################################################
## ✅ STEP 2 — SELECT FEATURES
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

setorder(data, issue_d_date)

############################################################
## ✅ STEP 3 — TRAIN TEST SPLIT (TIME BASED)
############################################################
cut <- floor(0.8 * nrow(data))
cut_date <- data[cut, issue_d_date]

train <- data[issue_d_date <= cut_date]
test  <- data[issue_d_date > cut_date]

############################################################
## ✅ STEP 4 — CLEANING
############################################################
clean_func <- function(df) {
  df %>%
    mutate(zip_group = substr(zip_code, 1, 2)) %>%
    select(-zip_code, -issue_d_date) %>%
    na.omit()
}

train_base <- clean_func(train)
test_base  <- clean_func(test)

############################################################
## ✅ STEP 5 — WOE MODEL
############################################################
features <- c(
  "ead","grade","utilization",
  "annual_inc","purpose",
  "inq_last_6mths",
  "mths_since_last_delinq",
  "dti","zip_group"
)

train_woe_input <- train_base[, c(features,"lgd"), with=FALSE]
test_woe_input  <- test_base[,  c(features,"lgd"), with=FALSE]

# Binary target (for binning only)
threshold <- quantile(train_woe_input$lgd, 0.7)
train_woe_input[, lgd_bin := ifelse(lgd >= threshold, 1, 0)]

# Binning
bins <- woebin(
  train_woe_input,
  y="lgd_bin",
  x=features,
  method="tree",
  stop_limit=0.05
)

# Transform to WOE
train_woe <- woebin_ply(train_woe_input, bins)
test_woe  <- woebin_ply(test_woe_input, bins)

woe_vars <- paste0(features, "_woe")

# Model
model_woe <- lm(
  as.formula(paste("lgd ~", paste(woe_vars, collapse="+"))),
  data=train_woe
)

############################################################
## ✅ STEP 6 — PREDICTION
############################################################
pred_woe <- predict(model_woe, test_woe)

pred_woe[is.na(pred_woe)] <- mean(train_woe$lgd)
pred_woe <- pmin(pmax(pred_woe, 0), 1)

############################################################
## ✅ STEP 7 — BUSINESS SUMMARY
############################################################

test_result <- copy(test_base)
test_result[, pred_lgd := pred_woe]

# Expected Loss
test_result[, loss := pred_lgd * ead]

############################################################
## ✅ KPI PORTFOLIO
############################################################
lgd_summary <- data.table(
  Avg_LGD        = mean(test_result$pred_lgd),
  Recovery_Rate  = 1 - mean(test_result$pred_lgd),
  Total_EAD      = sum(test_result$ead),
  Expected_Loss  = sum(test_result$loss)
)

cat("\n================ LGD SUMMARY ================\n")
print(lgd_summary)

############################################################
## ✅ DRIVER ANALYSIS
############################################################
coef_table <- data.table(
  Feature = gsub("_woe", "", names(coef(model_woe))),
  Coefficient = coef(model_woe)
)

coef_table <- coef_table[Feature != "(Intercept)"]

driver_table <- coef_table[order(-abs(Coefficient))]

cat("\n================ LGD DRIVERS ================\n")
print(driver_table)

############################################################
## ✅ SEGMENTATION ANALYSIS
############################################################

## ✅ LOSS BY GRADE
seg_grade <- test_result[, .(
  Avg_LGD   = mean(pred_lgd),
  Total_EAD = sum(ead),
  Total_Loss = sum(loss)
), by=grade][order(-Total_Loss)]

cat("\n================ LOSS BY GRADE ================\n")
print(seg_grade)

## ✅ LOSS BY PURPOSE
seg_purpose <- test_result[, .(
  Avg_LGD   = mean(pred_lgd),
  Total_EAD = sum(ead),
  Total_Loss = sum(loss)
), by=purpose][order(-Total_Loss)]

cat("\n================ LOSS BY PURPOSE ================\n")
print(seg_purpose)

## ✅ RISK SEGMENT
test_result[, risk_bucket :=
              fifelse(pred_lgd > 0.6, "High",
                      fifelse(pred_lgd > 0.3, "Medium", "Low"))]

seg_risk <- test_result[, .(
  Avg_LGD   = mean(pred_lgd),
  Total_EAD = sum(ead),
  Total_Loss = sum(loss),
  Loss_Rate = sum(loss) / sum(ead)
), by=risk_bucket][order(-Total_Loss)]

seg_grade <- seg_grade %>%
  mutate(
    loss_jt = Total_Loss / 1e6,
    perc = Total_Loss / sum(Total_Loss)
  )


cat("\n================ LOSS BY RISK SEGMENT ================\n")
print(seg_risk)

## ✅ TOP LOSS SEGMENT (GRADE + PURPOSE)
seg_combo <- test_result[, .(
  Total_Loss = sum(loss),
  Avg_LGD = mean(pred_lgd),
  Total_EAD = sum(ead)
), by=.(grade, purpose)][order(-Total_Loss)]

cat("\n================ TOP 10 LOSS SEGMENTS ================\n")
print(head(seg_combo, 10))

############################################################
## ✅ VISUALIZATION
############################################################

############################################################
## 1. KONTRIBUSI KERUGIAN PER CREDIT GRADE (PIE CHART)
############################################################
seg_grade_plot <- seg_grade %>%
  mutate(
    perc = Total_Loss / sum(Total_Loss),
    legend_label = paste0(grade, " (", percent(perc), ")")
  ) %>%
  arrange(desc(Total_Loss))

ggplot(seg_grade_plot,
       aes(x = "", y = perc,
           fill = factor(legend_label, levels = legend_label))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Kontribusi Kerugian per Credit Grade",
    fill = "Grade"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


############################################################
## 2. FAKTOR PENDORONG LGD (DRIVER IMPORTANCE)
############################################################
driver_plot <- coef_table %>%
  mutate(
    value = abs(Coefficient),
    Feature = ifelse(Feature == "Others", "lainnya", Feature)
  ) %>%
  arrange(desc(value)) %>%
  mutate(
    rank = row_number(),
    Feature = ifelse(rank <= 6, Feature, "lainnya")
  ) %>%
  group_by(Feature) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(perc = value / sum(value)) %>%
  arrange(desc(value)) %>%
  mutate(Feature = factor(Feature, levels = Feature))

ggplot(driver_plot,
       aes(x = "", y = perc, fill = Feature)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = percent(perc)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(
    title = "Faktor Pendorong Tingginya LGD",
    fill = "Variabel"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


############################################################
## 3. DISTRIBUSI KERUGIAN BERDASARKAN PURPOSE (TOP 6)
############################################################
seg_purpose_plot <- seg_purpose %>%
  arrange(desc(Total_Loss)) %>%
  slice(1:6) %>%
  mutate(
    perc = Total_Loss / sum(Total_Loss),
    legend_label = paste0(purpose, " (", percent(perc), ")")
  )

ggplot(seg_purpose_plot,
       aes(x = "", y = perc,
           fill = factor(legend_label, levels = legend_label))) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(
    title = "Distribusi Kerugian berdasarkan Purpose (Top 6)",
    fill = "Purpose"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


############################################################
## 4. DISTRIBUSI KERUGIAN BERDASARKAN RISK SEGMENT
############################################################
seg_risk_plot <- seg_risk %>%
  mutate(
    perc = Total_Loss / sum(Total_Loss),
    legend_label = paste0(risk_bucket, " (", percent(perc), ")")
  ) %>%
  arrange(desc(Total_Loss))

ggplot(seg_risk_plot,
       aes(x = "", y = perc,
           fill = factor(legend_label, levels = legend_label))) +
  geom_col(width = 1) +
  coord_polar("y") +
  labs(
    title = "Distribusi Kerugian berdasarkan Risk Segment",
    fill = "Risk Segment"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


############################################################
## 5. TREND LGD PER CREDIT GRADE (LINE CHART)
############################################################
lgd_grade <- test_result[, .(
  Avg_LGD = mean(pred_lgd)
), by = grade]

lgd_grade$grade <- factor(lgd_grade$grade,
                          levels = c("A","B","C","D","E","F","G"))

ggplot(lgd_grade,
       aes(x = grade, y = Avg_LGD, group = 1)) +
  geom_line(color = "#D7301F", linewidth = 1) +
  geom_point(color = "#D7301F", size = 3) +
  labs(
    title = "Trend LGD per Credit Grade",
    x = "Credit Grade",
    y = "Rata-rata LGD"
  ) +
  theme_minimal()


############################################################
## 6. TOP 10 SEGMENT KERUGIAN (GRADE + PURPOSE)
############################################################
top10_plot <- seg_combo %>%
  head(10) %>%
  mutate(label = paste(grade, "-", purpose),
         perc = Total_Loss / sum(Total_Loss)  
  )

ggplot(top10_plot,
       aes(x = reorder(label, Total_Loss),
           y = Total_Loss)) +
  geom_col(fill = "#E41A1C") +
  coord_flip() +
  geom_text(aes(label = percent(perc)),
            hjust = -0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  
  labs(
    title = "Top 10 Segment Kerugian (Grade + Purpose)",
    x = "Segment",
    y = "Total Loss"
  ) +
  theme_minimal()


