############################################################
## ✅ LIBRARY
############################################################
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(scales)
library(ggplot2)

############################################################
## ✅ LOAD DATA
############################################################
data <- readRDS("outputs/test_result.rds")
setDT(data)

############################################################
## ✅ FIX DATE
############################################################
data[, issue_d_date := as.character(issue_d_date)]

data[, issue_d_date := as.Date(
  issue_d_date,
  tryFormats = c(
    "%Y-%m-%d",
    "%d-%b-%Y",
    "%d-%b-%y",
    "%m/%d/%Y"
  )
)]

data <- data[!is.na(issue_d_date)]

############################################################
## ✅ FEATURE ENGINEERING
############################################################
data[, year := as.numeric(format(issue_d_date, "%Y"))]

data[, risk_bucket := fifelse(
  pred_lgd > 0.6, "High",
  fifelse(pred_lgd > 0.3, "Medium", "Low")
)]

############################################################
## ✅ FORMAT FUNCTION
############################################################
format_number <- function(x){
  if(is.na(x)) return("0")
  
  if(x >= 1e9){
    paste0(round(x / 1e9, 1), " B")
  } else if(x >= 1e6){
    paste0(round(x / 1e6, 1), " M")
  } else if(x >= 1e3){
    paste0(round(x / 1e3, 1), " K")
  } else {
    as.character(round(x, 0))
  }
}

############################################################
## ✅ COLOR PALETTE
############################################################
col_loss <- "#E74C3C"
col_lgd  <- "#F39C12"
col_high <- "#D73027"
col_medium <- "#FDAE61"
col_low <- "#66BD63"

############################################################
## ✅ HELPER: PREVIOUS PERIOD & TREND BADGE
############################################################
get_previous_period <- function(df, start_year, end_year){
  period_len <- end_year - start_year + 1
  prev_end   <- start_year - 1
  prev_start <- prev_end - period_len + 1
  
  df[df$year >= prev_start & df$year <= prev_end, ]
}

make_trend_badge <- function(current, previous, higher_is_better = TRUE, is_percentage_metric = FALSE){
  
  if(is.null(previous) || length(previous) == 0 || is.na(previous) || previous == 0){
    return('<div class="trend-badge trend-neutral">• No baseline</div>')
  }
  
  change_abs <- current - previous
  change_rel <- (current - previous) / abs(previous)
  
  # arah panah
  if(change_rel > 0){
    arrow <- "▲"
    cls <- if(higher_is_better) "trend-positive" else "trend-negative"
  } else if(change_rel < 0){
    arrow <- "▼"
    cls <- if(higher_is_better) "trend-negative" else "trend-positive"
  } else {
    arrow <- "•"
    cls <- "trend-neutral"
  }
  
    if(is_percentage_metric){
    
    change_pts <- change_abs * 100  # convert ke percentage points
    
    label <- paste0(
      arrow, " ",
      round(abs(change_pts),1), " pts"
    )
    
  } else if(abs(change_rel) > 1){
    
    ratio <- current / previous
    
    label <- paste0(
      arrow, " ",
      round(ratio,1), "x ",
      "(+", format_number(abs(change_abs)), ")"
    )
    
  } else {
    
    label <- paste0(
      arrow, " ",
      format_number(abs(change_abs)),
      " (", percent(abs(change_rel), accuracy = 0.1), ")"
    )
  }
  
  sprintf('<div class="trend-badge %s">%s</div>', cls, label)
}


############################################################
## ✅ UI
############################################################
ui <- dashboardPage(
  
  dashboardHeader(title = "LGD Risk Dashboard"),
  
  dashboardSidebar(
    
    sliderInput(
      "year_range",
      "Select Year Range:",
      min = min(data$year, na.rm = TRUE),
      max = max(data$year, na.rm = TRUE),
      value = c(min(data$year), max(data$year)),
      step = 1,
      sep = ""
    ),
    
    helpText("All metrics are cumulative within selected range")
  ),
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        background-color: #f5f7fb;
      }

      .skin-blue .main-header .logo {
        background-color: #2f80c1 !important;
        font-weight: 600;
      }

      .skin-blue .main-header .navbar {
        background-color: #2f80c1 !important;
      }

      .main-sidebar {
        background: linear-gradient(180deg, #1f2d3d 0%, #16222d 100%) !important;
      }

      .box {
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
      }

      .small-box {
        border-radius: 14px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.10);
      }

      .trend-badge {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
        margin-top: 6px;
      }

      .trend-positive {
        background: rgba(39, 174, 96, 0.18);
        color: #1e8449;
      }

      .trend-negative {
        background: rgba(231, 76, 60, 0.18);
        color: #c0392b;
      }

      .trend-neutral {
        background: rgba(108, 117, 125, 0.15);
        color: #5f6b7a;
      }

      .takeaway-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 14px;
      }

      .takeaway-card {
        background: #ffffff;
        border-radius: 12px;
        padding: 14px 16px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
        border-left: 5px solid #2f80c1;
      }
    "))
    ),
    ## KPI
    fluidRow(
      valueBoxOutput("total_ead", width = 6),
      valueBoxOutput("total_loss", width = 6)
    ),
    
    fluidRow(
      valueBoxOutput("avg_lgd", width = 6),
      valueBoxOutput("recovery", width = 6)
    ), 
    
    ## Key Takeaway
    fluidRow(
      box(
        width = 12,
        title = "Key Takeaways",
        status = "primary",
        solidHeader = TRUE,
        uiOutput("key_takeaways")   
      )
    ),
    
    
    ## TREND (FULL DATA)
    fluidRow(
      box(width = 6, title = "Average LGD Trend", status = "warning",
          plotOutput("lgd_trend", height = 300)),
      
      box(width = 6, title = "Total Loss Trend", status = "danger",
          plotOutput("loss_trend", height = 300))
    ),
    
    ## BREAKDOWN
    fluidRow(
      box(width = 6, title = "Total Loss by Grade",
          plotOutput("loss_grade_bar", height = 320)),
      
      box(width = 6, title = "Total Loss by Purpose",
          plotOutput("loss_purpose_bar", height = 320))
    ),
    
    fluidRow(
      box(width = 6, title = "Total Loss by Risk",
          plotOutput("loss_risk_bar", height = 320)),
      
      box(width = 6, title = "Average LGD by Grade",
          plotOutput("lgd_grade_bar", height = 320))
    ),
    
    fluidRow(
      box(width = 12, title = "Top 10 Loss Segments",
          plotOutput("loss_segment_bar", height = 400))
    )
  )
)

############################################################
## ✅ SERVER
############################################################
server <- function(input, output, session){
  
  ########################################################
  ## FILTER (RANGE CUMULATIVE)
  ########################################################
  filtered <- reactive({
    req(input$year_range)
    
    data[
      year >= input$year_range[1] &
        year <= input$year_range[2]
    ]
  })
  
  ########################################################
  ## KPI (WITH TREND INDICATOR)
  ########################################################
  output$total_ead <- renderValueBox({
    df <- filtered()
    prev_df <- get_previous_period(data, input$year_range[1], input$year_range[2])
    
    current_val <- sum(df$ead, na.rm = TRUE)
    prev_val    <- sum(prev_df$ead, na.rm = TRUE)
    
    valueBox(
      format_number(current_val),
      HTML(paste0(
        "Total EAD (", input$year_range[1], "–", input$year_range[2], ")<br/>",
        
        make_trend_badge(current_val, prev_val, higher_is_better = TRUE),
        
        "<div style='font-size:11px;opacity:0.7;'>vs previous period (",
        input$year_range[1] - (input$year_range[2] - input$year_range[1] + 1),
        "–",
        input$year_range[1] - 1,
        ")</div>"
        
      )),
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  output$total_loss <- renderValueBox({
    df <- filtered()
    prev_df <- get_previous_period(data, input$year_range[1], input$year_range[2])
    
    current_val <- sum(df$loss, na.rm = TRUE)
    prev_val    <- sum(prev_df$loss, na.rm = TRUE)
    
    valueBox(
      format_number(current_val),
      HTML(paste0(
        "Total Loss (", input$year_range[1], "–", input$year_range[2], ")<br/>",
        
        make_trend_badge(current_val, prev_val, higher_is_better = FALSE),
        
        "<div style='font-size:11px;opacity:0.7;'>vs previous period (",
        input$year_range[1] - (input$year_range[2] - input$year_range[1] + 1),
        "–",
        input$year_range[1] - 1,
        ")</div>"
        
      )),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$avg_lgd <- renderValueBox({
    df <- filtered()
    prev_df <- get_previous_period(data, input$year_range[1], input$year_range[2])
    
    current_val <- mean(df$pred_lgd, na.rm = TRUE)
    prev_val    <- mean(prev_df$pred_lgd, na.rm = TRUE)
    
    valueBox(
      percent(current_val, accuracy = 0.1),
      HTML(paste0(
        "Avg LGD (", input$year_range[1], "–", input$year_range[2], ")<br/>",
        
        make_trend_badge(current_val, prev_val, higher_is_better = FALSE, is_percentage_metric = TRUE),
        
        "<div style='font-size:11px;opacity:0.7;'>vs previous period (",
        input$year_range[1] - (input$year_range[2] - input$year_range[1] + 1),
        "–",
        input$year_range[1] - 1,
        ")</div>"
        
      )),
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$recovery <- renderValueBox({
    df <- filtered()
    prev_df <- get_previous_period(data, input$year_range[1], input$year_range[2])
    
    current_val <- 1 - mean(df$pred_lgd, na.rm = TRUE)
    prev_val    <- 1 - mean(prev_df$pred_lgd, na.rm = TRUE)
    
    valueBox(
      percent(current_val, accuracy = 0.1),
      HTML(paste0(
        "Recovery (", input$year_range[1], "–", input$year_range[2], ")<br/>",
        
        make_trend_badge(current_val, prev_val, higher_is_better = TRUE, is_percentage_metric = TRUE),
        
        "<div style='font-size:11px;opacity:0.7;'>vs previous period (",
        input$year_range[1] - (input$year_range[2] - input$year_range[1] + 1),
        "–",
        input$year_range[1] - 1,
        ")</div>"
        
      )),
      icon = icon("undo"),
      color = "green"
    )
  })
  
  
  
  ########################################################
  ## ✅ KEY TAKEAWAYS (VISUAL CARD STYLE)
  ########################################################
  output$key_takeaways <- renderUI({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    # ---- Risk contribution ----
    risk_df <- df %>%
      group_by(risk_bucket) %>%
      summarise(loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(loss))
    
    total_loss_all <- sum(risk_df$loss, na.rm = TRUE)
    top_risk <- risk_df$risk_bucket[1]
    top_risk_pct <- round(100 * risk_df$loss[1] / total_loss_all, 1)
    
    # ---- Purpose ----
    purpose_df <- df %>%
      group_by(purpose) %>%
      summarise(loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(loss))
    
    top_purpose <- purpose_df$purpose[1]
    top_purpose_pct <- round(100 * purpose_df$loss[1] / sum(purpose_df$loss, na.rm = TRUE), 1)
    
    # ---- LGD Trend ----
    trend_df <- data %>%
      group_by(year) %>%
      summarise(avg_lgd = mean(pred_lgd, na.rm = TRUE), .groups = "drop")
    
    lgd_trend <- ifelse(
      tail(trend_df$avg_lgd, 1) > head(trend_df$avg_lgd, 1),
      "increased",
      "decreased"
    )
    
    start_year <- min(trend_df$year, na.rm = TRUE)
    end_year   <- max(trend_df$year, na.rm = TRUE)
    
    # ---- Grade ----
    grade_df <- df %>%
      group_by(grade) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_loss))
    
    worst_grade <- grade_df$grade[1]
    worst_grade_pct <- round(100 * grade_df$total_loss[1] / sum(grade_df$total_loss, na.rm = TRUE), 1)
    
    tags$div(
      class = "takeaway-grid",
      
      tags$div(
        class = "takeaway-card danger",
        tags$div(class = "takeaway-title", "Dominant Risk"),
        tags$div(class = "takeaway-value",
                 paste0(top_risk, " risk contributes ~", top_risk_pct, "% of total loss")),
        tags$div(class = "takeaway-sub", "Largest concentration of loss is coming from this bucket.")
      ),
      
      tags$div(
        class = "takeaway-card warning",
        tags$div(class = "takeaway-title", "Main Loss Driver"),
        tags$div(class = "takeaway-value",
                 paste0("'", top_purpose, "' drives ~", top_purpose_pct, "% of total loss")),
        tags$div(class = "takeaway-sub", "This purpose category has the biggest impact on loss.")
      ),
      
      tags$div(
        class = "takeaway-card primary",
        tags$div(class = "takeaway-title", "LGD Direction"),
        tags$div(class = "takeaway-value",
                 paste0("Average LGD has ", lgd_trend, " from ", start_year, " to ", end_year)),
        tags$div(class = "takeaway-sub", "Useful as a quick strategic trend summary.")
      ),
      
      tags$div(
        class = "takeaway-card success",
        tags$div(class = "takeaway-title", "Worst Loss Grade"),
        tags$div(class = "takeaway-value",
                 paste0("Grade ", worst_grade, " contributes ~", worst_grade_pct, "% of total loss")),
        tags$div(class = "takeaway-sub", "This grade currently has the highest loss concentration.")
      )
    )
  })
  
  ########################################################
  ## TREND (FULL HISTORY)
  ########################################################
  output$lgd_trend <- renderPlot({
    df_sum <- data %>%
      group_by(year) %>%
      summarise(avg_lgd = mean(pred_lgd, na.rm = TRUE))
    
    ggplot(df_sum, aes(year, avg_lgd)) +
      geom_line(color = col_lgd, linewidth = 1.2) +
      geom_point(color = col_lgd, size = 3) +
      scale_y_continuous(labels = percent) +
      labs(
        x = "Year",
        y = "Average LGD (%)"
      ) +
      theme_minimal()
  })
  
  output$loss_trend <- renderPlot({
    df_sum <- data %>%
      group_by(year) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE))
    
    ggplot(df_sum, aes(year, total_loss)) +
      geom_line(color = col_loss, linewidth = 1.2) +
      geom_point(color = col_loss, size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      labs(
        x = "Year",
        y = "Total Loss (Million)"
      ) +
      theme_minimal()
  })
  
  ########################################################
  ## LOSS BY GRADE
  ########################################################
  output$loss_grade_bar <- renderPlot({
    df <- filtered()
    
    bar_df <- df %>%
      group_by(grade) %>%
      summarise(total_loss = sum(loss))
    
    ggplot(bar_df, aes(reorder(grade, total_loss), total_loss)) +
      geom_col(fill = "#16B5D8") +
      coord_flip() +
      geom_text(
        aes(label = label_number(scale = 1e-6, suffix = " M")(total_loss)),
        hjust = -0.1
      ) +
      scale_y_continuous(
        labels = label_number(scale = 1e-6, suffix = " M")
        ,
        expand = expansion(mult = c(0, 0.2))
        
      ) +
      labs(
        x = "Grade",
        y = "Total Loss (Million)"
      ) +
      theme_minimal()
    })
    
  ########################################################
  ## LOSS BY PURPOSE
  ########################################################
  output$loss_purpose_bar <- renderPlot({
    df <- filtered()
    
    bar_df <- df %>%
      group_by(purpose) %>%
      summarise(total_loss = sum(loss))
    
    ggplot(bar_df, aes(reorder(purpose, total_loss), total_loss)) +
      geom_col(fill = col_loss) +
      coord_flip() +
      geom_text(
        aes(label = label_number(scale = 1e-6, suffix = " M")(total_loss)),
        hjust = -0.1
      ) +
      scale_y_continuous(
        labels = label_number(scale = 1e-6, suffix = " M"),
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        x = "Purpose",
        y = "Total Loss (Million)"
      ) +
      theme_minimal()
  })
  
  ########################################################
  ## LOSS BY RISK
  ########################################################
  output$loss_risk_bar <- renderPlot({
    df <- filtered()
    
    bar_df <- df %>%
      group_by(risk_bucket) %>%
      summarise(total_loss = sum(loss))
    
    bar_df$risk_bucket <- factor(
      bar_df$risk_bucket,
      levels = c("High", "Medium", "Low")
    )
    
    ggplot(bar_df, aes(risk_bucket, total_loss, fill = risk_bucket)) +
      geom_col() +
      geom_text(
        aes(label = label_number(scale = 1e-6, suffix = " M")(total_loss)),
        vjust = -0.4
      ) +
      scale_fill_manual(values = c(
        "High" = col_high,
        "Medium" = col_medium,
        "Low" = col_low
      )) +
      scale_y_continuous(
        labels = label_number(scale = 1e-6, suffix = " M"),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        x = "Risk",
        y = "Total Loss (Million)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  ########################################################
  ## AVG LGD BY GRADE
  ########################################################
  output$lgd_grade_bar <- renderPlot({
    df <- filtered()
    
    bar_df <- df %>%
      group_by(grade) %>%
      summarise(avg_lgd = mean(pred_lgd))
    
    grade_levels <- sort(unique(bar_df$grade))
    
    colors <- colorRampPalette(c(
      "#D9F2D9",
      "#F6E8A6",
      "#F4A261",
      "#8B0000"
    ))(length(grade_levels))
    
    names(colors) <- grade_levels
    
    ggplot(bar_df, aes(grade, avg_lgd, fill = grade)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = colors) +
      scale_y_continuous(labels = percent) +
      labs(
        x = "Grade",
        y = "Average LGD (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  ########################################################
  ## TOP SEGMENT
  ########################################################
  output$loss_segment_bar <- renderPlot({
    df <- filtered()
    
    seg_df <- df %>%
      mutate(segment = paste(grade, purpose)) %>%
      group_by(segment) %>%
      summarise(loss = sum(loss)) %>%
      arrange(desc(loss)) %>%
      head(10)
    
    
    ggplot(seg_df, aes(reorder(segment, loss), loss)) +
      geom_col(fill = "#5B8DB8") + 
      geom_text(
        aes(label = label_number(scale = 1e-6, suffix = " M")(loss)),
        hjust = -0.1
      ) +
      coord_flip() +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")
                         ,
                         expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        x = "Segment",
        y = "Total Loss (Million)"
      ) +
      theme_minimal()
  })
}

############################################################
## ✅ RUN
############################################################
shinyApp(ui, server)

