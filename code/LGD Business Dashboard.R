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
## ✅ UI
############################################################
ui <- dashboardPage(
  
  dashboardHeader(title = "LGD Risk Dashboard"),
  
  dashboardSidebar(
    sliderInput(
      "year",
      "Select Year:",
      min = min(data$year, na.rm = TRUE),
      max = max(data$year, na.rm = TRUE),
      value = max(data$year, na.rm = TRUE),
      step = 1,
      sep = ""
    )
  ),
  
  dashboardBody(
    
    ########################################################
    ## KPI
    ########################################################
    fluidRow(
      valueBoxOutput("total_ead", width = 6),
      valueBoxOutput("total_loss", width = 6)
    ),
    
    fluidRow(
      valueBoxOutput("avg_lgd", width = 6),
      valueBoxOutput("recovery", width = 6)
    ),
    
    ########################################################
    ## TREND PLOTS
    ########################################################
    fluidRow(
      box(
        width = 6,
        title = "LGD Trend",
        status = "warning",
        solidHeader = TRUE,
        plotOutput("lgd_trend", height = 300)
      ),
      box(
        width = 6,
        title = "Loss Trend",
        status = "success",
        solidHeader = TRUE,
        plotOutput("loss_trend", height = 300)
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Loss by Grade",
        status = "info",
        solidHeader = TRUE,
        plotOutput("loss_grade_pie", height = 320)
      ),
      box(
        width = 6,
        title = "Loss by Purpose",
        status = "danger",
        solidHeader = TRUE,
        plotOutput("loss_purpose_pie", height = 320)
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Loss by Risk",
        status = "warning",
        solidHeader = TRUE,
        plotOutput("loss_risk_bar", height = 320)
      ),
      box(
        width = 6,
        title = "Average LGD by Grade",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("lgd_grade_bar", height = 320)
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Top 10 Loss Segments (Grade + Purpose)",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("loss_segment_bar", height = 380)
      )
    )
  )
)

############################################################
## ✅ SERVER
############################################################
server <- function(input, output, session){
  
  ########################################################
  ## FILTERED DATA
  ########################################################
  filtered <- reactive({
    req(input$year)
    data[year == input$year]
  })
  
  ########################################################
  ## KPI 
  ########################################################
  output$total_ead <- renderValueBox({
    valueBox(
      format_number(sum(data$ead, na.rm = TRUE)),
      "Total EAD",
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  output$total_loss <- renderValueBox({
    valueBox(
      format_number(sum(data$loss, na.rm = TRUE)),
      "Total Loss",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$avg_lgd <- renderValueBox({
    valueBox(
      percent(mean(data$pred_lgd, na.rm = TRUE), accuracy = 1),
      "Avg LGD",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$recovery <- renderValueBox({
    valueBox(
      percent(1 - mean(data$pred_lgd, na.rm = TRUE), accuracy = 1),
      "Recovery",
      icon = icon("undo"),
      color = "green"
    )
  })
  
  ########################################################
  ## LGD TREND
  ########################################################
  output$lgd_trend <- renderPlot({
    
    df_sum <- data %>%
      group_by(year) %>%
      summarise(avg_lgd = mean(pred_lgd, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_sum, aes(x = year, y = avg_lgd)) +
      geom_line(color = "orange", linewidth = 1.2) +
      geom_point(color = "orange", size = 3) +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(breaks = df_sum$year) +  # biar rapi
      theme_minimal() +
      labs(x = "Year", y = "Average LGD")
  })
  
  ########################################################
  ## LOSS TREND
  ########################################################
  output$loss_trend <- renderPlot({
    
    df_sum <- data %>%
      group_by(year) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_sum, aes(x = year, y = total_loss)) +
      geom_line(color = "green", linewidth = 1.2) +
      geom_point(color = "green", size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      scale_x_continuous(breaks = df_sum$year) +
      theme_minimal() +
      labs(x = "Year", y = "Total Loss")
  })
  ########################################################
  ## LOSS BY GRADE
  ########################################################
  output$loss_grade_pie <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    pie_df <- df %>%
      group_by(grade) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_loss)) %>%   # <-- penting
      mutate(
        pct = total_loss / sum(total_loss),
        grade_label = paste0(grade, " (", percent(pct, accuracy = 0.1), ")")
      )
    
    pie_df$grade_label <- factor(pie_df$grade_label, levels = pie_df$grade_label)
    
    ggplot(pie_df, aes(x = "", y = total_loss, fill = grade_label)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Grade")
  })
  
  ########################################################
  ## LOSS BY PURPOSE
  ########################################################
  output$loss_purpose_pie <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    pie_df <- df %>%
      group_by(purpose) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_loss))
    
    pie_df$purpose <- factor(pie_df$purpose, levels = pie_df$purpose)
    ggplot(pie_df, aes(x = "", y = total_loss, fill = purpose)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  ########################################################
  ## LOSS BY RISK
  ########################################################
  output$loss_risk_bar <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    bar_df <- df %>%
      group_by(risk_bucket) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop")
    
    bar_df$risk_bucket <- factor(
      bar_df$risk_bucket,
      levels = c("High", "Medium", "Low")
    )
    
    ggplot(bar_df, aes(x = risk_bucket, y = total_loss, fill = risk_bucket)) +
      geom_col() +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
      labs(x = "Risk Bucket", y = "Total Loss") +
      theme_minimal()
  })
  
  ########################################################
  ## AVG LGD BY GRADE
  ########################################################
  output$lgd_grade_bar <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    bar_df <- df %>%
      group_by(grade) %>%
      summarise(avg_lgd = mean(pred_lgd, na.rm = TRUE), .groups = "drop")
    
    ggplot(bar_df, aes(x = grade, y = avg_lgd, fill = grade)) +
      geom_col() +
      scale_y_continuous(labels = percent) +
      theme_minimal() +
      labs(x = "Grade", y = "Average LGD")
  })
  
  ########################################################
  ## TOP 10 LOSS SEGMENTS
  ########################################################
  output$loss_segment_bar <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    seg_df <- df %>%
      mutate(segment = paste(grade, purpose, sep = " - ")) %>%
      group_by(segment) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_loss)) %>%
      slice_head(n = 10) %>%
      mutate(loss_pct = total_loss / sum(total_loss))
    
    ggplot(seg_df, aes(x = reorder(segment, loss_pct), y = loss_pct)) +
      geom_col(fill = "#2C7FB8", show.legend = FALSE) +
        geom_text(
        aes(label = percent(loss_pct, accuracy = 0.1)),
        hjust = -0.1,  
        size = 4
      ) +
      coord_flip() +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
      theme_minimal() +
      labs(x = "Segment", y = "Loss %")
  })
}

############################################################
## ✅ RUN
############################################################
shinyApp(ui, server)

