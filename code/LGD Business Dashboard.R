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
  ## KPI
  ########################################################
  output$total_ead <- renderValueBox({
    df <- filtered()
    valueBox(
      format_number(sum(df$ead, na.rm = TRUE)),
      paste0("Total EAD (", input$year_range[1], "–", input$year_range[2], ")"),
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  output$total_loss <- renderValueBox({
    df <- filtered()
    valueBox(
      format_number(sum(df$loss, na.rm = TRUE)),
      paste0("Total Loss (", input$year_range[1], "–", input$year_range[2], ")"),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$avg_lgd <- renderValueBox({
    df <- filtered()
    valueBox(
      percent(mean(df$pred_lgd, na.rm = TRUE), 1),
      paste0("Avg LGD (", input$year_range[1], "–", input$year_range[2], ")"),
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$recovery <- renderValueBox({
    df <- filtered()
    valueBox(
      percent(1 - mean(df$pred_lgd, na.rm = TRUE), 1),
      paste0("Recovery (", input$year_range[1], "–", input$year_range[2], ")"),
      icon = icon("undo"),
      color = "green"
    )
  })
  
  ########################################################
  ## ✅ KEY TAKEAWAYS 
  ########################################################
  output$key_takeaways <- renderUI({
    
    df <- filtered()
    req(nrow(df) > 0)
    
    # ---- Risk contribution ----
    risk_df <- df %>%
      group_by(risk_bucket) %>%
      summarise(loss = sum(loss, na.rm = TRUE)) %>%
      arrange(desc(loss))
    
    total_loss_all <- sum(risk_df$loss)
    
    top_risk <- risk_df$risk_bucket[1]
    top_risk_pct <- round(100 * risk_df$loss[1] / total_loss_all, 1)
    
    # ---- Purpose ----
    purpose_df <- df %>%
      group_by(purpose) %>%
      summarise(loss = sum(loss, na.rm = TRUE)) %>%
      arrange(desc(loss))
    
    top_purpose <- purpose_df$purpose[1]
    top_purpose_pct <- round(100 * purpose_df$loss[1] / sum(purpose_df$loss), 1)
    
    # ---- LGD Trend (more precise wording) ----
    trend_df <- data %>%
      group_by(year) %>%
      summarise(avg_lgd = mean(pred_lgd, na.rm = TRUE))
    
    lgd_trend <- ifelse(
      tail(trend_df$avg_lgd, 1) > head(trend_df$avg_lgd, 1),
      "increased",
      "decreased"
    )
    
    start_year <- min(trend_df$year)
    end_year   <- max(trend_df$year)
    
    # ---- Grade (WITH %) ----
    grade_df <- df %>%
      group_by(grade) %>%
      summarise(total_loss = sum(loss, na.rm = TRUE)) %>%
      arrange(desc(total_loss))
    
    worst_grade <- grade_df$grade[1]
    worst_grade_pct <- round(100 * grade_df$total_loss[1] / sum(grade_df$total_loss), 1)
    
    # ---- Output ----
    tagList(
      tags$ul(
        tags$li(
          paste0(
            top_risk, " risk loans dominate total loss (~",
            top_risk_pct, "%)."
          )
        ),
        
        tags$li(
          paste0(
            "'", top_purpose, "' is the largest loss driver (~",
            top_purpose_pct, "% of total loss)."
          )
        ),
        
        tags$li(
          paste0(
            "Average LGD has ", lgd_trend,
            " from ", start_year, " to ", end_year, "."
          )
        ),
        
        tags$li(
          paste0(
            "Grade ", worst_grade,
            " contributes the highest total loss (~",
            worst_grade_pct, "%)."
          )
        )
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

