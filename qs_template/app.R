#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(glue)
library(dplyr)
library(stringr)
# library(forcats)
library(tidyr)
library(readr)
library(purrr)
library(rlang)
library(openxlsx)

source("./monitoring_template.R")
source("./assessment_action_template.R")


qs_directory <- read_csv("../output/qs_directory.csv", col_types = "cc") %>% 
    mutate(qs_disp = str_c(qs_id, "-", name, sep = " ")) %>% 
    arrange(order(str_order(qs_id, numeric = T)))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Change app theme
    theme = bslib::bs_theme(bootswatch = "darkly"),
  
    # Application title
    titlePanel("Quality Standard Service Improvement Template"),
  
    # Sidebar layout
    sidebarLayout(
        
        # Sidebar
        sidebarPanel(
            selectInput("select_qs",
                        "Select Quality Standard",
                        choices = qs_directory$qs_disp
                        )
        ),
        # Main panel
        mainPanel(
            tabsetPanel(
                tabPanel("1. Initial assessment",
                         tableOutput("statements"),
                         downloadButton("download_assessment",
                                        "Download initial assessment and action plan template")
                        ),
                tabPanel("2. Statements of interest",
                         checkboxGroupInput("select_statements",
                                            "Select statements to monitor:",
                                            choices = c("NA",
                                                        "NA",
                                                        "NA"),
                                            width = "100%"
                                            ),
                         actionButton("submit", "Submit"),
                         tableOutput("measures"),
                         downloadButton("download_monitoring",
                                        "Download monitoring template")
                        )
            )
        )
    )
)
                         
read_statements <- function(qs_id) {
  statement_table <- read_csv(sprintf("../output/%s_statements.csv", qs_id),
    col_types = "cic"
  ) %>% 
      mutate(statement_disp = str_c(statement_number, "-", statement, sep = " "))
}

read_measures <- function(qs_id) {
  measures_table <- read_csv(sprintf("../output/%s_measures.csv", qs_id),
    col_types = "icccccc"
  )
}

# 
server <- function(input, output, session) {
    
  qs_id <- reactive({
    qs_id <- qs_directory$qs_id[qs_directory$qs_disp == input$select_qs]
    
    message(glue("{qs_id} selected"))
    
    qs_id
  })

  # Statements
  statement_table <- reactive(read_statements(qs_id()))

  output$statements <- renderTable({
      statement_table() %>%
          select(-qs_id, -statement_disp) %>%
          rename(Number = statement_number, 
                 Statement = statement)
  })
  
  assessment_template <- eventReactive(input$select_qs, {
      assessment_action_template(qs = input$select_qs,
                                 statement_table = statement_table())
  })

  output$download_assessment <- downloadHandler(
     filename = function() {
         paste0(qs_id(), "_QSSIT_assessment_action", ".xlsx")
     },
     content = function(file) {
         saveWorkbook(assessment_template(), file)
     }
  )
  
  observeEvent(input$select_qs, {
      updateCheckboxGroupInput(inputId = "select_statements",
                               choices = statement_table()$statement_disp)
  })

  # Measures
  measure_table <- reactive(read_measures(qs_id()))
  
  user_statements <- eventReactive(input$submit, {
      user_statements <- statement_table() %>% 
          filter(statement_disp %in% input$select_statements)
      
      message(paste("Statements", 
                    paste0(user_statements$statement_number, collapse = ", "), 
                    "selected"))
      
      user_statements
  })
  
  user_measures <- eventReactive(input$submit, {
      measure_table() %>%
          filter(statement_number %in% user_statements()$statement_number)
  })
  
  monitoring_output <- eventReactive(input$submit, {
      monitoring_template(measure_table = user_measures(), 
                          statement_table = user_statements(), 
                          qs_id = qs_id(), 
                          qs = input$select_qs)
  })
  
  # measure_template <- reactive({
  #     measure_table
  # })

  output$measures <- renderTable({monitoring_output()$measures_ui})
  
  output$download_monitoring <- downloadHandler(
      filename = function() {
          paste0(qs_id(), "_QSSIT_monitoring", ".xlsx")
      },
      content = function(file) {
          saveWorkbook(monitoring_output()$wb, file)
      }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
