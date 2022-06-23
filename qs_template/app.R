#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(glue)
library(rlang)
library(openxlsx)
library(shinyjs)


source("./monitoring_template.R")
source("./assessment_action_template.R")


qs_directory <- read_csv("../output/qs_directory.csv", col_types = "cc") %>% 
    mutate(qs_disp = str_c(qs_id, "-", name, sep = " ")) %>% 
    arrange(order(str_order(qs_id, numeric = T)))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    useShinyjs(),
    
    # Change app theme
    theme = bslib::bs_theme(bootswatch = "darkly"),
  
    # Application title
    titlePanel("Quality standard service improvement template (QSSIT)"),
  
    # Sidebar layout
    sidebarLayout(
        
        # Sidebar
        sidebarPanel(
            selectInput("select_qs",
                        "Select quality standards",
                        choices = qs_directory$qs_disp,
                        multiple = TRUE
                        ),
            actionButton("submit_qs", "Submit")
        ),
        # Main panel
        mainPanel(
            tabsetPanel(
                tabPanel("1. Initial assessment and action plan",
                         tableOutput("statements"),
                         downloadButton("download_assessment",
                                        "Download initial assessment and action plan template")
                        ),
                # tabPanel("2. Monitoring selected statements",
                #          checkboxGroupInput("select_statements",
                #                             "Select quality statements to monitor:",
                #                             choices = c("NA",
                #                                         "NA",
                #                                         "NA"),
                #                             width = "100%"
                #                             ),
                #          actionButton("submit", "Submit"),
                #          downloadButton("download_monitoring",
                #                         "Download monitoring template"),
                #          tableOutput("measures")
                #         )
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
    
  selected_qs <- reactive({
    selected_qs <- tibble(qs_disp = input$select_qs) %>% 
        left_join(qs_directory %>% select(qs_disp, qs_id),
                  by = "qs_disp") %>% 
        arrange(order(str_order(qs_id, numeric = T)))
    
    message(sprintf("%s selected", paste(selected_qs$qs_id, collapse = ", ")))
    
    selected_qs
  })

  # Statements
  statement_table <- eventReactive(input$submit_qs, {
      selected_qs() %>% 
          mutate(statements = map(qs_id, read_statements)) %>% 
          select(-qs_id) %>% 
          unnest(statements,
                 names_repair = "unique")
    })

  output$statements <- renderTable({
      statement_table() %>%
          select(-c(qs_id, statement_number, statement)) %>%
          rename("Quality standard" = qs_disp,
          "Quality statement" = statement_disp)
  })

  assessment_template <- eventReactive(input$select_qs, {
      assessment_action_template(qs = input$select_qs,
                                 statement_table = statement_table())
  })

  output$download_assessment <- downloadHandler(
     filename = function() {
         paste0(paste0(selected_qs()$qs_id, collapse = "_"), "_QSSIT_assessment_action", ".xlsx")
     },
     content = function(file) {
         saveWorkbook(assessment_template(), file)
     }
  )

  # observeEvent(input$select_qs, {
  #     updateCheckboxGroupInput(inputId = "select_statements",
  #                              choices = statement_table()$statement_disp)
  # })
  # 
  # # Measures
  # measure_table <- reactive(read_measures(qs_id()))
  # 
  # user_statements <- eventReactive(input$submit, {
  #     user_statements <- statement_table() %>% 
  #         filter(statement_disp %in% input$select_statements)
  #     
  #     message(paste("Statements", 
  #                   paste0(user_statements$statement_number, collapse = ", "), 
  #                   "selected"))
  #     
  #     user_statements
  # })
  # 
  # user_measures <- eventReactive(input$submit, {
  #     measure_table() %>%
  #         filter(statement_number %in% user_statements()$statement_number)
  # })
  # 
  # monitoring_output <- eventReactive(input$submit, {
  #     monitoring_template(measure_table = user_measures(), 
  #                         statement_table = user_statements(), 
  #                         qs_id = qs_id(), 
  #                         qs = input$select_qs)
  # })
  # 
  # # measure_template <- reactive({
  # #     measure_table
  # # })
  # 
  # output$measures <- renderTable({monitoring_output()$measures_ui})
  # 
  # observe({
  #     if (input$submit > 0) {
  #         enable("download_monitoring")
  #     }
  # })
  # 
  # output$download_monitoring <- downloadHandler(
  #     filename = function() {
  #         paste0(qs_id(), "_QSSIT_monitoring", ".xlsx")
  #     },
  #     content = function(file) {
  #         saveWorkbook(monitoring_output()$wb, file)
  #     }
  # )
  # 
  # disable("download_monitoring")
  
}

# Run the application
shinyApp(ui = ui, server = server)
