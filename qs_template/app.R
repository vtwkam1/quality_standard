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

qs_directory <- read_csv("../output/qs_directory.csv", col_types = "cc")

qs_directory <- qs_directory %>%
  mutate(label = str_c(qs_id, "-", name, sep = " "))

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
                        choices = qs_directory$label
                        )
        ),
        # Main panel
        mainPanel(
            tabsetPanel(
                tabPanel("1. Initial assessment",
                         tableOutput("statements"),
                         downloadButton("download_statements",
                                        "Download statements")
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
                         tableOutput("measures")
                        )
            )
        )
    )
)
                         
read_statements <- function(qs_id) {
  statement_table <- read_csv(sprintf("../output/%s_statements.csv", qs_id),
    col_types = "cic"
  ) %>% 
      mutate(label = str_c(statement_number, "-", statement, sep = " "))
}

read_measures <- function(qs_id) {
  measures_table <- read_csv(sprintf("../output/%s_measures.csv", qs_id),
    col_types = "icccccc"
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
  qs_id <- reactive({
    qs_id <- qs_directory$qs_id[qs_directory$label == input$select_qs]
    
    message(glue("{qs_id} selected"))
    
    qs_id
  })

  # Statements
  statement_table <- reactive(read_statements(qs_id()))

  statement_template <- reactive({
    statement_table() %>%
      select(-qs_id, -label) %>%
      rename(Number = statement_number, 
             Statement = statement)
  })

  output$statements <- renderTable({
    statement_template()
  })

  output$download_statements <- downloadHandler(
    filename = function() {
      paste0(qs_id(), "_statements", ".csv")
    },
    content = function(file) {
      write_csv(statement_template(), file)
    }
  )
  
  observeEvent(input$select_qs, {
      updateCheckboxGroupInput(inputId = "select_statements",
                               choices = statement_table()$label)
  })

  # Measures
  measure_table <- reactive(read_measures(qs_id()))
  
  user_statements <- eventReactive(input$submit, {
      user_statements <- statement_table() %>% 
          filter(label %in% input$select_statements)
      
      message(paste("Statements", 
                    paste0(user_statements$statement_number, collapse = ", "), 
                    "selected"))
      
      user_statements
  })
  
  # measure_template <- reactive({
  #     measure_table
  # })

  output$measures <- renderTable({
    measure_table() %>%
          filter(statement_number %in% user_statements()$statement_number) %>% 
          select(-measure_id)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
