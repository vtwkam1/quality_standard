# library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

library(rvest)
library(rlang)

source('./src/qs_functions.R')

# Create output folder if it doesn't already exist
if(!dir.exists("output")) {
  dir.create("output")
}

extract_qs <- function(qs_number) {
  
  qs_id <- paste0("QS", qs_number)
  qs_url <- paste0("https://www.nice.org.uk/guidance/qs", qs_number)
  
  # Returns xml_document object with webpage html
  qs_html <- read_html(qs_url)
  
  # Extract links to quality statements within quality standard
  qs_links <- qs_html %>% 
    html_elements("#Guidance-Menu a") %>% 
    html_attr("href") %>%
    str_c("https://www.nice.org.uk", .) %>% 
    str_subset(., "(s|S)tatement-\\d+")
  
  if (!is_empty(qs_links)) {
    # Capture quality standard name
    qs_name <- qs_html %>% 
        html_elements("#content-start") %>% 
        html_text2()
    
    qs_directory <- tibble(qs_id = qs_id, qs_name = qs_name)
    
    # Create qs_directory or append new QS, if it doesn't already exist in the table
    if (!file.exists("./output/qs_directory.csv")) {
      # Create empty and write empty table
      write_csv(qs_directory, "./output/qs_directory.csv")
    } else {
      qs_dir_import <- read_csv("./output/qs_directory.csv")
      
      if (!(qs_id %in% qs_dir_import$qs_id)) {
        write_csv(qs_directory, "./output/qs_directory.csv", append = T)
      }
    }
    
    # Read quality statement
    
    statement_text <- data.frame(qs_id = character(),
                                 statement_number = numeric(),
                                 statement = character())
    
    all_statements <- data.frame(statement_number = numeric(),
                                 measure_type = character(),
                                 point = character(),
                                 measure_id = character(),
                                 measure = character(),
                                 numerator = character(),
                                 denominator = character())
    
    for (i in seq_along(qs_links)) {   
        st_table  <- extract_statement(qs_links, i, qs_id)
        
        statement_text <- rbind(statement_text, st_table$statement_row)
        all_statements <- rbind(all_statements, st_table$measures)
    }
    
    write_csv(statement_text, sprintf("./output/%s_statements.csv", qs_id))
    write_csv(all_statements, sprintf("./output/%s_measures.csv", qs_id))
  }
}

for (qs_number in 1:206) {
  extract_qs(qs_number)
}
