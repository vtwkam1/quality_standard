# library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(rvest)
library(knitr)

qs_number <- 33
qs_url <- paste0("https://www.nice.org.uk/guidance/qs", qs_number)

# Returns xml_document object with webpage html
qs_html <- read_html(qs_url)

# Capture quality standard name
qs_name <- qs_html %>% 
    html_elements("#content-start") %>% 
    html_text2()

# Extract links to quality statements within quality standard
qs_links <- qs_html %>% 
    html_elements("#Guidance-Menu a") %>% 
    html_attr("href") %>%
    str_c("https://www.nice.org.uk", .)

qs_links <- qs_links[str_detect(qs_links, "Quality-statement-\\d+")]

# Read quality statement
extract_statement <- function(qs_links, n, text, i, point, label, point_index, qs_number, measure) {
  qs <- read_html(qs_links[n])
  
  # assumes only one paragraph as using html_element() not elements
  title <- qs %>% 
      html_element(".h2") %>% 
      html_text2()
  
  statement_number <- str_extract(title, "(?<=Quality statement )\\d+(?=:)")
  
  statement <- qs %>% 
      html_element('div[title="Quality statement"] p') %>% 
      html_text2() %>% 
      # Remove bracketed info on year
      str_remove("\\[.*\\]") %>% 
      str_trim()
  
  # Extract all p elements inside a div element with title attribute "Structure" which is itself inside a div element with title attribute "Quality measures"
  qm_structure <- qs %>% 
      html_elements('div[title="Quality measures"] div[title="Structure"] p') %>% 
      html_text2() %>% 
      # Remove carriage return control character
      str_replace_all("\r", "") %>% 
      # trim white space
      str_trim() 
  
  qm_process <- qs %>% 
      html_elements('div[title="Quality measures"] div[title="Process"] p') %>% 
      html_text2() %>% 
      str_replace_all("\r", "") %>% 
      str_trim()
  
  qm_outcome <- qs %>% 
      html_elements('div[title="Quality measures"] div[title="Outcome"] p') %>% 
      html_text2() %>% 
      str_replace_all("\r", "") %>% 
      str_trim()
  
  # Separate out points for structure, process and outcome
  section_table <- function(section) {
      
      # Generate empty table
      table <- data.frame(point = rep(NA, length(section)), text = section)
      
      # If text in text column starts with a lower case character followed by a bracket, extract that letter into point column (e.g. "a)")
      table <- table %>% 
          mutate(point = if_else(
              str_detect(text, "^[:lower:]{1}\\) "),
              str_extract(text, "^[:lower:]{1}(?=\\) )"),
              NULL))
      
      # If for the first row, there is no a), b) etc, assign "a" in point column
      if (is.na(table$point[1])) {
          table$point[1] <- "a"
      }
      
      # If a row has no value in the point column, assign it the point letter for the row above
      for (i in 1:length(table$point)) {
          if (is.na(table$point[i])) {
              table$point[i] <- table$point[i-1]
          }
      }
      
      # E
      table <- table %>% 
          group_by(point) %>% 
          # Assign index to rows within each point
          mutate(point_index = 1:n()) %>% 
          ungroup() %>% 
          # Create label column and assign
          mutate(label = case_when(
              str_detect(text, "^[:lower:]{1}\\) ") ~ "statement",
              str_detect(text, "^Data source: ") ~ "data source",
              str_detect(text, "^Numerator \u2013") ~ "numerator",
              str_detect(text, "^Denominator \u2013") ~ "denominator")) %>% 
          # If no label and first statement in point group, assign "statement" label
          mutate(label = if_else(
              is.na(label) & point_index == 1,
              "statement",
              label)) %>% 
          # Drop index column
          select(-point_index)
      
      return(table)
  }
  
  qm_structure_table <- section_table(qm_structure) %>% 
      mutate(qs_number = paste0("QS", qs_number),
             statement_number = statement_number,
             measure = "structure",
             .before = 1
      )
  
  
  qm_process_table <- section_table(qm_process)  %>% 
      mutate(qs_number = paste0("QS", qs_number),
             statement_number = statement_number,
             measure = "process",
             .before = 1
      )
  
  
  qm_outcome_table <- section_table(qm_outcome)  %>% 
      mutate(qs_number = paste0("QS", qs_number),
             statement_number = statement_number,
             measure = "outcome",
             .before = 1
      )
  
  # Combine measures
  measures <- rbind(qm_structure_table, qm_process_table, qm_outcome_table) %>% 
      filter(label != "data source") %>% 
      mutate(text = str_remove(text, "^.*(:|\\)|\u2013)") %>% str_to_sentence()) %>% 
      relocate(text, .after = last_col()) %>% 
      mutate(measure_id = paste(statement_number, measure, point, sep = "-")) %>% 
      pivot_wider(names_from = label,
                  values_from = text)
}
