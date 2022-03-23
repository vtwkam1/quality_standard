library(tidyverse)
library(rvest)

qs_number <- 33
qs_url <- paste0("https://www.nice.org.uk/guidance/qs", qs_number)
qs_html <- read_html(qs_url)

qs_name <- 

qs_links <- qs_html %>% 
    html_elements("#Guidance-Menu a") %>% 
    html_attr("href") %>%
    str_c("https://www.nice.org.uk", .)
    
qs_links <- qs_links[str_detect(qs_links, "Quality-statement-\\d+")]

qs <- read_html(qs_links[1])
# qs
# class(qs)
# str(qs)

qs %>% 
    html_node("body") %>% 
    html_children()

# title, statement and rationale all use html_element(),
# assumes only one paragraph

title <- qs %>% 
    html_element(".h2") %>% 
    html_text2()

statement_number <- str_extract(title, "(?<=Quality statement )\\d+(?=:)")
    
statement <- qs %>% 
    html_element('div[title="Quality statement"] p') %>% 
    html_text2()

# html_elements(), captures all paragraphs
qm_structure <- qs %>% 
    html_elements('div[title= "Quality measures"] div[title="Structure"] p') %>% 
    html_text2() %>% 
    str_replace_all("\r", "") %>% # Remove carriage return control character
    str_trim() 

qm_process <- qs %>% 
    html_elements('div[title= "Quality measures"] div[title="Process"] p') %>% 
    html_text2() %>% 
    str_replace_all("\r", "") %>% 
    str_trim()

qm_outcome <- qs %>% 
    html_elements('div[title= "Quality measures"] div[title="Outcome"] p') %>% 
    html_text2() %>% 
    str_replace_all("\r", "") %>% 
    str_trim()


# Separate out points for structure, process and outcome
section_table <- function(section) {
    table <- data.frame(point = rep(NA, length(section)), text = section)
    
    table <- table %>% 
        mutate(point = if_else(
            str_detect(text, "^[:lower:]{1}\\) "),
            str_extract(text, "^[:lower:]{1}(?=\\) )"),
            NULL))
    
    if (is.na(table$point[1])) {
        table$point[1] <- "a"
    }
    
    for (i in 1:length(table$point)) {
        if (is.na(table$point[i])) {
            table$point[i] <- table$point[i-1]
        }
    }
    
    table <- table %>% 
        group_by(point) %>% 
        mutate(point_index = 1:n()) %>% 
        ungroup() %>% 
        mutate(label = case_when(
            str_detect(text, "^[:lower:]{1}\\) ") ~ "statement",
            str_detect(text, "^Data source: ") ~ "data source",
            str_detect(text, "^Numerator \u2013") ~ "numerator",
            str_detect(text, "^Denominator \u2013") ~ "denominator")) %>% 
        mutate(label = if_else(
            is.na(label) & point_index == 1,
            "statement",
            label)) %>% 
        select(-point_index)
    
    return(table)
}

qm_structure_table <- section_table(qm_structure) %>% 
    mutate(qs = ,
           qs_no = paste0("QS", qs_number),
           statement = statement,
           measure = "structure",
           )

qm_process_table <- section_table(qm_process)

qm_outcome_table <- section_table(qm_outcome)


