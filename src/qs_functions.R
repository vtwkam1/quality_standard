extract_measure <- function(qs_html, measure_type) {
    
    css <- str_c('div[title="Quality measures"] div[title="', measure_type, '"] p')
    
    quality_measure <- qs_html %>% 
        html_elements(css) %>% 
        html_text2() %>% 
        # Remove carriage return control character
        str_replace_all("\r", "") %>% 
        # trim white space
        str_trim() 
    
    return(quality_measure)
}

section_table <- function(section) {
    
    if (!is_empty(section))  {
        # Generate empty table
        table <- data.frame(point = rep(NA, length(section)), measure = section)
        
        # If measure in measure column starts with a lower case character followed by a bracket, extract that letter into point column (e.g. "a)")
        table <- table %>% 
            mutate(point = if_else(
                str_detect(measure, "^[:lower:]{1}\\) "),
                str_extract(measure, "^[:lower:]{1}(?=\\) )"),
                NULL))
        
        # If for the first row, there is no a), b) etc, assign "a" in point column
        if (is.na(table$point[1])) {
            table$point[1] <- "a"
        }
        
        # If a row has no value in the point column, assign it the point letter for the row above
        for (i in seq_along(table$point)) {
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
                str_detect(measure, "^[:lower:]{1}\\) ") ~ "statement",
                str_detect(measure, "^Data source: ") ~ "data source",
                str_detect(measure, "^Numerator \u2013") ~ "numerator",
                str_detect(measure, "^Denominator \u2013") ~ "denominator")) %>% 
            # If no label and first statement in point group, assign "statement" label
            mutate(label = if_else(
                is.na(label) & point_index == 1,
                "statement",
                label)) %>% 
            # Drop index column
            select(-point_index)
        
        return(table)
    }
    
    else {
        table <- data.frame(point = character(), measure = character(), label = character())
        
        return(table)
    }
}

extract_statement <- function(qs_links, n, qs_id) {
    qs_html <- read_html(qs_links[n])
    
    # assumes only one paragraph as using html_element() not elements
    title <- qs_html %>% 
        html_element(".h2") %>% 
        html_text2()
    
    statement_number <- str_extract(title, "(?<=Quality statement )\\d+(?=:)")
    
    statement <- qs_html %>% 
        html_element('div[title="Quality statement"] p') %>% 
        html_text2() %>% 
        # Remove bracketed info on year
        str_remove("\\[.*\\]") %>% 
        str_trim()
    
    statement_row <- tibble(qs_id = qs_id, statement_number = statement_number, statement = statement)
    
    # Extract all p elements inside a div element with title attribute "Structure" which is itself inside a div element with title attribute "Quality measures"
    qm_structure <- extract_measure(qs_html, measure = "Structure")
    qm_process <- extract_measure(qs_html, measure = "Process")
    qm_outcome <- extract_measure(qs_html, measure = "Outcome")
    
    # Separate out points for structure, process and outcome
    qm_structure_table <- section_table(qm_structure) %>% 
        mutate(qs_id = qs_id,
               statement_number = statement_number,
               measure_type = "structure",
               .before = 1
        )
    
    
    qm_process_table <- section_table(qm_process)  %>% 
        mutate(qs_id = qs_id,
               statement_number = statement_number,
               measure_type = "process",
               .before = 1
        )
    
    
    qm_outcome_table <- section_table(qm_outcome)  %>% 
        mutate(qs_id = qs_id,
               statement_number = statement_number,
               measure_type = "outcome",
               .before = 1
        )
    
    # Combine measures
    measures <- rbind(qm_structure_table, qm_process_table, qm_outcome_table) %>% 
        filter(label != "data source") %>% 
        mutate(measure = str_remove(measure, "^.*(:|\\)|\u2013)") %>% 
                   str_trim() %>% 
                   str_replace("^\\w{1}", toupper)) %>% 
        relocate(measure, .after = last_col()) %>% 
        mutate(measure_id = paste(qs_id, statement_number, measure_type, point, sep = "-")) %>% 
        pivot_wider(names_from = label,
                    values_from = measure) %>% 
        rename(measure = statement) %>% 
        select(-qs_id)
    
    if(!("numerator" %in% colnames(measures))) {
        measures$numerator <- NA
        measures$denominator <- NA
    }
    
    return(list("statement_row" = statement_row,
                "measures" = measures))
}