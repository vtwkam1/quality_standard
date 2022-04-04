extract_measure <- function(qs_html, measure_type) {
    
    css <- str_c('div[title^="Quality measure"] div[title*="', measure_type, '"] p')
    
    quality_measure <- qs_html %>% 
        html_elements(css) %>% 
        html_text2() %>% 
        # Remove carriage return control character
        str_replace_all("\r", "") %>% 
        # trim white space
        str_trim() 
    
    return(quality_measure)
}

section_table <- function(section, qs_id, statement_number, section_type) {
    
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
        if (is.na(table$point[[1]])) {
            table$point[[1]] <- "a"
        }
        
        # Renumber points if duplicate numbering detected
        if(sum(duplicated(table$point[!is.na(table$point)])) > 0) {
        
          table <- table %>% 
            mutate(row_id = row_number())
          
          renumber_points <- table %>% 
            filter(!is.na(point)) %>% 
            group_by(point) %>% 
            filter(n() > 1) %>%
            ungroup() %>% 
            mutate(point_group = letters[row_number()]) %>% 
            select(row_id, point_group) %>% 
            rename(point = point_group)
          
          table <- table %>%
            select(-point) %>% 
            left_join(renumber_points, by = "row_id") %>% 
            relocate(point) %>% 
            select(-row_id)
        }
        
        # If a row has no value in the point column, assign it the point letter for the row above
        for (i in seq_along(table$point)) {
            if (is.na(table$point[[i]])) {
                table$point[[i]] <- table$point[[i-1]]
            }
        }
        
        # Classify measures
        table <- table %>% 
            group_by(point) %>% 
            # Assign index to rows within each point
            mutate(point_index = 1:n()) %>% 
            ungroup() %>% 
            # Create label column and assign
            mutate(label = case_when(
                str_detect(measure, "^[:lower:]{1}\\) ") ~ "statement",
                str_detect(measure, "^Data source") ~ "data source",
                str_detect(measure, "^Numerator") ~ "numerator",
                str_detect(measure, "^Denominator") ~ "denominator")) %>% 
            # If no label and first statement in point group, assign "statement" label
            mutate(label = if_else(
                is.na(label) & point_index == 1,
                "statement",
                label)) %>% 
            # Drop index column
            select(-point_index)
        
        # Add identifiers
        table <-  table %>% 
          mutate(qs_id = qs_id,
                 statement_number = statement_number,
                 measure_type = section_type,
                 .before = 1
          )
    
        
        return(table)
    }
    
    else {
        table <- data.frame(point = character(), measure = character(), label = character())
        
        return(table)
    }
}

measures_table_fn <- function(statement_number = 9999, 
                           measure_type = NA, 
                           point = NA, 
                           measure_id = NA, 
                           measure = NA, 
                           numerator = NA, 
                           denominator = NA) {
  return(tibble(
      statement_number = statement_number,
      measure_type = measure_type,
      point = point,
      measure_id = measure_id,
      measure = measure,
      numerator = numerator,
      denominator = denominator
  ))
}

statement_row_fn <- function(qs_id,
                          statement_number,
                          statement) {
  return(tibble(qs_id = qs_id, statement_number = statement_number, statement = statement))
}


extract_statement <- function(qs_links, n, qs_id) {
    qs_html <- read_html(qs_links[[n]])
    
    # assumes only one paragraph as using html_element() not elements
    title <- qs_html %>% 
        html_element(".h2") %>% 
        html_text2()
    
    statement_number <- str_extract(title, "(?<=(s|S)tatement )\\d+")
    
    if (!str_detect(title, "(P|p)laceholder")) {
        
        statement <- qs_html %>% 
            html_element('div[title*="tatement"] p') %>% 
            html_text2() %>% 
            # Remove bracketed info on year
            str_remove("\\[.*\\]") %>% 
            str_trim()
        
        statement_row <- statement_row_fn(qs_id, statement_number, statement)
        
        # Extract all p elements inside a div element with title attribute "Structure" which is itself inside a div element with title attribute "Quality measures"
        qm_structure <- extract_measure(qs_html, measure = "Structure")
        qm_process <- extract_measure(qs_html, measure = "Process")
        qm_outcome <- extract_measure(qs_html, measure = "Outcome")
        
        if (is_empty(qm_structure) & is_empty(qm_process) & is_empty(qm_outcome)){
          
          measures <- measures_table_fn(
            statement_number = statement_number,
            measure_type = "error",
            measure_id = paste(qs_id, statement_number, "error", sep = "-"))
          
        } else {
        
          # Separate out points for structure, process and outcome
          qm_structure_table <- section_table(qm_structure, qs_id, statement_number, "structure")
          
          qm_process_table <- section_table(qm_process, qs_id, statement_number, "process")
          
          qm_outcome_table <- section_table(qm_outcome, qs_id, statement_number, "outcome")
          
          # Combine measures
          measures <- rbind(qm_structure_table, qm_process_table, qm_outcome_table) %>% 
              filter(label != "data source") %>% 
              mutate(measure = str_remove(measure, "^\\w+\\s?(:|\\)|\u2013|-)") %>% 
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
          
        }
        
    } else {
        statement <- sprintf("[Placeholder: %s]", str_extract(title, "(?<=:).+") %>% str_trim())
        
        statement_row <- statement_row_fn(qs_id, statement_number, statement)
        
        measures <- measures_table_fn(
            statement_number = statement_number,
            measure_type = "placeholder",
            measure_id = paste(qs_id, statement_number, "placeholder", sep = "-"))
    }
        return(list("statement_row" = statement_row,
                    "measures" = measures))
}