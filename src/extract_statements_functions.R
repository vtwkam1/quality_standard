

extract_measure <- function(qs_html, measure_type) {
    
    # Generate string to use as css selector to capture all paragraphs under the heading [measure_type]
    ## Selector says, "Extract all <p> elements inside the <div> element whose title attribute 
    ## value contains the substring [measure_type] which is itself inside a <div> element 
    ## whose title attribute value starts with "Quality measure"
    css <- str_c('div[title^="Quality measure"] div[title*="', measure_type, '"] p')
    
    # Performing the extraction from the xml object with the QS webpage
    quality_measure <- qs_html %>% 
        # Using the css selector specified above
        html_elements(css) %>% 
        # Retrieve text
        html_text2() %>% 
        # Remove carriage return control character, \r
        str_replace_all("\r", "") %>% 
        # Trim white space
        str_trim() 
    
    # Return quality_measure (a character vector)
    return(quality_measure)
}

section_table <- function(section, qs_id, statement_number, section_type) {
    
    # Handling sections (Structure, Process, Outcome) which are empty
    ## If not empty, then...
    if (!is_empty(section))  {
        
        # Generate empty table with NA as placeholders in the point column
        # And where the measure column holds the character vector returned in extract_measure()
        table <- tibble(point = rep(NA, length(section)), 
                        measure = section)
        
        # Pulling out the numbering of quality measures, e.g. "a" from "a) Evidence of local arrangements to ensure..."
        table <- table %>% 
            # If the string in the measure column starts with a single lower case letter followed by a bracket,
            # maybe with a starting "(" bracket (e.g. "(a)"),
            # extract that letter into the point column
            mutate(point = if_else(
                str_detect(measure, "^\\(?[:lower:]{1}\\) "),
                str_extract(measure, "^\\(?[:lower:]{1}(?=\\) )") %>% str_remove("\\("),
                NULL))
        
        # If in the first row, no letter has been pulled, assign "a"
        if (is.na(table$point[[1]])) {
            table$point[[1]] <- "a"
        }
        
        # In some quality statements, duplicate point numbering has been used mistakenly
        # e.g. two measures with "a)" under 'Structure'
        # So, renumber points if duplicate numbering detected
            ## Isolate points which are not NA (i.e. letter has been pulled), identify duplicates (returns logical vector with 
            ## TRUE for duplicated elements), sum the logical vector, and if the sum is greater than 0,
            ## (ie duplicates exist), then...
        if(sum(duplicated(table$point[!is.na(table$point)])) > 0) {
            
            # Create row_id column with row numbers
            table <- table %>%
                mutate(row_id = row_number())
            
            # Renumber in a new table
            renumber_points <- table %>% 
                # Filter out rows where no 'point' pulled
                filter(!is.na(point)) %>%
                # Group by point
                group_by(point) %>%
                # Filter, keeping only rows with duplicate numbering
                filter(n() > 1) %>%
                ungroup() %>%
                # Renumber using letters (built-in to R, contains the 26 lower-case alphabet letters) and
                # index using the new row_numbers 
                # (remember rows with NA in point column have been filtered out, leaving only the rows with a statement)
                mutate(point_group = letters[row_number()]) %>%
                # Only keep row_id and point_group
                select(row_id, point_group) %>% 
                # point_group column becomes point
                rename(point = point_group)
          
            # Replace with renumbered point col
            table <- table %>%
                # Drop old point col in master table
                select(-point) %>% 
                # Join renumber_points table on row_id for renumbered points col
                left_join(renumber_points, by = "row_id") %>% 
                # Move point column to left-hand side
                relocate(point) %>% 
                # Drop row_id column
                select(-row_id)
        }
        
        # Group the other relevant paragraphs for a quality measure
            # If a row has no value in the point column (ie it contains the denominator, numerator, data source 
            # etc for a quality measure, rather than the quality measure itself), assign it the point letter for the row above
        for (i in seq_along(table$point)) {
            if (is.na(table$point[[i]])) {
                table$point[[i]] <- table$point[[i-1]]
            }
        }
        
        # Classify text as statement, data source, numerator or denominator
        table <- table %>% 
            group_by(point) %>% 
            # Assign index to rows within each point, e.g. for all rows under point "a", number each row
            mutate(point_index = 1:n()) %>% 
            ungroup() %>% 
            # Create label column, detect marker, and assign
            mutate(label = case_when(
                str_detect(measure, "^\\(?[:lower:]{1}\\) ") ~ "statement",
                str_detect(measure, regex("^data source", ignore_case = T)) ~ "data source",
                str_detect(measure, regex("^numerator", ignore_case = T)) ~ "numerator",
                str_detect(measure, regex("^denominator", ignore_case = T)) ~ "denominator")) %>% 
            # If a row has no label and is the first row of its point group, assign "statement" label
            mutate(label = if_else(
                is.na(label) & point_index == 1,
                "statement",
                label)) %>% 
            # Drop point_index column
            select(-point_index) %>% 
            # (NEEDS THINKING) Drops rows which still have an NA label
            filter(!is.na(label))
        
        # Add identifier columns to the rows, all rows will have same qs_id, statement_number and
        # measure_type (ie Structure, Process, Outcome)
        table <-  table %>% 
          mutate(qs_id = qs_id,
                 statement_number = statement_number,
                 measure_type = section_type,
                 .before = 1
          )
    
        
        return(table)
    }
    
    else {
        # If the section is empty, return empty table
        table <- tibble(qs_id = character(),
                        statement_number = character(),
                        measure_type = character(),
                        point = character(),
                        measure = character(),
                        label = character())
        
        return(table)
    }
}

measures_table_fn <- function(statement_number = 9999, 
                           measure_type = NA, 
                           point = NA, 
                           measure_id = NA, 
                           measure = NA, 
                           numerator = NA, 
                           denominator = NA,
                           data_source = NA) {
    
    # Function just to standardise table creation for measures table
  return(tibble(
      statement_number = statement_number,
      measure_type = measure_type,
      point = point,
      measure_id = measure_id,
      measure = measure,
      numerator = numerator,
      denominator = denominator,
      data_source = data_source
  ))
}

statement_row_fn <- function(qs_id,
                          statement_number,
                          statement) {
    
    # Function just to standardise table creation for statements table
    return(tibble(qs_id = qs_id, statement_number = statement_number, statement = statement))
}

# MAIN FUNCTION WHERE ABOVE FUNCTIONS ARE USED
# Extracts all information from a quality statement
extract_statement <- function(qs_links, n, qs_id) {
    # Scrape webpage for selected quality statement
    qs_html <- read_html(qs_links[[n]])
    
    # Capture title of quality statement (e.g. "Quality statement 1: Standardised bags")
    ## Not the full quality statement, just the title
    title <- qs_html %>% 
        # Select element with class="h2"(html_element, not elements, assumes only one element)
        html_element(".h2") %>% 
        html_text2()
    
    # Extract the statement number, e.g. 1, from "Quality statement 1: Standardised bags"
    statement_number <- str_extract(title, regex("(?<=statement )\\d+", ignore_case = T))
    
    # If statement to handle placeholder quality statements
    ## If "placeholder" is not detected in the title, then...
    if (!str_detect(title, regex("placeholder", ignore_case = T))) {
        
        # Capture the full quality statement, e.g. "Preterm and term babies who are prescribed neonatal
        # parenteral nutrition are started on a standardised bag."
        statement <- qs_html %>% 
            # Select <div> element whose title attribute value contains the substring "tatement"
            ## "tatement" chosen due to inconsistencies in labelling 
            html_element('div[title*="tatement"] p') %>% 
            html_text2() %>% 
            # Remove information on year of publishing/updating (anything in square brackets, e.g. "[2005]")
            str_remove("\\[.*\\]") %>% 
            # Trim white from start or end of string, e.g. "XXX standardised bag. " -> "XXX standardised bag."
            str_trim()
        
        # Call custom function statement_row(), declared above in same script
        ## Generates table with one row, containing qs_id, statement_number and statement
        statement_row <- statement_row_fn(qs_id, statement_number, statement)
        
        # Call custom function extract_measure() to pull structure, process, then outcome measures
        qm_structure <- extract_measure(qs_html, measure = "Structure")
        qm_process <- extract_measure(qs_html, measure = "Process")
        qm_outcome <- extract_measure(qs_html, measure = "Outcome")
        
        # Handling cases where all three come back empty, e.g. where the quality statement has been removed
        ## If nothing pulled under Structure, Process AND Outcome...
        if (is_empty(qm_structure) & is_empty(qm_process) & is_empty(qm_outcome)){
          
            # Create the measures table, but populate the row with an error message
            measures <- measures_table_fn(
              statement_number = statement_number,
              measure_type = "error",
              measure_id = paste(qs_id, statement_number, "error", sep = "-"))
          
        } else {
            
            # Call custom function section_table() to identify the 'type' (label) of each paragraph (element)
            # pulled using extract_measure(), (e.g. statement, numerator, denominator, data source)
            # and convert it into a neat, table format with one row per quality measure
            qm_structure_table <- section_table(qm_structure, qs_id, statement_number, "structure")
            qm_process_table <- section_table(qm_process, qs_id, statement_number, "process")
            qm_outcome_table <- section_table(qm_outcome, qs_id, statement_number, "outcome")
          
          # Bind the tables for Structure, Process and Outcome
          measures <- rbind(qm_structure_table, qm_process_table, qm_outcome_table) %>%
              # Remove 
              mutate(measure = str_remove(measure, "^\\(?\\w+\\s?(:|\\)|\u2013|-)") %>% 
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
          
          if(!("data source" %in% colnames(measures))) {
              measures$data_source <- NA
          } else {
              measures <- measures %>% 
                  rename(data_source = "data source")
          }
          
          measures <- measures %>% 
              mutate(numerator = ifelse(measure_type == "outcome" & is.na(numerator), 
                                         "To be determined locally",
                                         numerator),
                     denominator = ifelse(measure_type == "outcome" & is.na(denominator),
                                           "To be determined locally",
                                           denominator)) %>% 
              relocate(data_source, .after = last_col()) %>% 
              mutate(data_source = str_remove(data_source, "^\\w+\\s{1}\\w+\\s?[:punct:]{1}") %>% 
                         str_trim() %>% 
                         str_replace("^\\w{1}", toupper))
          
          
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