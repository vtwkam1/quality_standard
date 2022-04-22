extract_qs <- function(qs_number) {
    
    # Concatenate number inputted to QS format, e.g. 205 -> "QS205"
    qs_id <- paste0("QS", qs_number)
    
    # Generate url of QS
    qs_url <- paste0("https://www.nice.org.uk/guidance/qs", qs_number)
    
    # Reads url and returns xml_document object of webpage
    qs_html <- read_html(qs_url)
    
    # Extract links to the quality statements within the quality standard from the sidebar
    qs_links <- qs_html %>% 
        # Select all <a> elements which are descendants of the element with id="Guidance-Menu"
        html_elements("#Guidance-Menu a") %>% 
        # Get the href attribute value (specifies url of page the link goes to), ie get all links (e.g.
        # "/guidance/qs205/chapter/Quality-statement-1-Standardised-bags")
        html_attr("href") %>%
        # Add the beginning of the NICE url so it is a complete url
        str_c("https://www.nice.org.uk", .) %>% 
        # Only keep the links which go to quality statements
        str_subset(., regex("statement-\\d+", ignore_case = T))
    
    # Handling quality standards which are deprecated and no longer have quality statements
    ## If quality statement links were extracted (ie not empty)...
    if (!is_empty(qs_links)) {
        
        # Capture quality standard name
        qs_name <- qs_html %>% 
            # Select element with id="content_start"
            html_elements("#content-start") %>% 
            # Retrieve text
            html_text2()
        
        # Create a qs_directory tibble with one row containing the selected qs_id and qs_name
        qs_directory <- tibble(qs_id = qs_id, qs_name = qs_name)
        
        # If the qs_directory file does not exist in the output folder...
        if (!file.exists("./output/qs_directory.csv")) {
            # Write it as a new file
            write_csv(qs_directory, "./output/qs_directory.csv")
        } else {
            # Else, read in the existing qs_directory file
            qs_dir_import <- read_csv("./output/qs_directory.csv", show_col_types = F)
            
            # And if the selected qs_id does not already exist in the directory...
            if (!(qs_id %in% qs_dir_import$qs_id)) {
                # Append it as a new row
                write_csv(qs_directory, "./output/qs_directory.csv", append = T)
            }
        }
        
        # CAPTURING EACH QUALITY STATEMENT -----
        
        # Create empty statement_text table 
        ## Will populate row by row with for loop looping over the list of quality statements
        statement_text <- statement_row_fn(qs_id = character(),
                                     statement_number = numeric(),
                                     statement = character())
        
        # Create empty all_statements table
        ## Will append with rows capturing all measures for each quality statement with for loop
        all_statements <- measures_table_fn(statement_number = numeric(),
                                     measure_type = character(),
                                     point = character(),
                                     measure_id = character(),
                                     measure = character(),
                                     numerator = character(),
                                     denominator = character(),
                                     data_source = character())
        
        # Loop over list of all quality statement links for this quality standard and extract info
        for (i in seq_along(qs_links)) {
            # Run custom function extract_statement, captures all the information for each quality statement
            ## Returns a list with a one-row table with the statement info (statement_row) and a table with all the quality measures as rows
            ## Separated out statement name from measures table in attempt to normalise data (SQL principles)
            st_table  <- extract_statement(qs_links, i, qs_id)
            
            # Append statement row for this quality statement
            ## So final table after for loop finishes will contain all statements for this QS
            statement_text <- rbind(statement_text, st_table$statement_row)
            
            # Append measures rows for this quality statement
            all_statements <- rbind(all_statements, st_table$measures)
        }
        
        # With complete tables capturing all quality measures within all quality statements for this QS...
        # Write to csv
        write_csv(statement_text, sprintf("./output/%s_statements.csv", qs_id))
        write_csv(all_statements, sprintf("./output/%s_measures.csv", qs_id))
    }
    # No 'else' clause, ie if no links to quality statements captured in qs_links, do nothing
}
