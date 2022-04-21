extract_qs <- function(qs_number) {
    
    # Concatenate number inputted to QS format, e.g. 1 -> "QS1"
    qs_id <- paste0("QS", qs_number)
    
    # Generate url of QS
    qs_url <- paste0("https://www.nice.org.uk/guidance/qs", qs_number)
    
    # Reads url and returns xml_document object of webpage
    qs_html <- read_html(qs_url)
    
    # Extract links to the quality statements within the quality standard
    qs_links <- qs_html %>% 
        # Select all <a> elements which are descendants of the element with id="Guidance-Menu"
        html_elements("#Guidance-Menu a") %>% 
        # Get the href attribute (specifies url of page the link goes to), ie get all links
        html_attr("href") %>%
        # Add the beginning of the NICE url so it is a complete url
        str_c("https://www.nice.org.uk", .) %>% 
        # Only keep the links which go to quality statements
        str_subset(., regex("statement-\\d+", ignore_case = T))
    
    if (!is_empty(qs_links)) {
        # Capture quality standard name
        qs_name <- qs_html %>% 
            # Select element with id="content_start"
            html_elements("#content-start") %>% 
            # Retrieve text
            html_text2()
        
        qs_directory <- tibble(qs_id = qs_id, qs_name = qs_name)
        
        # Create qs_directory or append new QS, if it doesn't already exist in the table
        if (!file.exists("./output/qs_directory.csv")) {
            # Create empty and write empty table
            write_csv(qs_directory, "./output/qs_directory.csv")
        } else {
            qs_dir_import <- read_csv("./output/qs_directory.csv", show_col_types = F)
            
            if (!(qs_id %in% qs_dir_import$qs_id)) {
                write_csv(qs_directory, "./output/qs_directory.csv", append = T)
            }
        }
        
        # Read quality statement
        
        statement_text <- statement_row_fn(qs_id = character(),
                                     statement_number = numeric(),
                                     statement = character())
        
        all_statements <- measures_table_fn(statement_number = numeric(),
                                     measure_type = character(),
                                     point = character(),
                                     measure_id = character(),
                                     measure = character(),
                                     numerator = character(),
                                     denominator = character(),
                                     data_source = character())
        
        for (i in seq_along(qs_links)) {   
            st_table  <- extract_statement(qs_links, i, qs_id)
            
            statement_text <- rbind(statement_text, st_table$statement_row)
            all_statements <- rbind(all_statements, st_table$measures)
        }
        
        write_csv(statement_text, sprintf("./output/%s_statements.csv", qs_id))
        write_csv(all_statements, sprintf("./output/%s_measures.csv", qs_id))
    }
}
