# 
# qs_directory <- read_csv("./output/qs_directory.csv", col_types = "cc") %>%
#     mutate(qs_disp = str_c(qs_id, "-", name, sep = " "))
# 
# qs_id <- "QS206"
# qs <- qs_directory$qs_disp[qs_directory$qs_id == qs_id]
# 
# statement_table <- read_csv(sprintf("./output/%s_statements.csv", qs_id),
#                                 col_types = "cic"
#     ) %>%
#         mutate(statement_disp = str_c(statement_number, "-", statement, sep = " "))
# 
# 
# measure_table <- read_csv(sprintf("./output/%s_measures.csv", qs_id),
#                            col_types = "icccccc")

monitoring_template <- function(measure_table, statement_table, qs_id, qs) {
  
  measures_template <- measure_table %>% 
      left_join(statement_table %>% select(-qs_id, -statement),
                by = "statement_number") %>% 
      mutate(measure_type = str_to_sentence(measure_type)) %>% 
      relocate(statement_disp, .before = 2) %>% 
      group_by(statement_number, measure_type) %>% 
      mutate(rn = row_number(),
             sum_rn = sum(rn)) %>% 
      mutate(point = if_else(sum_rn != 1, point, NA_character_)) %>% 
      ungroup() %>%
      mutate(point_disp = if_else(!is.na(point), paste0("(", point, ")"), NULL),
             measure_type_disp = paste("-", measure_type)) %>% 
      unite(measure_id, 
            c(statement_number, measure_type_disp, point_disp),
            sep = " ",
            remove = F, 
            na.rm = T) %>% 
      unite(measure_disp,
            c(point_disp, measure),
            sep = " ",
            remove = F,
            na.rm = T) %>% 
      relocate(ends_with("_disp"),
               .after = last_col()) %>% 
      select(-rn, -sum_rn, -measure_type_disp)
  
  wb <- createWorkbook()
  
  options("openxlsx.fontName" = "Lato")
  options("openxlsx.fontSize" = 12)
  options("openxlsx.dateFormat" = "dd/mm/yyyy")
  
  addWorksheet(wb, sheetName = qs_id)
  
  header_fn <- function(val1, val2) {
      tibble(col1 = val1,
             col2 = val2)
  }
  
  qs_header <- header_fn("Quality standard:", qs)
  
  writeData(wb, 
            qs_id, 
            qs_header,
            startCol = 1,
            startRow = 1,
            colNames = F)
  
  addStyle(wb, 
           qs_id, 
           style = createStyle(fontSize = 14,
                       textDecoration = "bold"), 
           rows = 1, 
           cols = 1:2, 
           gridExpand = T, 
           stack = T)
  
  measures_ui <- measures_template %>% 
      select(-c(measure_id, 
                statement_number, 
                point, 
                measure, 
                point_disp)) %>%
      select(statement_disp, measure_type, measure_disp, everything()) %>% 
      rename("Quality statement" = statement_disp,
             "Measure type" = measure_type,
             "Quality measure" = measure_disp,
             Numerator = numerator,
             Denominator = denominator,
             "Data source" = data_source
      )
  
  writeDataTable(wb, 
                 qs_id, 
                 measures_ui,
                 startCol = 1,
                 startRow = 3
  )
  
  wrap_text <- createStyle(valign = "center",
                       wrapText = T)
  
  addStyle(wb, qs_id, wrap_text, rows = 3:50, cols = 1:20, gridExpand = T, stack = T)
  
  setColWidths(wb, qs_id, cols = c(1, 3:6), widths = 35)
  
  measure_sheet <- function(i, measures_template, wb, qs_header, qs_id) {
    measure <- as.list(measures_template[i, ])
    sheet_name <- measure$measure_id
    
    addWorksheet(wb, sheetName = sheet_name)
    
    statement_header <- header_fn("Quality statement:", measure$statement_disp)
    
    measure_type_header <- header_fn("Quality measure type:", measure$measure_type)
    
    measure_header <- header_fn("Quality measure:", measure$measure_disp)
    
    data_source_header <- header_fn("Data source:", measure$data_source)
    
    empty_row <- header_fn(NA, NA)
    
    header_chunk <- bind_rows(qs_header, 
                              statement_header, 
                              empty_row, 
                              measure_type_header, 
                              measure_header,
                              data_source_header)
    
    if (!is.na(measure$numerator)) {
        numerator_header <- header_fn("Numerator:", measure$numerator)
        denominator_header <- header_fn("Denominator:", measure$denominator)
        
        header_chunk <- header_chunk %>% 
            bind_rows(numerator_header, denominator_header)
    }
    
    writeData(wb,
              sheet_name,
              header_chunk,
              startCol = 1,
              startRow = 1,
              colNames = F)
    
    tbl_startrow <- 10
    tbl_endrow <- 50 + tbl_startrow  
    
    addStyle(wb, 
             sheet_name,
             style = createStyle(textDecoration = "bold"),
             rows = 1:(tbl_startrow-1),
             cols = 1,
             gridExpand = T,
             stack = T
    )
    
    addStyle(wb, 
             sheet_name,
             style = createStyle(textDecoration = "bold"),
             rows = 5,
             cols = 2,
             gridExpand = T,
             stack = T
    )
    
    
    setColWidths(wb, sheet_name, cols = 1, widths = 20)
    
    # Create table
  
    
    if (!is.na(measure$numerator)) {
        input_tbl <- tibble(Date = rep(NA, tbl_endrow), 
                            Numerator = rep(NA, tbl_endrow),
                            Denominator = rep(NA, tbl_endrow),
                            Percentage = paste(paste0("B", 1:tbl_endrow + tbl_startrow),
                                               paste0("C", 1:tbl_endrow + tbl_startrow),
                                               sep = " / "))
    
        class(input_tbl$Percentage) <- "formula"
        
        addStyle(wb, 
                 sheet_name,
                 style = createStyle(numFmt = "PERCENTAGE"),
                 rows = (tbl_startrow:tbl_endrow) + 1,
                 cols = 4,
                 gridExpand = T,
                 stack = T
        )
        
        setColWidths(wb, sheet_name, cols = 2:4, widths = 15)
        
    } else {
        input_tbl <- tibble(Date = rep(NA, tbl_endrow),
                            "Comment/Value" = rep(NA, tbl_endrow))
        
        addStyle(wb, 
                 sheet_name, 
                 wrap_text, 
                 rows = tbl_startrow:tbl_endrow, 
                 cols = 2, 
                 gridExpand = T,
                 stack = T)
        
        setColWidths(wb, sheet_name, cols = 2, widths = 50)
    }
    
    class(input_tbl$Date) <- "date"
    
    writeDataTable(wb,
              sheet_name,
              input_tbl,
              startCol = 1,
              startRow = tbl_startrow)
  }
  
  walk(row.names(measures_template),
       measure_sheet,
       measures_template = measures_template, 
       wb = wb, 
       qs_header = qs_header, 
       qs_id = qs_id)
  
    return(list(measures_ui = measures_ui,
                wb = wb))
}
