library(openxlsx)

qs_id <- "QS206"

statement_table <- read_csv(sprintf("./output/%s_statements.csv", qs_id),
                                col_types = "cic"
    ) %>% 
        mutate(label = str_c(statement_number, "-", statement, sep = " "))


measures_table <- read_csv(sprintf("./output/%s_measures.csv", qs_id),
                           col_types = "icccccc")

measures_template <- measures_table %>% 
    left_join(statement_table %>% select(-qs_id, -label),
              by = "statement_number") %>% 
    mutate(measure_type = str_to_sentence(measure_type)) %>% 
    relocate(statement, .before = 2) %>% 
    group_by(statement_number, measure_type) %>% 
    mutate(rn = row_number(),
           sum_rn = sum(rn)) %>% 
    mutate(point = if_else(sum_rn != 1, point, NA_character_)) %>% 
    ungroup() %>% 
    select(-measure_id, -rn, -sum_rn) %>% 
    rename("Statement number" = statement_number,
           Statement = statement,
           "Measure type" = measure_type,
           "Measure number" = point,
           Measure = measure,
           Numerator = numerator,
           Denominator = denominator
           )

wb <- createWorkbook()
addWorksheet(wb, sheetName = qs_id)
writeDataTable(wb, qs_id, x = measures_template)
style <- createStyle(halign = "center",
                     valign = "center",
                     wrapText = T)
addStyle(wb, qs_id, style, rows = 1:20, cols = 1:20, gridExpand = T, stack = T)
setColWidths(wb, qs_id, cols = c(2,5,6,7), widths = 35)
saveWorkbook(wb, paste0(qs_id, ".xlsx"), overwrite = T)
