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
    mutate(point_temp = if_else(!is.na(point), paste0("(", point, ")"), NULL),
           measure_type_temp = paste("-", measure_type)) %>% 
    unite(measure_id, 
          c(statement_number, measure_type_temp, point_temp),
          sep = " ",
          remove = F, 
          na.rm = T) %>% 
    select(-rn, -sum_rn, -measure_type_temp)

wb <- createWorkbook()

addWorksheet(wb, sheetName = qs_id)

writeDataTable(wb, 
               qs_id, 
               x = measures_template %>% 
                   rename("Statement number" = statement_number,
                          Statement = statement,
                          "Measure type" = measure_type,
                          "Measure number" = point,
                          Measure = measure,
                          Numerator = numerator,
                          Denominator = denominator
                   ) %>% 
                   select(-measure_id, -point_temp)
)

style <- createStyle(valign = "center",
                     wrapText = T)

addStyle(wb, qs_id, style, rows = 1:20, cols = 1:20, gridExpand = T, stack = T)

setColWidths(wb, qs_id, cols = c(2,5,6,7), widths = 35)

saveWorkbook(wb, paste0("./output/", qs_id, ".xlsx"), overwrite = T)

measure <- as.list(measures_template[1, ])
sheet_name <- measure[["measure_id"]]

addWorksheet(wb, sheetName = sheet_name)

statement_wording <- data.frame(as.list(c("Statement:", paste(measure[["statement_number"]], 
                                                              "-",
                                                              measure[["statement"]]))))

writeData(wb, 
          sheet_name, 
          statement_wording,
          startCol = 1,
          startRow = 1,
          colNames = F)

if (is.na(measure[["point_temp"]])) {
    measure_wording <- c("Quality measure:",
                         paste(measure[["measure_type"]],
                               "-",
                               measure[["measure"]]))
} else {
    measure_wording <- c("Quality measure:",
                         paste(measure[["measure_type"]],
                               measure[["point_temp"]],
                               "-",
                               measure[["measure"]]))
}

measure_wording <- data.frame(as.list(measure_wording))

writeData(wb, 
          sheet_name, 
          measure_wording,
          startCol = 1,
          startRow = 2,
          colNames = F)

saveWorkbook(wb, paste0("./output/", qs_id, ".xlsx"), overwrite = T)
