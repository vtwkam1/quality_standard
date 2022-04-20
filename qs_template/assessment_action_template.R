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

wb <- createWorkbook()

options("openxlsx.fontName" = "Lato")
options("openxlsx.fontSize" = 12)
options("openxlsx.dateFormat" = "dd/mm/yyyy")


# Initial assessment -----
sheet_name <- "Initial assessment"

addWorksheet(wb, sheetName = sheet_name)

wrap_text <- createStyle(valign = "center",
                         wrapText = T)

header_fn <- function(val1, val2) {
    tibble(col1 = val1,
           col2 = val2)
}

template_header <- header_fn("Initial assessment", NA)
qs_header <- header_fn("Quality standard:", qs)
empty_row <- header_fn(NA, NA)
date_header <-  header_fn("Date completed:", NA)
assessor_header <- header_fn("Assessor:", NA)


header_chunk <- bind_rows(template_header, 
                          qs_header, 
                          empty_row, 
                          date_header, 
                          assessor_header)

writeData(wb,
          sheet_name,
          header_chunk,
          startCol = 1,
          startRow = 1,
          colNames = F)

# Style template name
template_style <- createStyle(fontSize = 16,
                              textDecoration = "bold",
                              halign = "center")

addStyle(wb, 
         sheet_name, 
         style = template_style, 
         rows = 1, 
         cols = 1, 
         gridExpand = T, 
         stack = T)


# Style QS name
qs_header_style <- createStyle(fontSize = 14,
                              textDecoration = "bold")

addStyle(wb, 
         sheet_name, 
         style = qs_header_style, 
         rows = 2, 
         cols = 1:2, 
         gridExpand = T, 
         stack = T)

# Style other header starters
addStyle(wb, 
         sheet_name,
         style = createStyle(textDecoration = "bold"),
         rows = 3:nrow(header_chunk),
         cols = 1,
         gridExpand = T,
         stack = T
)

# Date formatting
# addStyle(wb, 
#          sheet_name,
#          style = createStyle(numFmt = openxlsx_getOp("numFmt", "DATE")),
#          rows = 3,
#          cols = 2,
#          gridExpand = T,
#          stack = T
# )

statements_assessment <- statement_table %>% 
    select(statement_disp) %>% 
    mutate("How does the current service compare with the statement?" = NA,
           "What is the source of evidence to support this?" = NA,
           "What are the risks associated with not making these improvements?\n(This should be an initial high-level assessment)" = NA,
           "Has this statement been prioritised for improvement?\n(If no, record a date for the review of the decision; if yes, use the remaining columns to record an action plan and monitor delivery)" = NA) %>% 
    rename(Statement = statement_disp)

tbl_start <- nrow(header_chunk) + 2
tbl_end <- tbl_start + nrow(statements_assessment)

writeDataTable(wb,
               sheet_name,
               statements_assessment,
               startCol = 1,
               startRow = tbl_start)

addStyle(wb, 
         sheet_name, 
         wrap_text, 
         rows = tbl_start:tbl_end, 
         cols = 1:20, 
         gridExpand = T, 
         stack = T)

setRowHeights(wb,
              sheet_name,
              rows = tbl_start:tbl_end, 
              heights = 80)

setColWidths(wb, sheet_name, cols = 1, widths = 35)
setColWidths(wb, sheet_name, cols = 2:5, widths = 40)


# Action plan ------
sheet_name <- "Action plan"

addWorksheet(wb, sheetName = sheet_name)

template_header <- header_fn("Action plan", NA)
qs_header <- header_fn("Quality standard:", qs)

header_chunk <- bind_rows(template_header, 
                          qs_header)

writeData(wb,
          sheet_name,
          header_chunk,
          startCol = 1,
          startRow = 1,
          colNames = F)

# Style template name
addStyle(wb, 
         sheet_name, 
         style = template_style, 
         rows = 1, 
         cols = 1, 
         gridExpand = T, 
         stack = T)


# Style QS name
addStyle(wb, 
         sheet_name, 
         style = qs_header_style, 
         rows = 2, 
         cols = 1:2, 
         gridExpand = T, 
         stack = T)

# # Style other header starters
# addStyle(wb, 
#          sheet_name,
#          style = createStyle(textDecoration = "bold"),
#          rows = 2:nrow(header_chunk),
#          cols = 1,
#          gridExpand = T,
#          stack = T
# )

# Date formatting
# addStyle(wb, 
#          sheet_name,
#          style = createStyle(numFmt = openxlsx_getOp("numFmt", "DATE")),
#          rows = 3,
#          cols = 2,
#          gridExpand = T,
#          stack = T
# )

statements_assessment <- statement_table %>% 
    select(statement_disp) %>% 
    mutate("Action(s) to improve the service to meet the statement" = NA,
           "Date action decided\n(dd/mm/yy)" = NA,
           "Person responsible" = NA,
           "Deadline for action\n(dd/mm/yy)" = NA,
           "Progress\n(Provide examples of actions in progress, changes in practices etc)" = NA,
           "Change stage\n(e.g. Not yet actioned, Action in progress, Action completed, Never actioned)" = NA) %>% 
    rename(Statement = statement_disp)

class(statements_assessment$`Date action decided\n(dd/mm/yy)`) <- "date"

class(statements_assessment$`Deadline for action\n(dd/mm/yy)`) <- "date"


tbl_start <- nrow(header_chunk) + 2
tbl_end <- tbl_start + nrow(statements_assessment)

writeDataTable(wb,
               sheet_name,
               statements_assessment,
               startCol = 1,
               startRow = tbl_start)

addStyle(wb, 
         sheet_name, 
         wrap_text, 
         rows = tbl_start:tbl_end, 
         cols = 1:20, 
         gridExpand = T, 
         stack = T)

setRowHeights(wb,
              sheet_name,
              rows = tbl_start:tbl_end, 
              heights = 80)

setColWidths(wb, sheet_name, cols = 1, widths = 35)
setColWidths(wb, sheet_name, cols = c(2, 6, 7), widths = 40)
setColWidths(wb, sheet_name, cols = 3:5, widths = 18)


saveWorkbook(wb, "test.xlsx", overwrite = T)

