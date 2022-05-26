library(tidyverse)
library(lubridate)

# Create vector with all the relevant months
months <- ym("2019-06") + months(1:34)

# Generate file names using months specified above
filename <- map(months, ~ sprintf("./download_analytics/HSC dashboard - Engagement and implementation_All webpages_ links and downloads_Table_%s.csv", as.character(.x) %>% str_remove("-01$")))

# Import data
data <- filename %>% 
    set_names() %>% 
    map_dfr(read_csv, col_types = "ccd", .id = "filename")

# Extract the date from the file name
data <- data %>% 
    mutate(year_month = str_extract(filename, "(?<=Table_)\\d{4}-\\d{2}(?=.csv)") %>% ym(.)) %>% 
    select(-filename)

# Filter for QSSIT rows
qs_raw <- data %>% 
    filter(str_detect(File, '((QS)|(quality-standard))-service-improvement-template')) %>% 
    select(year_month, everything()) %>% 
    rename(page_path = 'Page path',
           file = File,
           downloads = Downloads)

# Save
write_csv(qs_raw, './download_analytics/qssit_downloads_2019-06_2022-04.csv')
