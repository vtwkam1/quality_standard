library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(rlang)
library(purrr)

measure_files <-
    list.files(path = "./output/",
               pattern = "_measures.csv", 
               full.names = T) %>% 
    map_dfr(~ read_csv(., show_col_types = F)) 

write_csv(measure_files, sprintf("./output/%s_log.csv", Sys.Date()))
