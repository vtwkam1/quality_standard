library(tidyverse)
library(rlang)

measure_files <-
    list.files(path = "./output/",
               pattern = "_measures.csv", 
               full.names = T) %>% 
    map_dfr(~ read_csv(., show_col_types = F)) %>% 
    mutate(qs = str_extract(measure_id, "^QS\\d+"))

write_csv(measure_files, sprintf("./output/%s_log.csv", Sys.Date()))

error_measure <- measure_files %>% 
    filter(measure_type == "error") 

error_qs <- error_measure %>% 
    distinct(qs) %>% 
    arrange(order(str_order(qs, numeric = T)))

write_csv(error_measure, sprintf("./output/%s_error.csv", Sys.Date()))

write_csv(error_qs, sprintf("./output/%s_error_qs.csv", Sys.Date()))

na_measure <- measure_files %>% 
    filter(is.na(measure), !(measure_type %in% c("placeholder", "error")))


na_qs <- na_measure %>% 
    distinct(qs) %>% 
    arrange(order(str_order(qs, numeric = T)))

write_csv(na_measure, sprintf("./output/%s_na.csv", Sys.Date()))
write_csv(na_qs, sprintf("./output/%s_na_qs.csv", Sys.Date()))


