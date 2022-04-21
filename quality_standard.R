# library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(purrr)

library(rvest)
library(rlang)

source('./src/extract_qs_functions.R')
source('./src/extract_statements_functions.R')

# Create output folder if it doesn't already exist
if(!dir.exists("output")) {
    dir.create("output")
}

to_pull <- 206

for (qs_number in to_pull) {
    
    extract_qs(qs_number)
    
    message(sprintf("QS%s extracted. Check output for errors.", qs_number))
}
