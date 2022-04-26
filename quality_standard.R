library(tidyverse)
library(rvest)
library(rlang)

# Source functions written elsewhere
source('./src/extract_qs_functions.R')
source('./src/extract_statements_functions.R')

# Create output folder if it doesn't already exist
if(!dir.exists("output")) {
    dir.create("output")
}

# Specify which QSs to pull
to_pull <- 22


for (qs_number in to_pull) {
    
    # Run custom function performing extraction for given QS
    extract_qs(qs_number)
    
    # Generate message in console when run
    message(sprintf("QS%s extracted. Check output for errors.", qs_number))
}
