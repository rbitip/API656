# PEMEPT automatic script
# GW, refactored by AW @ PEMY

# INSTRUCTIONS ####
# Place your .xlsx/.xls/.csv input file in the "inputs/" folder
# Source this file (click top right "Source" or ctrl+shift+s)
# Follow the dialogs
# Save your plot

library(here)
library(extRemes)
library(fExtremes)
library(dplyr)
library(HDInterval)
library(utils)
library(svDialogs)
library(readxl)

# call source function on source code files in source-files
invisible(
  lapply(
    list.files(here("src")),
    function(x){source(here("src",x))}
  )
)




# perform the analysis ----

params <- auto_inputs()

analysis(params)
