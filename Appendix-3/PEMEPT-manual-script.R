# PEMEPT manual script
# GW, refactored by AW @ PEMY

# INSTRUCTIONS ####
# Place your .xlsx/.xls/.csv input file in the "inputs/" folder
# Setup the analysis parameters below (file.name, column numbers, etc)
# Source this file (click top right "Source" or ctrl+shift+s)
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

# setup the analysis parameters ----

file.name <- "Rain Baytown TX.csv" # the input file name in the inputs/ folder
MRI.col = 1 # the column corresponding to MRI in the input file
y.col = 3 # the column corresponding to y or exceedance in the input file
y.label = "Exceedance" # y-axis label
Title = "Exceedance Projection" # plot title
MRI.proj = c(1000, 2475) # MRI values to be projected




# perform the analysis ----

params <- manual_inputs(
  file.name = here("inputs", file.name),
  MRI.col = MRI.col,
  y.col = y.col,
  y.label = y.label,
  Title = Title,
  MRI.proj = MRI.proj
)

analysis(params)
