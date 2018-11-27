library(readr)
library(dplyr)
library(RSQLite)
library(PhysicalActivity)

source("./Functions/acceldata_prep.R")

acceldata_prep(input_dir = "./Data", file_pattern = "*1secAGdata.csv", output_dir = "./Outputs",
               naming_start = 1, naming_stop = 2)