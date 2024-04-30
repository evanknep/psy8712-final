#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import and Cleaning
file_list <- list.files("../data")

spark_detect <- str_detect(file_list, "SPARK")

spark_files <- file_list[spark_detect]
rep_files <- file_list[!spark_detect]



