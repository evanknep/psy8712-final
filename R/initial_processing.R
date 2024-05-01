#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyr)

# Data Import and Cleaning
file_list <- list.files("../data")
file_list <- paste("../data/", file_list, sep = "")

spark_detect <- str_detect(file_list, "SPARK")

spark_files <- file_list[spark_detect]
rep_files <- file_list[!spark_detect]

spark_master <- lapply(spark_files, read_csv) %>%
  bind_rows()
  
rep_master <- lapply(rep_files, read_csv) %>%
  bind_rows()

spark_master <- spark_master %>%
  filter(test_part == "restless") %>%
  select(-c(trial_type, trial_index, time_elapsed, internal_node_id, participantCode, success, test_part, generatedNum))
  

rep_master <- rep_master %>%
  filter(test_part == "restless") %>%
  select(-c(trial_type, trial_index, time_elapsed, internal_node_id, participantCode, success, test_part, generatedNum))
  


# write_csv(spark_master, "../out/spark_master.csv")
# write_csv(rep_master, "../out/rep_master.csv")


