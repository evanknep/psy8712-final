#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #setting my current directory as the working directory to avoid weird pathing issues
library(tidyr) #bringing in our tidy functions but for now hoping to avoid needing the full tidyverse

# Data Import and Cleaning
file_list <- list.files("../data") #my raw data is output as one .csv file per participant. I need to combine all ind files into a mega-file, so first I create a list of all files in the data directory
file_list <- paste("../data/", file_list, sep = "") #pasting the path information onto the start of the filenames to avoid issues when reading all the files in

spark_detect <- str_detect(file_list, "SPARK") #the raw data was from two different rounds of collection, one through the psyREP program here at the U, and one through the SPARK for autism foundation. I can separate these groups by checking for the presence of the word spark in the filename. 

spark_files <- file_list[spark_detect] #creating a list of all filesnames that contain spark
rep_files <- file_list[!spark_detect] #creating a list of all filenames that do not contain spark (meaning they were collected through psyREP)

spark_master <- lapply(spark_files, read_csv) %>% #reading in each individual .csv in our spark files
  bind_rows() #binding rows of each file to make one superfile
  
rep_master <- lapply(rep_files, read_csv) %>% #reading in each individual .csv in our rep files
  bind_rows() #binding rows of each file to make one superfile

spark_master <- spark_master %>% #the raw output files contain a good deal of unnecessary information, including a great deal of information saved to the server in between trials, none of which is relevant to us. 
  filter(test_part == "restless") %>% #each individual trial has a "test_part" value associated with it, the rows that contain NA here are only saving server info in between trials so we can filter them out. In the spark sample we only used the restless bandit task, so we can filter out all the NA by only including rows that contain a restless value in the test_part column.
  select(-c(trial_type, trial_index, time_elapsed, internal_node_id, participantCode, success, test_part, generatedNum)) %>% #removing irrelevant columns. Some of it is server info from Pavlovia.org, some it is simply redundant information that we've used for organizing files
  mutate(group = "spark") #I intend to merge the spark and rep master files, so before doing that I will label all the spark data by creating a new "group" column
  

rep_master <- rep_master %>% #the rep files require a bit more filtering because these participants completed several more rounds of tasks after completing the bandit task. 
  filter(test_part == "restless") %>% #filtering for restless here will remove not only the inter-trial info that was present in the spark dataset, but also spatial and visual task components
  select(-c(trial_type, trial_index, time_elapsed, internal_node_id, participantCode, success, test_part, generatedNum)) %>% #removing the same irrelevant columns as in our spark data
  mutate(group = "rep") #labeling this group as rep prior to merging with the spark data
  

# write_csv(spark_master, "../out/spark_master.csv") #writing these to csvs to avoid needing to do the pre-processing each time
# write_csv(rep_master, "../out/rep_master.csv") #writing these to csvs to avoid needing to do the pre-processing each time

combined_master <- bind_rows(spark_master, rep_master) #combining rep and spark datasets now that they've been labeled





