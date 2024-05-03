#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #setting my current directory as the working directory to avoid weird pathing issues
library(tidyr) #bringing in our tidy functions but for now hoping to avoid needing the full tidyverse

# Data Import and Cleaning
file_list <- list.files("../data") #my raw data is output as one .csv file per participant. I need to combine all ind files into a mega-file, so first I create a list of all files in the data directory
file_list <- paste("../data/", file_list, sep = "") #pasting the path information onto the start of the filenames to avoid issues when reading all the files in

spark_detect <- str_detect(file_list, "SPARK") #the raw data was from two different rounds of collection, one through the psyREP program here at the U, and one through the SPARK for autism foundation. I can separate these groups by checking for the presence of the word spark in the filename. 

spark_files <- file_list[spark_detect] #creating a list of all filenames that contain spark
rep_files <- file_list[!spark_detect] #creating a list of all filenames that do not contain spark (meaning they were collected through psyREP)

spark_master <- lapply(spark_files, read_csv) %>% #reading in each individual .csv in our spark files
  bind_rows() %>% #binding rows of each file to make one superfile
  mutate(group = "spark") #I intend to merge the spark and rep master files, so before doing that I will label all the spark data by creating a new "group" column
  
rep_master <- lapply(rep_files, read_csv) %>% #reading in each individual .csv in our rep files
  bind_rows() %>% #binding rows of each file to make one superfile
  mutate(group = "rep") #labeling this group as rep prior to merging with the spark data

combined_master <- bind_rows(spark_master, rep_master) %>% #combining rep and spark datasets now that they've been labeled
  filter(test_part == "restless") %>% #each individual trial has a "test_part" value associated with it, the rows that contain NA here are only saving server info in between trials so we can filter them out. In the spark sample we only used the restless bandit task, so we can filter out all the NA by only including rows that contain a restless value in the test_part column.
  select(-c(trial_type, trial_index, time_elapsed, internal_node_id, participantCode, success, test_part, generatedNum)) #removing irrelevant columns. Some of it is server info from Pavlovia.org, some it is simply redundant information that we've used for organizing files
  

# write_csv(spark_master, "../out/spark_master.csv") #writing these to csvs to avoid needing to do the pre-processing each time
# write_csv(rep_master, "../out/rep_master.csv") #writing these to csvs to avoid needing to do the pre-processing each time


combined_master <- combined_master %>% #cleaning up the columns and ensuring the datatypes are correct
  mutate(group = factor(group, ordered = FALSE)) %>% #factorizing my group variable, initially it was being treated as just a string 
  mutate(rt = as.numeric(rt)) %>% #changing my reaction time values to numeric
  mutate(key_press = case_when( #for readability, changing the key_press values from javascript keycode values to one that will make more sense to someone looking at the data. Each keypress (37,38,39) corresponds to the left,center,right arrow keys
    key_press == "37" ~ "left",
    key_press == "38" ~ "center", 
    key_press == "39" ~ "right"
  )) %>%
  mutate(stimulus = sub(".*/(.*?)\\.jpg$", "\\1", stimulus)) %>% 
  mutate(chosen_image = case_when(
    key_press == "left" ~ substring(stimulus, 1,2),
    key_press == "center" ~ substring(stimulus, 3,4),
    key_press == "right" ~ substring(stimulus, 5,6)
  )) %>%
  mutate(walkNumber = str_remove(walkNumber, ".csv"))

# write_csv(combined_master, "../out/combined_master.csv") #saving combined master file so I don't need to re-run the pre-processing each time


# Analysis


  


