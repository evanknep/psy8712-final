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
  mutate(stimulus = sub(".*/(.*?)\\.jpg$", "\\1", stimulus)) %>% #pulling out specific stimulus identity from the path to the image file, grabbing the text in between the final / and the .jpg
  mutate(chosen_image = case_when( # the stimulus file name grabbed in the previous mutate is named by the combination of the 3 images shown to the participant. So the first two characters refer to the left image, the middle two characters refer to the middle image, and the last two characters refer to the right image. I can look at image chosen by pulling those specific characters based on what the keypress was
    key_press == "left" ~ substring(stimulus, 1,2),
    key_press == "center" ~ substring(stimulus, 3,4),
    key_press == "right" ~ substring(stimulus, 5,6)
  )) %>%
  mutate(walkNumber = str_remove(walkNumber, ".csv")) %>% #participants were given random "walks", that are the potential jumps in reward probability for each option. While they were generated to have the same environmental richness over the course of the task, the different walks could theoretically influence behaviors.
  mutate(z_score = scale(rt)) %>% #exploratory data analysis revealed some significant outliers in reaction time, because there was no limited hold on each trial. Calculating the z-score of the column and filtering out values above 3 or below -3 allows us to remove rts that are more than 3 standard deviations away from the mean
  filter(abs(z_score) <= 3) %>% # ^
  rowwise() %>%
  mutate(chance = sum(leftProb, midProb, rightProb) / 3) # looking at probability of reward of each participant can provide a skewed sense of performance, because the random component of the task means that different participants will be exposed to difference levels of chance. By calculating the chance each trial and averaging those values per participant (in analysis), we can determine each participants performance above chance
  

# write_csv(combined_master, "../out/combined_master.csv") #saving combined master file so I don't need to re-run the pre-processing each time

# combined_master <- read_csv("../out/combined_master.csv") #for importing dataset rather than recreating each time


ind_summary_stats <- combined_master %>%
  group_by(group, subject) %>%
  summarise(
    num_trials = n(),
    num_correct = sum(correct),
    p_reward = num_correct / num_trials,
    p_chance = mean(chance),
    reward_over_chance = p_reward - p_chance,
    avg_rt = mean(rt, na.rm = T),
    median_rt = median(rt),
    sd_rt = sd(rt)
  ) %>%
  arrange(desc(reward_over_chance))

write_csv(ind_summary_stats, "../out/ind_summary.csv")


ind_summary_stats %>%
  group_by(group) %>%
  summarise(
    avg_performance = mean(reward_over_chance),
    avg_num_trials = mean(num_trials),
    avg_rt = mean(avg_rt, na.rm = T),
  ) %>%
  View()

# Calculate the distribution of chosen images by group
choice_distribution <- combined_master %>%
  group_by(subject) %>%
  count(key_press) %>%
  pivot_wider(names_from = key_press, values_from = n) %>%
  mutate(p_left = left/sum(left,right,center),
         p_right = right/sum(left,right,center),
         p_center = center/sum(left,right,center)) %>%
  mutate(max_choice_bias = max(p_left, p_right, p_center)) %>%
  select(max_choice_bias, subject)


ind_summary_stats <- left_join(ind_summary_stats, choice_distribution)



# Visualizations  

ind_summary_stats %>%
  ggplot(aes(x=reward_over_chance, fill = group)) +
  geom_density(position = "stack")
