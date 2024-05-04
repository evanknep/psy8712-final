#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #setting my current directory as the working directory to avoid weird pathing issues
library(tidyr) #bringing in our tidy functions but for now hoping to avoid needing the full tidyverse
library(stringr) #for string detection 
library(caret) #for machine learning

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
  

ind_summary_stats <- combined_master %>% #the combined_master doc has trial by trial data for each participant, but for the purposes of this project I am interested in comparing the summary statistics of each participant.
  group_by(group, subject) %>% #grouping by individual and subject before getting summary stats. The calculations will be done on a subject level here but I included group to retain that column
  summarise( #summarise is great here because it will return one row for each combination of grouping variables, which in this case is one row per subject
    num_trials = n(), #getting total number of trials completed by counting the number of rows with each subject id
    num_correct = sum(correct), # returning a sum of the TRUE values (which convert to 1) to quantify how many times each participant was rewarded
    p_reward = num_correct / num_trials, #dividing the number of rewards by number of trials gives us a percent rewarded
    p_chance = mean(chance), #each participant will have a slightly different set of reward probabilities given the randomness built into the task. To observe how well each participant did we need to incorporate chance level. Using mean() gives me an average chance level for each participant
    reward_over_chance = p_reward - p_chance, # to account for how chance level might play into percent rewarded, we can subtract chance from the percent rewarded
    avg_rt = mean(rt, na.rm = T), #using mean() to return an average reaction time, na.rm tells it to strip missing values
    median_rt = median(rt), #even though we filtered some outliers, our mean rt differs quite a bit from our median so I wanted to include this as well
    sd_rt = sd(rt) #another metric for viewing reaction time
  ) %>%
  arrange(desc(reward_over_chance)) #using this to sort by reward over chance. Arrange() works better than sort() here because arrange is designed to work directly with dataframes/tibbles. sort() takes in a vector but does not fit as seemlessly into our dpylr pipeline


choice_distribution <- combined_master %>% # Calculating the distribution of chosen images by group
  group_by(subject) %>% #grouping by subject to ensure that calculations are done by subject instead of across the entire dataframe
  count(key_press) %>% #using count() to return the number of times each key was pressed by each participant. This results in 3 rows for each participant  
  pivot_wider(names_from = key_press, values_from = n) %>% #pivot_wider to reduce the number of rows per participant from 3 to 1. Taking the name value from key_press to add columns for left,center,right, and the values from = n returns the count for each key_press
  mutate(p_left = left/sum(left,right,center), #using mutate to create three new columns based on key_press. by dividing the values of left/right/center by the sum of all combined, I can see how much each participant favored each choice
         p_right = right/sum(left,right,center),
         p_center = center/sum(left,right,center)) %>%
  mutate(max_choice_bias = max(p_left, p_right, p_center)) %>% #using mutate to create another new column, this time pulling the max value of the three columns indicating how often a participant chose each choice. This lets me know how much each participant favored one choice over the others, and can be an indicator of task engagement
  select(max_choice_bias, subject) #taking our max choice bias calculation and subject columns to join with our main ind_summary database

ind_summary_stats <- left_join(ind_summary_stats, choice_distribution) %>% #using left join to join our smaller choice_distribution dataframe onto our main ind_summary_stats dataframe. Because of my initial cleaning above left,right, or inner join should produce identical results, but I've chosen left_join because in most cases where I am appending to a main dataframe I would only want the choice_distribution values for participants that are also in my main file
  select(-subject, -num_trials) #removing unnecessary columns

# saveRDS(ind_summary_stats, "../shiny/ind_summary.rds") #saving to RDS for shiny app
# write_csv(ind_summary_stats, "../out/summary_stats.csv") #for our .doc visualizations

# ML component

holdout_indices <- createDataPartition(ind_summary_stats$group, #using createDataPartition to create our train/test split partitions. In this case I've decided to holdout 25% of the dataset as our testset
                                       p = .25,
                                       list = T)$Resample1
training_tbl <- ind_summary_stats[-holdout_indices,] #taking the indices that were not listed in our holdout_indices
test_tbl <- ind_summary_stats[holdout_indices,] #taking only our holdout indices

training_folds <- createFolds(training_tbl$group) #for cross validation, we can bootstrap our training set to hopefully provide better performance without overfitting

training_control <- trainControl(method = "cv", #setting up our control function for the actual model training. cv means we are using cross validation
                                 number = 10, #our desired number of folds for cross validation
                                 indexOut = training_folds, #lets us set custom folds, because we created training_folds above. Good for reproducibility
                                 verboseIter = TRUE #shows more line by line output while training, which is good for troubleshooting and viewing progress
)

m_glm <- train(
  group ~ ., #predicting group off of all other features
  training_tbl, #running this only on the training set
  method = "glmnet", #glms are highly versatile and can provide interpretable results, which would let us know which features are of most importance for predicting spark vs rep
  preProcess = c("center","scale", "nzv", "medianImpute"), #we can use caret's built in preprocessing steps before training our model. Center subtracts the mean of each variable from each value, bringing all of our features onto a similar scale. Scale continues this process by then dividing each value by the sd. We are essentially z-scoring our data in these first two steps. NZV stands for near zero variance, and it removes columns that have near zero variance as they will provide no real value as a predictor. Removing these features can increase training speed and simplify the output. medianImpute is my chosen method of handling missing values. Missing data will be replaced with the median value from that feature.
  na.action = na.pass, #our chosen method of na handling, indicating that we are passing them on to the algorithm as is
  trControl = training_control) #using our predefined trainControl to specify how we want our model to run

glm_train_predictions <- predict(m_glm, training_tbl) #running our trained model on our training set to determine training accuracy


glm_training_accuracy <- sum(glm_train_predictions == training_tbl$group) / length(training_tbl$group) #comparing our predicted classes from our training set to actual classes

glm_test_predictions <- predict(m_glm, test_tbl) #running our trained glm model on our test set to determine test accuracy
glm_test_accuracy <- sum(glm_test_predictions == test_tbl$group) / length(test_tbl$group) #comparing our predicted classes from our test set to actual classes


m_rf <- train( #running a second model using random forest
  group ~ ., #again trying to predict group based on all other variables
  training_tbl, #using only training data
  method = "ranger", #using ranger for random forest, which is a great algorithm for unpacking complex relationships, and particularly should perform better than our glm if the relationship is nonlinear.
  preProcess = c("center","scale", "nzv", "medianImpute"), #we can use caret's built in preprocessing steps before training our model. Center subtracts the mean of each variable from each value, bringing all of our features onto a similar scale. Scale continues this process by then dividing each value by the sd. We are essentially z-scoring our data in these first two steps. NZV stands for near zero variance, and it removes columns that have near zero variance as they will provide no real value as a predictor. Removing these features can increase training speed and simplify the output. medianImpute is my chosen method of handling missing values. Missing data will be replaced with the median value from that feature.
  na.action = na.pass, #our chosen method of na handling, indicating that we are passing them on to the algorithm as is
  trControl = training_control)  #using our predefined trainControl to specify how we want our model to run

rf_train_predictions <- predict(m_rf, training_tbl) #running our trained model on our training set to determine training accuracy
rf_training_accuracy <- sum(rf_train_predictions == training_tbl$group) / length(training_tbl$group) #comparing our predicted classes from our training set to actual classes

rf_test_predictions <- predict(m_rf, test_tbl) #running our trained rf model on our test set to determine test accuracy
rf_test_accuracy <- sum(rf_test_predictions == test_tbl$group) / length(test_tbl$group) #comparing our predicted classes from our test set to actual classes


m_xgb <- train( #running a third model using xgboost
  group ~ ., #again trying to predict group based on all other variables
  training_tbl, #using only training data
  method = "xgbTree", #xgboost is great for handling complex relationships in data, both linear and nonlinear. It will be a great indicator of if there are actually predictable group differences in our dataset
  preProcess = c("center","scale", "nzv", "medianImpute"),  #we can use caret's built in preprocessing steps before training our model. Center subtracts the mean of each variable from each value, bringing all of our features onto a similar scale. Scale continues this process by then dividing each value by the sd. We are essentially z-scoring our data in these first two steps. NZV stands for near zero variance, and it removes columns that have near zero variance as they will provide no real value as a predictor. Removing these features can increase training speed and simplify the output. medianImpute is my chosen method of handling missing values. Missing data will be replaced with the median value from that feature.
  na.action = na.pass, #our chosen method of na handling, indicating that we are passing them on to the algorithm as is
  trControl = training_control)  #using our predefined trainControl to specify how we want our model to run

xgb_train_predictions <- predict(m_xgb, training_tbl) #running our trained model on our training set to determine training accuracy
xgb_training_accuracy <- sum(xgb_train_predictions == training_tbl$group) / length(training_tbl$group) #comparing our predicted classes from our training set to actual classes

xgb_test_predictions <- predict(m_xgb, test_tbl) #running our trained model on our test set to determine test accuracy
xgb_test_accuracy <- sum(xgb_test_predictions == test_tbl$group) / length(test_tbl$group) #comparing our predicted classes from our test set to actual classes


# Visualizations 

