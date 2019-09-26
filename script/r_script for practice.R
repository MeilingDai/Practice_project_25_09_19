#for practice

#install tidyverse packages
.libPaths(c("C:/Users/DAI035/Data School/Packages"))
library(tidyverse)
sessionInfo()

#read csv data 
read_csv("data/BOM_data.csv")
read_csv("data/BOM_data.csv")

#assign csv data as a variable
BOM_data <- read_csv("data/BOM_data.csv")


#question1:
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
# solution below
question_1 <- BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>% #seperate column of Temp_min_max, into = column "Temp_min" and "Temp_max") 
  filter(Temp_min != '-' ) %>%      #filter rows for Temp_min with numbers
filter(Temp_max != '-' ) %>%        #filter rows for Temp_max with numbers
filter(Rainfall != '-') %>%         #filter rows for Rainfall with numbers
  group_by (Station_number) %>%     #group data by station_number
  summarise(days_recorded = n())    #Count number of rows and assign as a new column named days_recorded
question_1                          #form new data
write_csv(question_1, "results/question_1.csv")   #save new data as csv file and save to folder"Result"

  
  
#question2:Which month saw the lowest average daily temperature difference?
#solution:
question_2 <- BOM_data %>% 
  separate(col = Temp_min_max, into = c("Temp_min", "Temp_max"), sep = "/") %>%   #seperate column of Temp_min_max, into = column "Temp_min" and "Temp_max") 
  mutate(daily_tem_diff = as.numeric(Temp_max) - as.numeric(Temp_min)) %>%        #creat a new column as daily_tem_diff to calculate daily tem diff    
  filter(daily_tem_diff != "NA") %>%                                              #filter rows for daily_tem_diff with meaningful numbers
  group_by(Month) %>%                                                             #group data by month
  summarise(aver_daily_tem_diff = mean(daily_tem_diff)) %>%                       #calculate average daily tem diff and assign to a new column
 arrange(aver_daily_tem_diff)                                                     #arrange data to find the month saw the lowest average daily temperature difference
question_2                                                                        #form new data
write_csv(question_2, "results/question_2.csv")                                   #save new data as csv file and save to folder"Result"
