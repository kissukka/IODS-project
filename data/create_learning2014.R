# Katri Kleemola, Nov. 9th 2021.  
# Creating data for data wrangling exercise week 2.
#
# Reading the data from the URL
learning <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep = "\t", header = TRUE)
# Exploring dimensions of the data. Data has 183 rows and 60 columns.
dim(learning)
# Exploring structure of the data. Most of the values are integer, 
# between 1 and 5. Values for age, attitude and points are higher. 
# The gender variable in type chr, F or M.
str(learning)
# Activating dplyr.
library(dplyr)
# Creating sum variable for attitude by dividing it by the number of items.
learning$att_sum <- learning$Attitude / 10
# Selecting the appropriate items for each approach.
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
# Creating sum variable for deep.
deep_columns <- select(learning, one_of(deep_questions))
learning$deep <- rowMeans(deep_columns)
# Creating sum variable for surface
surf_columns <- select(learning, one_of(surface_questions))
learning$surf <- rowMeans(surf_columns)
# Creating sum variable for strategic
stra_columns <- select(learning, one_of(strategic_questions))
learning$stra <- rowMeans(stra_columns)
# Selecting the appropriate variables and creating the analysis dataset.
keep_columns <- c("gender", "Age", "att_sum", "deep", "surf", "stra", "Points")
learn_df <- select(learning, one_of(keep_columns))
# Filtering out the observations where the points are zero.
learn_df <- filter(learn_df, Points > 0)
# Testing dimensions to see if I succeeded. (Yay! 166 and 7)
dim(learn_df)
# Setting the working directory to my project folder using Session and Set working directory
# Saving the dataset in data folder
write.csv(learn_df, file = "data/learn.txt", row.names = FALSE)
# Reading the dataset back
testing <- read.table("data/learn.txt", sep = ",", header = TRUE)
# Testing the structure and head. Seems to be working.
head(testing)
str(testing)
