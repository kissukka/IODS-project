# Katri Kleemola, Nov. 16th 2021.  
# Creating data for data wrangling exercise week 3.
#
# The data has been downloaded and unzipped as instructed. 
#
# Reading the data in. 
math_data <- read.table("data/student-mat.csv", sep = ";", header=TRUE)
por_data <- read.table("data/student-por.csv", sep = ";", header=TRUE)
#Exploring the dimensions of datasets below. There are 33 variables in both datasets, 
# 395 observations in the Math data and 649 observations in the Portuguese dataset.
dim(math_data)
dim(por_data)
#Exploring the structure of datasets below. Variables are either types chr or int.
str(math_data)
str(por_data)
#Trying to join the datasets using Reijo's code. I have copy-pasted
#from Reijo's file the stuff below.
#Defining own id's for datasets, first activating dplyr.
library(dplyr)
por_id <- por_data %>% mutate(id=1000+row_number()) 
math_id <- math_data %>% mutate(id=2000+row_number())
# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")
# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)
pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))
# Combine datasets to one long data
#   NOTE! There are NO 382 but 370 students that belong to both datasets
#         Original joining/merging example is erroneous!
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data (more joining variables than in the example)  
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )
#Finding out if it worked or not. At least, the number of observations
#seems to be correct. I have 370 obs and 51 variables.
dim(pormath)
str(pormath)
#At this point I realized that with Reijo's script, I completed stages 5 and 6
# of the data wrangling exercise as well, because I thought it was all
#about stage 4. So please do not grant me points for those stages!
#However, next I moved on to stage 7 to save the data.
write.csv(pormath, file = "data/alc.txt", row.names = FALSE)
