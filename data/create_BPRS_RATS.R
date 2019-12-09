#Iina Tuomainen, IODS-course, exercise 6, 9.12.2019
#Data can be read here: https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt and here https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt

#Open the data and look the structure, collumn names and summary of the data

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

dim(BPRS)
dim(RATS)

colnames(BPRS)
colnames(RATS)

str(BPRS)
str(RATS)

summary(BPRS)
summary(RATS)

#BPRS includes 40 observations and 11 variables. There is 2 categorical variables and al the other variables are intervals. Data RATS includes 16 observations and 13 variables. There is 2 categorial variables and all the other variables are also intervals.

#Convert the categorial variables to factors

BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

str(BPRS)
str(RATS)

#Convert the data sets to long format and add a week variable to BPRS and a Time variable to RATS

library(dplyr); library(tidyr)
BPRSL <- BPRS %>% gather (key=weeks, value=bprs, -treatment, -subject)
BPRSL <- BPRSL %>% mutate (week= as.integer(substr(weeks, 5,5)))                
glimpse(BPRSL)

RATSL <- RATS %>% gather (key=WD, value=Weight, -ID, -Group) %>% mutate(Time=as.integer(substr(WD, 3,4)))
glimpse(RATSL)

#Look at the new data sets and compare them with their wide form versions.

colnames(BPRSL)
colnames(RATSL)

str(BPRSL)
str(RATSL)

summary(BPRSL)
summary(RATSL)

#In wide format, in the BPRSL data, there is 360 observations and 5 variables. There is 2 factor variables, 2 intervals and one character. In the RATSL, there is 176 observations and 5 variables. There is also 2 factor variables, 2 intervals and one character variables.
#In wide format, weeks from 0 to 8 are converteted to on variable in BPRSL data. In RATSL data, WD from 1 to 64 are converted to on variable as well. 

