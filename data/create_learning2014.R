"Iina Tuomainen, 8.11.2019. IODS exercise 2."

mydata <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE) 

str(mydata)
dim(mydata)
summary(mydata)
#183 rows, 60 variables
#122 women, 61 men, age mean 25.58, attitude mean 31.21 (min;max 14;50), points mean 20.61 (min;max 0;33) 

#instal dplyr
library(dplyr)

#questions related to deep learning, surface learning and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")


summary (deep_questions)
summary (surface_questions)
summary(strategic_questions)

#select the columns related to deep learnig, surface learning and strategic learning
deep_columns <- mydata %>% select(one_of(deep_questions))
surface_columns <- mydata %>% select(one_of(surface_questions))
strategic_columns <- mydata %>% select(one_of(strategic_questions))

#create column "attitude" by scaling the column "Attitude"
#create columns deep, surf and stra by averaging 
#exclude operations where the exam point variable is zero
#select variables gender, age, attitude, deep, stra, surf, and points
learnin2014 <- mydata %>% mutate(attitude=Attitude/10, deep=rowMeans(deep_columns, na.rm = TRUE), surf=rowMeans(surface_columns, na.rm = TRUE), stra=rowMeans(strategic_columns, na.rm = TRUE)) %>% filter(Points != 0) %>% select(gender, age=Age, attitude, deep, stra, surf, points=Points)

dim(learnin2014)

?write.csv

#change working directory to IODS-folder
setwd("~/IODS-project")

#save created data to folder data 
write.csv(learnin2014, file="~/IODS-project/data/learning2014")

read.csv(file="~/IODS-project/data/learning2014")

dim(learnin2014)
str(learnin2014)
head(learnin2014)


learnin2014
str(learnin2014)
dim(learnin2014)
#Data includes 7 following variables: 'gender', 'age', 'attitude', 'deep', 'stra', 'surf' and 'points'. Data includes 166 observations. Age and points are integers, gender is a factor and other variables are numerical.

#Graphical overview of the data and summeries of the variables 
pairs(learnin2014[-1], col = learnin2014$gender)
ggpairs(learnin2014, mapping = aes(col=gender, alpha = 0.3), lower=list(combo=wrap("facethist", bins=20)))
#Strongest correlation is between attitude and points (positive correaltion), whereas the lowest correlation is between deep and points (negative correlation). All the variables, except gender, are normally distributed. 

library(GGally)

my_model <- lm (points ~ attitude + stra + surf, data = learnin2014)
summary(my_model)
my_model2 <- lm (points ~ attitude, data = learnin2014)
summary(my_model2)
#I chose attitude, stra and surf as explanatory variables in my model. However, stra and surf did not have a significant realtionship with points variable and therefore I removed them my next model. In the final model, attitude had a significant realationship with the points. And when attitude increases by one unit, the average change in points is 3.5 points
#Multiple R-squared means that aproximately 19 % of the variation in points is explained by the variation in attitude. Therefore 81 % of the variation in points is explained by other explanatory variables.

par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))

