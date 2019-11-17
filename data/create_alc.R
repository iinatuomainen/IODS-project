#Iina Tuomainen, 17.11.2019
#IODS excersice 3, data: UCI Maching Learning Repsotory, Student Performance Data Set
#Data available also at:
# https://archive.ics.uci.edu/ml/datasets/Student+Performance


#Read both data into R. Exploring the stuctures and dimensions of the data
MAT <- read.csv(file = "~/IODS-project/data/mat")
POR <- read.csv(file = "~/IODS-project/data/por")
str(MAT)
str(POR)
dim(MAT)
dim(POR)


#Join two data sets using variables "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
library(dplyr)
math_por <- inner_join(MAT, POR, by = join_by, suffix = c(".MAT", ".POR"))
dim(math_por)
str(math_por)

#The if-else structure (copy fro DataCamp execise)
alc <- select(math_por, one_of(join_by))
notjoined_columns <- colnames(MAT)[!colnames(MAT) %in% join_by]
notjoined_columns
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

#Average of the answers related to weekday and weekend alcohol consumption to create a new column "alc_use". Create "high_use", where TRUE is for students for which "alc_use" is greater than 2, otherwise FALSE 
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
alc <- mutate(alc, high_use = alc_use > 2)

#Glimpse data and save data set to "data folder"
glimpse(alc)
write.csv(alc, file = "~/IODS-project/data/alc")

#Read data and print out the names of the varibles in the data
read.csv(file = "~/IODS-project/data/alc")
colnames(alc)
dim(alc)
#Data includes 35 variables from 385 observations. Data have information about students' school (2 different schools), their parents' education and work, information about shcool success, information about students' freetime. In addition, data includes information about students' alcohol consumption.

#Study the relationships between high/low alcohol consumption and other variables in the data. I choose variables such as mother education, activities, higher and health. My hypothesis is that higher mother education and higher current health status are associated with low alcohol consumption. Moreover, extra-curricular activities (yes) and students who wants to take higher education are related to lower alcohol consumption.

#Explore numerically and graphically the distributions of variables and their relationships with alcohol consumption. 
mean(alc$Medu)
mean(alc$health)
mean(alc$alc_use)
summary(alc$higher)
summary(alc$activities)
summary(alc$Medu)
summary(alc$health)
summary(alc$alc_use)
summary(alc$high_use)

alc %>% group_by(Medu, high_use) %>% summarise(count=n(), mean_edu=mean(Medu))

#Median of alcohol use is 1.5 and 112 of the patients alcohol consumption is more than point 2 from 1 to 5 scale during one week. Mean of students' mother eduaction is 2.8, which is close to 3 as secundary education. Mean of students' current health status is 3.57, which is average from scale 1 from 5. 201 students have extra-curricular activities and 364 students wants to take higher education.

#Logistic regression analyses
m1 <- glm(high_use ~ Medu + activities + higher + health, data = alc, family = "binomial")
summary(m1)
OR <- coef(m1) %>% exp
CI <- confint(m1)%>% exp

cbind(OR, CI)

#None of the selected variables were significantly associated with the higher alcohol use consumption. 
#Coefficient values for variabes were: Medu 1.07 (95% CI 0.2-2.2), activities (yes) 0.79 (0.9-1.3), higher (yes) 0.42 (0.2-1.1) and health 1.09 (0.9-1.3). If the associations were statistically significant, if the higher health or mother education values grow by one unit, the probability that students would be higher alcohol consumption are 6% for Medu and 9% for health. If student wants to higher education (yes) or she/he have extra-curricular activities (yes), the probability to be lower alcohol use would be 86 % for higher education and 22 % for activity.
#Coefficient value is odds ratios between a unit change in explanatory variable. That means if healht change for one unit, the probability to be higher alcohol user would be 9 %. If the predictor variable is binary: if student replied yes for higher education question, she or he has a 86 % probability to be LOWER alcohol consumption user. Again these are not true because of missing statistically significant p-values.

#I am using the variable mother education. 
#2x2 cross tabulation of predictors versus the actual values
library(ggplot2); library (dplyr)
table(high_use=alc$high_use, Medu=alc$Medu) %>% prop.table() %>% addmargins()

#predict the probability of high_use
probabilities <- predict(m1, type = "response")

#add the predicted probabilities to "alc"
alc <- mutate(alc, probability = probabilities)

#use the probabilities to make prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

#tabulate the target variable versus predictions 
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col=prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table() %>% addmargins()
#I guess data shows 263 true negative values and 3 true positive valus. Likewise, there is 111 false negative and 5 false positive values.

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data


loss_func(class = alc$high_use, prob = alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m1, K = nrow(alc))

# average number of wrong predictions in the cross validation
cv$delta[1]

#The mean prediction error in my model is 0.30, therefore my model has worse test set performance. Lower number is better for better model.  