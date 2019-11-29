#Iina Tuomainen
#Open data exercise 5, "Dimensionality reduction techniques". 
#28.11.2019, data: Human developmental report 2014, Work for Human Development.
#Data source: 
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")


str(hd)
str(gii)
dim(hd)
dim(gii)
summary(hd)
summary(gii)

names(hd)
names(gii)

names(hd) [1]<- "HDI"
names(hd) [3]<- "Human.HDI"
names(hd) [4]<- "Life.exp"
names(hd) [5]<- "Years.exp" 
names(hd) [6]<- "Years.mean" 
names(hd) [7]<- "GNI"
names(hd) [8]<- "GNI.calc"

names (gii) [1]<- "GII"
names (gii) [3]<- "gender.GII"
names (gii) [4]<- "mater.mor"
names (gii) [5]<- "adol.birth"
names (gii) [6]<- "rep.parl"
names (gii) [7] <- "sec.edu.F"
names (gii) [8] <- "sec.edu.M"
names (gii) [9] <- "lab.F"
names (gii) [10] <- "lab.M"

names(gii)
names(hd)

gii$edu2.FM <- gii$sec.edu.F/gii$sec.edu.M
gii$lab.FM <- gii$lab.F/gii$lab.M
names(gii)

human <- merge(gii, hd, inner_join=Country)
dim(human)

write.csv(human, file = "~/IODS-project/data/human")

read.csv(file = "~/IODS-project/data/human")
str(human)
dim(human)
#Data includes 195 observations and 19 variables. There is two characters variables, four intervals and the others are numeric.


#Mutate the Gross National Income (GrossNat) variable into numeric:
library(stringr)
library(dplyr)
str(human$GNI)
human <- mutate(human, GNI = str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric)
str(human$GNI)
str(human)

#Exclude unneeded variables 
names(human)
keep <- c("Country", "edu2.FM", "lab.FM", "Life.exp", "Years.exp", "GNI", "mater.mor", "adol.birth", "rep.parl")
human <- select(human, one_of (keep) )
names(human)

#Remove all rows with missing values
complete.cases(human)
data.frame(human [-1], comp=complete.cases(human))
human_ <- filter(human, complete.cases(human))
dim(human_)
complete.cases(human_)

#Remove the observations which relate to regions instead of countries
dim(human)
dim(human_)
tail(human_, n=10)
last <- nrow(human_) - 7
human_ <- human_[1:last,]
dim(human_)



#Define the row names of the data and remove the country name column from the data.
rownames(human_) <- human_$Country
human_ <- select(human_, -Country)
dim(human_)
str(human_)

write.csv(human_, file = "~/IODS-project/data/human_")


#Graphical overview and summaries of the variables:
library(GGally); library(ggplot2)
ggpairs(human_)
cor(human_)%>%corrplot()
#The most strongest correlation is between mater mortality ratio and life expectancy at birth, and the correlation is negative. 

#Analysis of PCA without standardized data 
pca_human <- prcomp(human_)
biplot(pca_human, choices = 1:2, cex = c(0.8,1), col = c("grey40", "deeppink2"))


#Standardize the variables in the human data and same analysis as above
human_std <- scale(human_)
pca_human2 <- prcomp(human_std)
biplot(pca_human2, choices = 1:2, cex = c(0.8,1), col = c("grey40", "deeppink2"))

library(ggplot2); library(FactoMineR); library(dplyr); library(tidyr)
data(tea)
str(tea)
dim(tea)
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")
tea_time <- select(tea, one_of(keep_columns))
summary(tea_time)
str(tea_time)

#Visualizing the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#multiple correspondence analysis
maca <- MCA(tea_time, graph = FALSE)
plot(maca, invisible=c("ind"), habillage = "quali")

