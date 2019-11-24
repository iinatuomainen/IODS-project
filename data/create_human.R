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
names(hd) [7]<- "GrossNat"
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

gii$ratio.edu2 <- gii$sec.edu.F/gii$sec.edu.M
gii$ratio.lab <- gii$lab.F/gii$lab.M
names(gii)

human <- merge(gii, hd, inner_join=Country)
dim(human)

write.csv(human, file = "~/IODS-project/data/human")


