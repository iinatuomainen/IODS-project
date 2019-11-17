
#open files studen-mat.csv and student-por.csv
mat <- file.path("~", "desktop", "student", "student-mat.csv")
por <- file.path("~", "desktop", "student", "student-por.csv")

MAT <- read.csv(mat, header = TRUE, sep = ";", stringsAsFactors = TRUE)
POR <- read.csv(por, header = TRUE, sep = ";", stringsAsFactors = TRUE)

write.csv(MAT, file="~/IODS-project/data/MAT")
write.csv(POR, file="~/IODS-project/data/POR")

read.csv(file = "~/IODS-project/data/mat")
read.csv(file = "~/IODS-project/data/por")
dim(POR)
dim(MAT)

alc <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep=",", header=TRUE) 
dim(alc)
