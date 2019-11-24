library(MASS)
data("Boston")
str(Boston)
dim(Boston)

pairs(Boston)
summary(Boston)
library(corrplot)
m <- cor(Boston)
corrplot(m, method = "circle")

boston_scaled <- scale(Boston)
summary(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)

summary(boston_scaled$crim)
bins <- quantile(boston_scaled$crim)
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(crime)

boston_scaled <- dplyr::select(boston_scaled, -crim)
boston_scaled <- data.frame(boston_scaled, crime)

n <- nrow(boston_scaled)
ind <- sample(n, size = n*0.8)
train <- boston_scaled[ind, ]
test <- boston_scaled

lda.fit <- lda(crime ~ ., data=train)
classes <- as.numeric(train$crime)
plot(lda.fit, dimen = 2, col = classes, pch = classes)

lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

plot(lda.fit, dimen = 2, col = classes, pch = classes) 
lda.arrows(lda.fit, myscale = 1)


crime_cat <- test$crime
test <- dplyr::select(test, -crime)
summary(test)

lda.pred <- predict(lda.fit, newdata = test)
table(correct = crime_cat, predicted = lda.pred$class)


data('Boston')
Boston2 <- scale(Boston)
dist_eu <- dist(Boston2)
summary(dist_eu)

set.seed(123)
k_max <- 10
twcss <- sapply(1:k_max, function(k){kmeans(Boston2, k)$tot.withinss})

library(ggplot2)
qplot(x=1:k_max, y=twcss, geom = 'line')

km <- kmeans(Boston2, centers = 2)
pairs(Boston2, col= km$cluster)

