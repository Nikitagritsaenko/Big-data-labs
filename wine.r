library(mlbench)
library(repr)
library(maptree)
library(tree)

# Read data
data.wine <- read.table('winequality-white.csv', 
                        sep=';', 
                        header=TRUE, 
                        na.strings="NA",
                        stringsAsFactors=T)

set.seed(12345)

custom.panel <- function() {
     axis(1, tck=1, col.ticks="light gray")
     axis(1, tck=-0.015, col.ticks="black")
     axis(2, tck=1, col.ticks="light gray")
     axis(2, tck=-0.015)
}

head(data.wine)

data.wine$quality <- as.factor(data.wine$quality)

data.wine[, -dim(data.wine)[2]] <- scale(data.wine[, -dim(data.wine)[2]])

summary(data.wine)

n <- dim(data.wine)[1]
n

#
#data.wine <- data.wine[-sample(which(as.character(data.wine$quality) == "6"), 2060),]
#data.wine <- data.wine[-sample(which(as.character(data.wine$quality) == "5"), 1320),]
#data.wine <- data.wine[-sample(which(as.character(data.wine$quality) == "7"), 750),]
data.wine$quality <- as.factor(data.wine$quality)

options(repr.plot.width=6, repr.plot.height=6)
element_samples <- summary(data.wine$quality)
barplot(element_samples, col = "peachpuff1")
abline(h = nrow(data.wine), lty = 2, col = 2)

data.wine <- data.wine[complete.cases(data.wine),]

data <- data.wine
n <- dim(data.wine)[1]
data <- data[order(runif(n)),]
#func 
n.train <- as.integer(0.8 * n)
data.train <- data[1:n.train,]
data.test <- data[(n.train + 1):n,]

options(repr.plot.width=13, repr.plot.height=7)
par(mfrow = c(1, 2))

element_samples <- summary(data.train$quality)#apply(data.train[c(-10)] != 0, 2, sum)
barplot(element_samples, col = "peachpuff1")
abline(h = nrow(data.train), lty = 2, col = 2)
title(main = "Train set types number")

element_samples <- summary(data.test$quality)#apply(data.test[c(-10)] != 0, 2, sum)
barplot(element_samples, col = "peachpuff1")
abline(h = nrow(data.test), lty = 2, col = 2)
title(main = "Test set types number")

wine.tree <- tree(quality ~., data.train)
predict.test <- predict(wine.tree, data.test, type = "class")
predict.train <- predict(wine.tree, data.train, type = "class")

result.test <- table(data.test$quality, predict.test)
result.train <- table(data.train$quality, predict.train)

accuracy.test <- sum(diag(result.test)) / sum(result.test)
accuracy.train <- sum(diag(result.train)) / sum(result.train)
accuracy.test
accuracy.train

options(repr.plot.width = 14, repr.plot.height = 10)
draw.tree(wine.tree, cex = 0.75)
