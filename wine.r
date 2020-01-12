library(repr)
library(maptree)
library(tree)
library(caret)
library(forcats)
library(dplyr)
library(rpart.plot)
library(rpart)
drawHistogram <- function(wines){
  options(repr.plot.width=6, repr.plot.height=6)
  element_samples <- summary(wines$quality)
  midpoints <- barplot(element_samples, col = "peachpuff1")
  abline(h = nrow(wines), lty = 2, col = 2)
  num_elements <- dim(data.wine)[1]
  num_types <- length(element_samples)
  y_coord <- element_samples
  max_val <- max(element_samples)
  for (j in seq(1, num_types)) {
    if (element_samples[j] / max_val > 0.9){
      y_coord[j] <- y_coord[j] - 0.1 * max_val 
    }
    else{
      y_coord[j] <- y_coord[j] + 0.05 * max_val
    }
        
    
  }
  text(midpoints, y_coord, labels=element_samples)
}

makeDataSets <- function(wines){
  set.seed(12345) #constant seed for result reproducibility
  n <- dim(wines)[1]
  data <- wines[order(runif(n)),]
  n.train <- as.integer(0.8 * n)
  data.train <- data[1:n.train,]
  data.test <- data[(n.train + 1):n,]
  return (list(train = data.train, test = data.test))
}

testModel <- function(tree, test_data, train_data){
  
  predict.test <- predict(tree, test_data, type = "class")
  predict.train <- predict(tree, train_data, type = "class")
  
  result.test <- table(test_data$quality, predict.test)
  result.train <- table(train_data$quality, predict.train)
  
  accuracy.test <- sum(diag(result.test)) / sum(result.test)
  accuracy.train <- sum(diag(result.train)) / sum(result.train)
  print("test data prediction accuracy: ")
  print(accuracy.test)
  print("train data prediction accuracy:")
  print(accuracy.train)
  
  #confusion matrix
  caret::confusionMatrix(test_data$quality, predict.test)
  
}


combineClasses <- function(wines){
  df = wines
  df %>%
    mutate(quality = fct_collapse(df$quality,
                                  Low = c("3","4","5"),
                                  Medium = c("6"),
                                  High = c("7","8","9")))->x
  return(x)
}


#----------------------------------------------------------------
# Read data
data.wine <- read.table('data/winequality-white.csv', 
                        sep=';', 
                        header=TRUE, 
                        na.strings="NA",
                        stringsAsFactors=T)

# Make 'quality' vector to be factor
data.wine$quality <- as.factor(data.wine$quality)

# Remove 'NA' rows from table. 
# N.B. In file 'data/winequality-white.csv' there is no NA values, so this line is irrelevant
data.wine <- data.wine[complete.cases(data.wine),]

# Scale all columns in table except 'quality', which is factor column
data.wine[, -dim(data.wine)[2]] <- scale(data.wine[, -dim(data.wine)[2]])

drawHistogram(data.wine)

data <- makeDataSets(data.wine)

options(repr.plot.width=13, repr.plot.height=7)

drawHistogram(data$train)

drawHistogram(data$test)

#build model
wine.tree <- tree(quality ~., data$train)

testModel(wine.tree, data$test, data$train)

#draw tree
options(repr.plot.width = 14, repr.plot.height = 10)
draw.tree(wine.tree, cex = 0.75)
# another method (tree)
wine.rpart <- rpart(quality ~., data$train)
testModel(wine.rpart, data$test, data$train)
draw.tree(wine.rpart, cex = 0.75)
rpart.plot(wine.rpart, 
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE,
           roundint = FALSE
)

#-----------------------------------------------------------------------
# combine classes
x <- combineClasses(data.wine)
drawHistogram(x)
# build model
xdata <- makeDataSets(x)
x.tree <- tree(quality ~., xdata$train)
# test
testModel(x.tree, xdata$test, xdata$train)
options(repr.plot.width = 14, repr.plot.height = 10)
draw.tree(x.tree, cex = 0.75)
# draw
x.rpart <- rpart(quality ~., xdata$train)
testModel(x.rpart, xdata$test, xdata$train)
draw.tree(x.rpart, cex = 0.75)
rpart.plot(x.rpart, 
           type=4,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE,
           roundint = FALSE
)
