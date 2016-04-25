#```{r setup, include = F}
#knitr::opts_chunk$set(cache = T, 
#                      echo = F, 
#                      warning = F, 
#                      message = F)
#```

library(caret)
library(randomForest)


cleanData <- function(rawData){
  # toss columns with NAs that affect train()
  # columns 1:7 have personnel IDs and timestamps that don't help MLA
  # numeric columns with #DIV/0! should be removed as well.

  classe        <- rawData$classe
  naColumns     <- colSums(is.na(rawData)) > 0
  numericCols   <- sapply(rawData, is.numeric)
  dropTheSeven  <- c(rep(F, 7), rep(T, ncol(rawData) - 7))
  keepColumns   <- as.logical(!naColumns & numericCols & dropTheSeven)
  keepRows      <- complete.cases(rawData)

  df        <- rawData[, keepColumns]
  df$classe <- classe
  df
}

rawTrain  <- read.csv('C:/Users/mcdanid2/Desktop/pml-training.csv')
trainData <- cleanData(rawTrain)
trainRows <- createDataPartition(trainData$classe, p = 0.8, list = F)

training  <- trainData[trainRows, ]
testing   <- trainData[-trainRows, ]

featurePlot <- featurePlot(training[, -53], training$classe, "strip")

# Do any features have close to zero variance -- no good as predictors?
nearZeroVar(trainData[, -53])

# factor correlations
corrplot(corr = cor(trainData[, -53]), method = 'ellipse',
         type = 'lower', diag = F, outline = F, main = "Factor correlations",
         tl.pos = 'n')

modelCV <- trainControl(method = 'cv', number = 10)

modFit            <- train(classe ~ ., data = training, method = 'rf', trControl = trainControl )
predictedClasse   <- predict(modFit, testing)
testResults       <- confusionMatrix(testing$classe, predictedClasse)
estAccuracy <- testResults$overall[1]
estKappa    <- testResults$overall[2]

rawTest   <- read.csv('C:/Users/mcdanid2/Desktop/pml-testing.csv')
testData  <- cleanData(rawTest)
testData  <- testData[, 1:(ncol(testData) - 1)]  # remove problem ID -- what is it?
testPrediction <- predict(modFit, testData)



