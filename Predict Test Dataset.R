# Expression levels of genes involved in a genetically inherited disease of a population sample
# 1.  Build a model to predict whether a new person will have the disease
# 2.  Given a test dataset, predict the disease state of each individual (test.txt)
# 3.  Evaluate the accuracy of your model 

#####
# Predict disease state for test dataset
#####

# Import test dataset
test <- read.csv("./test.csv",header=TRUE, sep=",")
testScale <- as.data.frame(scale(test[,featSelect(1.3)], center = TRUE, scale = TRUE))

# Predict test dataset with final model
testPred <- predict(finalModel, newdata=testScale)