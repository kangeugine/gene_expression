# Expression levels of genes involved in a genetically inherited disease of a population sample
# 1.  Build a model to predict whether a new person will have the disease
# 2.  Given a test dataset, predict the disease state of each individual (test.txt)
# 3.  Evaluate the accuracy of your model 

#####
# Feature Selection with RandomForest
#####

# We still suffer from the curse of high dimensionality p >>> n
# For us to use a wider selection of models, lets narrow down our features
# We will use RandomForest to compute the importance of feature for classification
require(mlbench)