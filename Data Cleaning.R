# Expression levels of genes involved in a genetically inherited disease of a population sample
# 1.  Build a model to predict whether a new person will have the disease
# 2.  Given a test dataset, predict the disease state of each individual (test.txt)
# 3.  Evaluate the accuracy of your model 

#####
# Exploratory Analysis and Data Cleaning
#####

# Import Data
setwd("C:/Users/Golden/Desktop/Splunk/gene_expression_data")
genex <- read.table("./genex.txt", header=FALSE, sep=" ")
data <- read.table("./training.txt",header=TRUE, sep=" ")
label <- read.table("./labels.txt", header=FALSE, sep=" ")

# What is the dimension of data?
dim(data)

# High-dimensional data
# Let's remove variables with no variance or near zero variance
require(caret)
var0 <- nearZeroVar(data)
length(var0)
# 3245 features will be removed from data
dataZero <- data[,-var0]

# Are there any features with missing values?
noNA <- sapply(dataZero, function(x) sum(is.na(x)))
table(noNA)
# 6 Columns have missing values
# Not many but lets try to fill out the missing values with imputation

# Fill in NA values using the k Nearest Neighbours
require(DMwR)
dataImpute <- knnImputation(dataZero, k=10)

# Let's try to remove the effects of Multicollinearity
# Remove features with more than 0.70 of correlation
require(corrplot)
dataScale <- scale(dataImpute, center=TRUE, scale=TRUE)
corMat <- cor(dataScale)
require(caret)
highlyCor <- findCorrelation(corMat, 0.70)
dataFilter <- as.data.frame(dataScale[,-highlyCor])
dataFilter$Y <- ifelse(label == 1, 1,0)
dataFilter$Y <- as.factor(dataFilter$Y)