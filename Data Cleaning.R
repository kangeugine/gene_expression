# Splunk Homework
setwd("C:/Users/Golden/Desktop/Splunk/gene_expression_data")
genex <- read.table("./genex.txt", header=FALSE, sep=" ")
train <- read.table("./training.txt",header=TRUE, sep=" ")
label <- read.table("./labels.txt", header=FALSE, sep=" ")
#wtf <- read.table("./test.txt",header=TRUE, sep=" ")

# Convert label to 1 and 0
train$y <- ifelse(label == 1, 1,0)

# Lets remove variables that do not have any variance
# Use nearZerovariance in caret
require(caret)
var0 <- nearZeroVar(train, saveMetrics=TRUE)
str(var0)
head(var0[var0[,"zeroVar"] > 0, ],10) 
head(var0[var0[,"zeroVar"] + var0[,"nzv"] > 0, ],20) 
var0 <- nearZeroVar(train)
train2 <- train[,-var0]

# What should we do about missing values?
noNA <- sapply(train2, function(x) sum(is.na(x)))
colNA <- which(noNA > 0)
for(i in 1:length(colNA)){
        print(which(is.na(train2[,colNA[i]])))
}
# NAs are spread out so lets try kNN imputation to fill in the values
#install.packages("DMwR")
require(DMwR)
dataImpute <- knnImputation(train2, k=10)

# Let's remove features with more than 0.70 of correlation
#install.packages("corrplot")
require(corrplot)
data.scale <- scale(dataImpute, center=TRUE, scale=TRUE)
# Compute correlation matrix
corMat <- cor(data.scale)

# Set correlation threshold at 0.70
install.packages("caret")
require(caret)
highlyCor <- findCorrelation(corMat, 0.70)
data.filter <- as.data.frame(data.scale[,-highlyCor])
data.filter$Y <- ifelse(label == 1, 1,0)
data.filter$Y <- as.factor(data.filter$Y)