# Expression levels of genes involved in a genetically inherited disease of a population sample
# 1.  Build a model to predict whether a new person will have the disease
# 2.  Given a test dataset, predict the disease state of each individual (test.txt)
# 3.  Evaluate the accuracy of your model 

#####
# Compare Models by ROC value
#####

# Extract data.frame by Importance threshold
featSelect <- function(thres){
        return(fullVarImp[fullVarImp$Imp>thres,"varName"])
        }
featSet <- featSelect(1.1)
# Independent Variables for Model
X <- dataFilter[,featSet]
train <- 1:nrow(X)
# Dependent Variables for Model
Y <- dataFilter$Y
levels(Y)[1] <- make.names(levels(Y))[1]
levels(Y)[2] <- make.names(levels(Y))[2]


# Train Models
# Prepare training scheme
folds=10
repeats=1
# ROC will be the metric to evaluate the model "summaryFunction=twoClassSummary"
myControl <- trainControl(method='cv', number=folds, repeats=repeats, 
                          returnResamp='none', classProbs=TRUE,
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          summaryFunction=twoClassSummary,
                          index=createMultiFolds(try$Y, k=folds, times=repeats))
PP <- c('center', 'scale')

# For reproducable results
set.seed(1)

# Stochastic Gradient Boosting
mdlGBM <- train(X[train,], Y[train], 
                method='gbm', 
                trControl=myControl, 
                tuneLength=10, 
                preProcess=PP)
# Boosted Tree
mdlBLACKBOOST <- train(X[train,], Y[train], 
                       method='blackboost', 
                       trControl=myControl, 
                       tuneLength=10, 
                       preProcess=PP)
# Random Forest
mdlPARRF <- train(X[train,], Y[train], 
                  method='parRF', 
                  trControl=myControl, 
                  tuneLength=10, 
                  preProcess=PP)
# Multi-Layer Perceptron
mdlMLP <- train(X[train,], Y[train], 
                method='mlpWeightDecay', 
                trControl=myControl, 
                trace=FALSE,
                tuneLength=3,
                preProcess=PP)
# k-Nearest Neighbors
mdlKNN <- train(X[train,], Y[train], 
                method='knn', 
                trControl=myControl,
                tuneLength=10,
                preProcess=PP)
# Multivariate Adaptive Regression Spline
mdlEARTH <- train(X[train,], Y[train], 
                  method='earth', 
                  trControl=myControl, 
                  tuneLength=10,
                  preProcess=PP)
# Generalized Linear Model
mdlGLM <- train(X[train,], Y[train], 
                method='glm', 
                trControl=myControl, 
                preProcess=PP)
# Support Vector Machines with Radial Basis Function Kernel
mdlSVM <- train(X[train,], Y[train], 
                method='svmRadial', 
                trControl=myControl, 
                tuneLength=10,
                preProcess=PP)
# Lasso and Elastic-Net Regularized Generalized Linear Models
mdlGLMNET <- train(X[train,], Y[train], 
                   method='glmnet', 
                   trControl=myControl, 
                   tuneLength=10,
                   preProcess=PP)

# Organize results from each model
models <- c("Stochastic Gradient Boosintg", "Boosted Tree", "Random Forest", 
            "Multi-Layer Perceptron", "k-Nearest Neighbots", "EARTH", 
            "Generalized Linear Model", "SVM Radial", "GLM Net")
modelList <- list(mdlGBM, mdlBLACKBOOST, mdlPARRF,
                  mdlMLP, mdlKNN, mdlEARTH,
                  mdlGLM, mdlSVM, mdlGLMNET)
modelResult <- data.frame()
for(i in 1:length(models)){
        resultDF <- modelList[[i]]$result[,c("ROC","Sens","Spec")]
        resultDF$Model <- models[i]
        modelResult <- rbind(modelResult,resultDF)
}
require(data.table)
modelResultDT <- data.table(modelResult)
meanROC <- as.data.frame(modelResultDT[,list(Mean=mean(ROC),Max=max(ROC)),by=Model])
meanROC <- meanROC[order(meanROC$Mean),]
modelResult$Model <- factor(modelResult$Model,
                       levels = meanROC$Model,ordered = TRUE)

ggplot(modelResult, aes(x=Model, y=ROC)) + geom_boxplot(fill="orange") +
        coord_flip() +
        ggtitle("Compare Models with ROC") +
        labs(x="Models",y="ROC") + 
        theme(plot.title = element_text(color="#666666", face="bold", size=25, hjust=0)) +
        theme(axis.title = element_text(color="#666666", face="bold", size=22)) +
        theme(axis.text = element_text(color="#666666", face="bold", size=13))

# We can see that Random Forest shows the best result for our Classification Data