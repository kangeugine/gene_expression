# Expression levels of genes involved in a genetically inherited disease of a population sample
# 1.  Build a model to predict whether a new person will have the disease
# 2.  Given a test dataset, predict the disease state of each individual (test.txt)
# 3.  Evaluate the accuracy of your model 

#####
# Optimize Selected Model
#####

# Adjust features selected and evaluste with Random Forest
testPerformance <- function(thres){
        # Prepare data.frame for model
        featSet <- featSelect(thres)
        X <- dataFilter[,featSet]
        # Run model
        model <- train(X[train,], Y[train], 
                          method='parRF', 
                          trControl=myControl, 
                          tuneLength=10, 
                          preProcess=PP)
        # Return result
        return(model$result)
}

thresLvl <- seq(1,1.5,0.05)
set.seed(1)
testRange <- lapply(thresLvl, function(x) testPerformance(x))
testResult <- data.frame()
for(i in 1:length(thresLvl)){
        df <- testRange[[i]]
        df$Thres <- paste("Thres",thresLvl[i])
        testResult <- rbind(testResult,df)
}
ggplot(testResult, aes(x=Thres, y=ROC)) + geom_boxplot(fill="orange") +
        coord_flip() +
        ggtitle("Optimal Feature Selection") +
        labs(x="Threshold", y="ROC") + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0)) +
        theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) +
        theme(axis.text = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=13))

# When threshold is set to 1.3, we get the best performance for Random Forest
# How many features are selected for 1.3?
length(featSelect(1.3))
# 62 features are selected for best performance

# What is the best parameter?
testRange[[7]]
# mtry = 55, but lets go deeper

# Let's see how the performace of mtry 48:60 comes out
repeats = 3
set.seed(1)
X <- dataFilter[,featSelect(1.3)]
optModel <- train(X[train,], Y[train], 
               method='parRF', 
               trControl=myControl, 
               tuneGrid=expand.grid(.mtry=48:60),
               preProcess=PP)
optModel$result
# We can say mtry 50 or 56 performs the best.
# our Final model is a Random Forest with mtry = 56
set.seed(1)
finalModel <- train(X[train,], Y[train], 
                    method='parRF', 
                    trControl=myControl, 
                    tuneGrid=expand.grid(.mtry=56),
                    preProcess=PP)
