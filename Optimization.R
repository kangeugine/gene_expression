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

set.seed(1)
testRange <- lapply(seq(1,1.5,0.05), function(x) testPerformance(x))
