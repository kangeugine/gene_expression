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

# Prepare training scheme
require(mlbench)
control <- trainControl(method="repeatedcv", number=10, repeats=1)

# For reproducable results
set.seed(1)
# RandomForest to determine Feature importance
rfModel <- train(Y~., data=datFilter, 
                   method="rf", 
                   trControl=control,
                   importance=TRUE,
                   do.trace=FALSE
)
# Organize Feature Importance Data
fullImportance <- varImp(rfModel, scale=FALSE)
fullVarImp <- fullImportance$importance
fullVarImp$varName <- row.names(fullVarImp)
colnames(fullVarImp) <- c("Imp","Imp2","varName")
fullVarImp <- fullVarImp[order(fullVarImp$Imp, decreasing=T),]

# How many features are selected by Importance?
hist(fullVarImp$Imp)
lvl <- seq(1,2,0.05)
noFeatvsImp <- data.frame(Imp=lvl, 
                          noFeat=sapply(lvl, function(x) sum(fullVarImp$Imp>x))
                          )
require(ggplot2)
ggplot(data=noFeatvsImp, aes(x=Imp,y=noFeat)) + 
        geom_line(aes(color="blue", size=3)) +
        ggtitle("No.Features by Importance Threshold") +
        labs(x="Importance",y="Number of Features") + 
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=25, hjust=0)) +
        theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) +
        guides(colour=FALSE, size=FALSE)

# We can select features by importance and compare models