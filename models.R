########################################################
########## BiG Data FIE456 final project ###############
########################################################

# we will try to use machine learning algorithms 
# to predict whether stock returns of companies 
# are going to be positive or negative 
# additionally whether the volatility is high 
# squared return is basically a measure (the simplest measure) of volatility

# this supposedly gives useful information if we need money in near future 


rm(list=ls())

#libraries
library(tidyverse)
library(lubridate)  # ymd function 
library(corrplot) 
library(factoextra) # pca - fviz_eigen function
library(caret)      # machine learning algos
library(kknn)
library(nnet)
library(randomForest)
library(gbm)
library(kernlab)


data <- read_csv("data.csv")


################ Data Manipulations #########################################

## create lag variables 

data <- 
   data %>% 
   arrange(PERMNO,DATE) %>% 
   mutate(return1 = lag(RET,n=1),
          return2 = lag(RET,n=2),
          return3 = lag(RET,n=3),
          shrout = lag(SHROUT,n=1),
          vol    = lag(VOL,n=1),
          assets     = lag(atq,n=1),
          cash     = lag(chq, n=1),
          debt   = lag(dlttq, n=1),
          dep_amor     = lag(dpq,n=1),
          dividends    = lag(dvpq,n=1),
          eps  = lag(epsfiq, n=1),
          goodwill   = lag(gdwlq, n=1),
          investments   = lag(ivltq, n=1),
          income     = lag(niq, n=1),
          lag_prcraq  = lag(prcraq, n=1),
          rd   = lag(rdipq, n=1),
          revenue   = lag(revtq, n=1),
          equity    = lag(teqq, n=1),
          taxes    = lag(txtq, n=1),
          oilprice  = lag(DCOILWTICO, n=1),
          policy    = lag(GEPUCURRENT, n=1),
          growth  = lag(growth_rate, n=1),
          bbkm    = lag(BBKMGDP, n=1),
          unrate  = lag(UNRATE, n=1),
          yield   = lag(yield, n=1),
          oaspread    = as.numeric(lag(BAMLH0A1HYBBEY, n=1)),
          cbyield = lag(cbyield, n=1),
          spread  = lag(spread, n=1),
          usdeur  = lag(usdeur, n=1),
          yenusd  = lag(yenusd, n=1),
          vix     = lag(vix, n=1),
          fed     = lag(FEDFUNDS, n=1),
          ted     = lag(TEDRATE, n=1)) %>% 
   select(-c(3,6:37)) %>% 
             drop_na()


# why not running statistical significance tests 
# to choose the variables ? 
# First of all, "adding or removing variables based on their significance" 
# is not a good practice!
# As the name suggests, significance testing is about testing a hypothesis,
# it is not a tool for optimizing anything, and 
# by using it for the variable selection you assume some kind of optimization problem.


##create dependent variables 

data <- 
   data %>% 
   mutate(RET_squared = as.numeric(RET^2),
          y1 = as.factor(ifelse(RET>0,1,0)),
          y2 = as.factor(ifelse(RET_squared >0.025,1,0)))  
             

# check for na in data
anyNA(data)
          
          
          
################## split into training set and test set #######################

# splitting by time makes more sense, 
# because training all algorithm on future data and 
# for classifying something in the past does not make sense

training <- data[data$DATE<=as.Date("2017-12-31"),] # 70 %   
testing <-  data[data$DATE>as.Date("2017-12-31"),]  # 30 %   
          
# Checking distribution in original data and partitioned data
prop.table(table(training$y1)) * 100
prop.table(table(testing$y1)) * 100
prop.table(table(data$y1)) * 100

prop.table(table(training$y2)) * 100
prop.table(table(testing$y2)) * 100
prop.table(table(data$y2)) * 100


# which variables to use for "training" and "testing" 
train1 <- training[,c(5:31,33)] 
train2 <- training[,c(5:31,34)]



############### correlation plot ###########################

df <- data[,4:31]

cor_ma <- cor(df)

corrplot(cor_ma,tl.cex=0.6,tl.offset = 0.5)

# based on this plot we excluded variables that were extremely highly correlated
# for obvious reasons and probably dont give useful extra information 

# There are three main reasons why you would remove correlated features:
# Make the learning algorithm faster
# Decrease harmful bias
# interpretability of your model

##################### pca plots ##########################################


# there are several reasons why you want to use PCA:
# 1. Removes correlated features. 
# 2. Improves machine learning algorithm performance. 
# 3. Reduce overfitting
# 4. maybe smallers run-time 


# change factor variable to numeric
df2 <- train1

df2$y1=as.numeric(df2$y1)
class(df2$y1)

pca = prcomp(df2,scale = T)

fviz_eig(pca,title="",addlabels=T)
fviz_pca_var(pca,title="", geom = c("point","text"),repel=T)
fviz_pca_var(pca,title="", geom = c("point"),repel=T)


######################## models ##########################################


# knn algorithm 

#grid <- expand.grid(kmax = c(9,13),            # allows to test a range of k values
#                    distance = 2,        # allows to test a range of minkowski distances
#                    kernel = 'rectangular')   # different weighting types in kkn (not used in this thesis)


set.seed(42)
# pcacomp for number of pcs considered, "cv" for cross validtion 
trctrl = trainControl(method = "cv",
                      number=3,
                      preProcOptions = list(pcaComp=5),
                      verboseIter = TRUE) 

# knn fit for returns:
knnfit1 = train(y1~., data = train1,
                      method = "knn",
                      trControl=trctrl,
                      preProcess=c("BoxCox","center","scale","pca"),
                      tuneLength=9)
                      #tuneGrid=grid)



# knn fit for volatility
knnfit2 = train(y2~., data = train2,
                method = "knn",
                trControl=trctrl,
                preProcess=c("BoxCox","center","scale","pca"),
                tuneLength=9)
                #tuneGrid=grid)


# neural network for returns 
nnfit1 <- train(y1 ~ ., 
                     data = train1, 
                     method = "nnet", 
                     preProcess=c("BoxCox","center","scale","pca"),
                     trControl = trctrl,
                     na.action = na.omit,
                     trace = FALSE)

nnfit2 <- train(y2 ~ ., 
                data = train2, 
                method = "nnet", 
                trControl = trctrl,
                na.action = na.omit,
                trace = FALSE)



# We can also plot a ROC curve, in which the True Positive rate (sensitivity)
# is plotted against the True Negative rate(specificity).
# This is good for evaluating whether your model is both correctly predicting
# which are and are not positive sentiment (not just one or the other).

# library(pROC) 
# 
# #Draw the ROC curve 
# nn.probs <- predict(m.NeuralNet,test_data,type="prob")
# nn.ROC <- roc(predictor=nn.probs$`1`,
#               response=as.numeric(test_data$Sentiment)-1,
#               levels=rev(levels(test_data$Sentiment)))
# nn.ROC$auc



# random forest fit for return
set.seed(42)
rffit1 <- train(y1 ~ ., 
                 data = train1, 
                 method = "rf", 
                 trControl = trctrl,
                 na.action = na.omit,
                 trace = FALSE)

plot(rffit1$finalModel)
plot(rffit1)

 
 # random forest fit for volatility
set.seed(42)
rffit2 <- train(y2 ~ ., 
                 data = train2, 
                 method = "rf", 
                 trControl = trctrl,
                 na.action = na.omit,
                 trace = FALSE)

plot(rffit2$finalModel)
plot(rffit2)
 
 
# gradient boosting machines 

gbmGrid <-  expand.grid(interaction.depth = c(1, 9), 
                        n.trees = 500, 
                        shrinkage = 0.05,
                        n.minobsinnode = 10)


# extreme boosting machines better ? 
xbgfit1 <- train(y1 ~ .,
                 data = train1,
                 method = "xgbTree",
                                   trControl = trctrl,
                                   preProc = c("center", "scale","pca"))

xbgfit2 <- train(y2 ~ .,
                 data = train2,
                 method = "xgbTree",
                 trControl = trctrl,
                 preProc = c("center", "scale","pca"))

# what is n.minobsinnode for ?
# At each step of the GBM algorithm, a new decision tree is constructed.
# The question when growing a decision tree is 'when to stop?'.
# The furthest you can go is to split each node
# until there is only 1 observation in each terminal node -> n.minnobsinnode = 1 
# The default for the R GBM package is 10.
# Generally, results are not very sensitive to this parameter and given the stochastic nature of GBM performance it might actually be difficult to determine exactly what value is 'the best'. The interaction depth, shrinkage and number of trees will all be much more significant in general.
# This is why we gonna take n = 10 


# Package GBM uses interaction.depth parameter as a number of splits
#it has to perform on a tree


set.seed(42)
gbmfit1 <- train(y1 ~ ., data = train1, 
                 method = "gbm", 
                 trControl = trctrl,
                 tuneGrid = gbmGrid)

pretty.gbm.tree(gbmfit1)



set.seed(42)
gbmfit2 <- train(y2 ~ ., data = train2, 
                 method = "gbm", 
                 trControl = trctrl,
                 tuneGrid = gbmGrid)






##################### Predictions on test set #########################

knnpredict1 <- predict(knnfit1,newdata=testing)
confusionMatrix(knnpredict1, testing$y1)

knnpredict2 <- predict(knnfit2,newdata=testing)
confusionMatrix(knnpredict2, testing$y2)

nnpredict1 <- predict(nnfit1,newdata=testing)
confusionMatrix(nnpredict1, testing$y1)

nnpredict2 <- predict(nnfit2,newdata=testing)
confusionMatrix(nnpredict2, testing$y2)

rfpredict1 <- predict(rffit1,newdata=testing)
confusionMatrix(rfpredict1, testing$y1)

rfpredict2 <- predict(rffit2,newdata=testing)
confusionMatrix(rfpredict2, testing$y2)

gbmpredict1 <- predict(rffit1,newdata=testing)
confusionMatrix(gbmpredict1, testing$y1)

gbmpredict2 <- predict(gbmfit2,newdata=testing)
confusionMatrix(gbmpredict2, testing$y2)











