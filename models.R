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

# create returns for 
# compare compustat ratios to industry ratios ?? 
# is accounting data unusual for your industry


##### do we have book/market ratio ?? 

## create lag variables and 

data <- 
   data %>% 
   rename(SP500 = Price,
          oil = DCOILWTICO) %>% 
   arrange(PERMNO,DATE) %>% 
   mutate(SP500return = SP500/lag(SP500)-1) %>% 
   mutate(marketdiff = RET-SP500return) %>% 
   mutate(oilreturn = oil/lag(oil)-1) %>% 
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
          oilprice  = lag(oil, n=1),
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
          ted     = lag(TEDRATE, n=1),
          SP500   = lag(SP500, n=1),
          SP500return = lag(SP500return,n=1),
          oilreturn = lag(oilreturn,n=1)) %>% 
   select(-c(2,3,6:39)) %>% 
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

# full plot 
corrplot(cor_ma,tl.cex=0.6,tl.offset = 0.5)

# function for corr plot of only high correlated variables 
corr_simple <- function(data=df,sig=0.5){
   #convert data to numeric in order to run correlations
   #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
   df_cor <- data %>% mutate_if(is.character, as.factor)
   df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
   #run a correlation and drop the insignificant ones
   corr <- cor(df_cor)
   #prepare to drop duplicates and correlations of 1     
   corr[lower.tri(corr,diag=TRUE)] <- NA 
   #drop perfect correlations
   corr[corr == 1] <- NA 
   #turn into a 3-column table
   corr <- as.data.frame(as.table(corr))
   #remove the NA values from above 
   corr <- na.omit(corr) 
   #select significant values  
   corr <- subset(corr, abs(Freq) > sig) 
   #sort by highest correlation
   corr <- corr[order(-abs(corr$Freq)),] 
   #print table
   print(corr)
   #turn corr back into matrix in order to plot with corrplot
   mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
   
   #plot correlations visually
   corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}


# plot with variables of high correlation 
corr_simple(df)


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


# pcacomp for number of pcs considered, "cv" for cross validtion 
trctrl = trainControl(method = "cv",
                      number=5,
                      preProcOptions = list(pcaComp=5),
                      verboseIter = TRUE) 


# knn algorithm 
grid <- expand.grid(kmax = c(5,9,13,17,21),            # allows to test a range of k values
                    distance = 2,        # allows to test a range of minkowski distances
                    kernel = 'rectangular')   # different weighting types in kkn (not used in this thesis)




# knn fit for returns:
knnfit1 = train(y1~., data = train1,
                      method = "kknn",
                      trControl=trctrl,
                      preProcess=c("BoxCox","center","scale","pca"),
                      #tuneLength=9)
                      tuneGrid=grid)



# knn fit for volatility
knnfit2 = train(y2~., data = train2,
                method = "kknn",
                trControl=trctrl,
                preProcess=c("BoxCox","center","scale","pca"),
                #tuneLength=9)
                tuneGrid=grid)



nnet_grid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5), 
                         .size = c(3, 5, 10, 20))

# neural network for returns 
nnfit1 <- train(y1 ~ ., 
                     data = train1, 
                     method = "nnet", 
                     preProcess=c("BoxCox","center","scale","pca"),
                     trControl = trctrl,
                     na.action = na.omit,
                     tuneGrid = nnet_grid,
                     trace = FALSE)

nnfit2 <- train(y2 ~ ., 
                data = train2, 
                method = "nnet", 
                trControl = trctrl,
                preProcess=c("BoxCox","center","scale","pca"),
                na.action = na.omit,
                tuneGrid = nnet_grid,
                trace = FALSE)





#rfgrid <- expand.grid(.mtry=c(1:15))

# random forest fit for return
set.seed(42)
rffit1 <- train(y1 ~ ., 
                 data = train1, 
                 method = "rf", 
                 trControl = trctrl,
                 preProcess=c("BoxCox","center","scale","pca"),
                 na.action = na.omit,
                 #tuneGrid=rfgrid,
                 tuneLength=4,
                 trace = FALSE)

plot(rffit1$finalModel)
plot(rffit1)

 
 # random forest fit for volatility
set.seed(42)
rffit2 <- train(y2 ~ ., 
                 data = train2, 
                 method = "rf", 
                 trControl = trctrl,
                 preProcess=c("BoxCox","center","scale","pca"),
                 na.action = na.omit,
                #tuneGrid = rfgrid,
                 tuneLength=4,
                 trace = FALSE)

plot(rffit2$finalModel)
plot(rffit2)
 
 
# gradient boosting machines 

# Max shrinkage for gbm
nl = nrow(train1)
max(0.01, 0.1*min(1, nl/10000))

# Max Value for interaction.depth
floor(sqrt(ncol(train1)))
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5),
                        n.trees = c(50,100,300,500,1000,2000), 
                        shrinkage = c(0.005,0.025,0.05,0.075,0.1),
                        n.minobsinnode = 10)





# # extreme boosting machines better ? 
# xbgfit1 <- train(y1 ~ .,
#                  data = train1,
#                  method = "xgbTree",
#                                    trControl = trctrl,
#                                    preProc = c("center", "scale","pca"))
# 
# xbgfit2 <- train(y2 ~ .,
#                  data = train2,
#                  method = "xgbTree",
#                  trControl = trctrl,
#                  preProc = c("center", "scale","pca"))

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
                 preProcess=c("BoxCox","center","scale","pca"),
                 tuneGrid = gbmGrid)




set.seed(42)
gbmfit2 <- train(y2 ~ ., data = train2, 
                 method = "gbm", 
                 trControl = trctrl,
                 preProcess=c("BoxCox","center","scale","pca"),
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





