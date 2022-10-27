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


######### merge of crsp and compustat ###############################


library(tidyverse)
library(lubridate)  # ymd function 
library(xts)        # end of month subsetting
library("sqldf")
library(corrplot) 
library(factoextra) # pca - fviz_eigen function

crsp <- read.csv(unz("crsp-monthly.zip", "crsp-monthly.csv"))
crsp <- read.csv("crsp-monthly.csv")
crsp <- crsp[ !(crsp$RET %in% c("", "B", "C")), ]
crsp$PRC <- abs(crsp$PRC)
crsp$RET <- as.numeric(crsp$RET)
crsp$RETX <- as.numeric(crsp$RETX)
 
compustat = read.csv(unz("compustat.zip", "compustat2.csv"))
compustat <- read.csv("compustat2.csv")
 
# yearmon to merge with Compustat
 to.year <- function (x) {
   x %/% 10000 - 1900
 }
 
 to.month <- function (x) {
   (x %/% 100) %% 100 - 1  
 }
 
 
 
 
 to.yearmon <- function (x) {
   to.year(x) * 12 + to.month(x)
 }
 
 names(compustat)[ names(compustat) == "LPERMNO" ] = "PERMNO"
 dups <- duplicated(compustat[,1:3])
 compustat <- compustat[!dups,]
 
 crsp$yearmon <- to.yearmon(crsp$date)
 compustat$yearmon <- to.yearmon(compustat$datadate)
 
 crsp$MARKETCAP <- crsp$PRC * crsp$SHROUT
 compustat = merge(compustat, crsp[c("PERMNO", "yearmon", "MARKETCAP")], by=c("PERMNO", "yearmon"), all.x = TRUE)
 crsp$MARKETCAP <- NULL
 
# rename
names(compustat)[ names(compustat) == "PERMNO" ] = "PERMNO2"
names(compustat)[ names(compustat) == "yearmon" ] = "yearmon2"
 
merged <- sqldf("select * from crsp, compustat where crsp.PERMNO = compustat.PERMNO2 and crsp.yearmon - compustat.yearmon2 between 4 and 6 order by PERMNO, date")

merged$yearmon <- NULL
merged$yearmon2 <- NULL
merged$PERMNO2 <- NULL

# safe merged dataframe as csv
write_csv(merged,"C:\\Users\\Tomke\\Desktop\\uni\\Master\\FIE456\\data\\merged.csv")

################## data preparation before addiontal merges ###################


# merged crsp + compustat 
data <- read_csv("merged.csv")

# subset data for useful variables 
data <- data[,c(1:8,43,54,85,89,98,101,136,166,188,250,262,272,313,328)]


data <- 
   data %>%
   rename(DATE = date) %>% 
   mutate(DATE = ymd(DATE)) %>%
   drop_na()



# oil data 
oil <- read_csv("oil.csv")

oil <- 
  oil %>% 
  mutate(DATE = ymd(DATE),
         DCOILWTICO = as.numeric(DCOILWTICO)) %>% 
           drop_na() 

# end of month subsetting
oil <- oil[endpoints(oil$DATE, on = "months"), ]





# monetary policy uncertainty index 
mon_po_unc <- read_csv("global_econ_policy_uncertainty.csv")
 
 mon_po_unc <- 
   mon_po_unc %>% 
   mutate(DATE = ymd(DATE)-1,
          GEPUCURRENT = as.numeric(GEPUCURRENT)) %>% 
   drop_na() 
 
 
 #consumer price index
 
 cpi <- read_csv("cpi.csv")
 
 cpi <- 
   cpi %>% 
   rename(growth_rate = CPALTT01USM657N) %>% 
   mutate(DATE = ymd(DATE)-1,
          growth_rate = as.numeric(growth_rate)) %>% 
   drop_na() 

# gdp --> brave-butters-kelley index 
 
 bbk <- read_csv("bbk.csv")
 
 bbk <- 
   bbk %>% 
   mutate(DATE = ymd(DATE)-1) %>% 
   drop_na()

# unemployment rate 

 unrate <- read_csv("unrate.csv")
 
 unrate <- 
   unrate %>% 
   mutate(DATE = ymd(DATE)-1) %>% 
   drop_na()
  
# 10 year goverment bond yield 
 
 govbond <- read_csv("10ygovbond.csv")
 
 govbond <- 
   govbond %>%
   rename(yield = IRLTLT01USM156N) %>% 
   mutate(DATE = ymd(DATE)-1) %>% 
   drop_na()
 
 
 # high yield corp bond 
 
 cbond <- read_csv("corp_bond_high_yield.csv")
 
 cbond <- 
   cbond %>% 
   mutate(DATE = ymd(DATE),
          cbyield = as.numeric(BAMLH0A1HYBBEY)) %>% 
   drop_na() 
 
 # end of month subsetting
 cbond <- cbond[endpoints(cbond$DATE, on = "months"), ]
 
 
 # high yield option adjusted spread 
 
 
 oaspread <- read_csv("option_adjusted_spread.csv")
 
 oaspread <- 
   oaspread %>% 
   rename(spread = BAMLH0A1HYBB) %>% 
   mutate(DATE = ymd(DATE),
          spread = as.numeric(spread)) %>% 
   drop_na() 
 
 # end of month subsetting
 oaspread <- oaspread[endpoints(oaspread$DATE, on = "months"), ]
 
 # dollar to euro currency exchange rate 
 
 usd_eur <- read_csv("usd_to_eur.csv")
 
 usd_eur <- 
   usd_eur %>% 
   rename(usdeur = DEXUSEU) %>% 
   mutate(DATE = ymd(DATE),
          usdeur = as.numeric(usdeur)) %>% 
   drop_na()
 
 usd_eur <- usd_eur[endpoints(usd_eur$DATE ,on = "months"),]
 
 # yen to dollar currency exchange rate 
 
 yen_usd <- read_csv("usd_to_yen.csv")
 
 yen_usd <- 
   yen_usd %>% 
   rename(yenusd = DEXJPUS) %>% 
   mutate(DATE = ymd(DATE),
          yenusd = as.numeric(yenusd)) %>% 
   drop_na()
 
 yen_usd<- yen_usd[endpoints(yen_usd$DATE ,on = "months"),]
 
 # VIX 
 vix <- read_csv("vix.csv")
 
 vix <- 
   vix %>% 
   rename(vix = VIXCLS) %>% 
   mutate(DATE = ymd(DATE),
          vix = as.numeric(vix)) %>% 
   drop_na()
 
 vix<- vix[endpoints(vix$DATE ,on = "months"),]

# financial stress index 
 
 fedrates <- read_csv("fedrates.csv")
 
 fedrates <- 
   fedrates %>% 
   mutate(DATE = ymd(DATE)-1,
          fs_index = as.numeric(FEDFUNDS)) %>% 
   drop_na()
 
# ted rate
 
 ted <- read_csv("tedrate.csv")
 
 ted <- 
   ted %>% 
   mutate(DATE = ymd(DATE),
          TEDRATE = as.numeric(TEDRATE)) %>% 
   drop_na()
 
 ted<- ted[endpoints(ted$DATE ,on = "months"),]
 
 
 
############### final merge ########################################

data <- merge(data,oil,by="DATE")
data <- merge(data,mon_po_unc,by="DATE")
data <- merge(data,cpi,by="DATE")
data <- merge(data,bbk,by="DATE")
data <- merge(data,unrate,by="DATE")
data <- merge(data,govbond,by="DATE")
data <- merge(data,cbond,by="DATE") 
data <- merge(data,oaspread,by="DATE")
data <- merge(data,usd_eur,by="DATE")
data <- merge(data,yen_usd,by="DATE")
data <- merge(data,vix,by="DATE")
data <- merge(data,fedrates,by="DATE")
data <- merge(data,ted,by="DATE")



write_csv(data,"C:\\Users\\Tomke\\Desktop\\uni\\Master\\FIE456\\data\\data.csv")

################ create lags #########################################

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


############## create dependent variables #####################

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


# which variables to use for "training" 
train1 <- training[,c(5:31,33)] 
train2 <- training[,c(5:31,34)]




############### correlation plot ###########################

df <- data[,4:31]

cor_ma <- cor(df)

corrplot(cor_ma,tl.cex=0.6,tl.offset = 1)

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
fviz_pca_var(pca,title="", geom = c("point", "text"),repel=T)











