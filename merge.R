# Merging data together 

rm(list=ls())




######### merge of crsp and compustat ###############################

# libraries
library(tidyverse)
library(lubridate)  # ymd function 
library(xts)        # end of month subsetting
library("sqldf")


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



