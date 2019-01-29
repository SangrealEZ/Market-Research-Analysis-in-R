rm(list = ls())
setwd("/00Courses/03 Research/Wegmans/Data")

#####################################################
# Import data
#####################################################
# before: 45 weeks; after:10 weeks
# import survey data
wgm <- read.csv('Survey Data FINAL.csv',header = T)
# remove duplicated records in survey data
wgm <- wgm[!duplicated(wgm),]
# import customer data
custdat <- read.table('custdata.txt',header = T,sep = '|',row.names = NULL)
# import insider data
insidersdat <- read.table('txdata_insiders.txt',header = T,sep = '|',row.names = NULL)
# import other top 30 data
othertop30dat <- read.table('txdata_othertop30.txt',header = T,sep = '|',row.names = NULL)
# take a summary
summary(insiderAlt)
summary(othertop30dat)

# check unique HH
insiderUni <- unique(insidersdat$HH) # 8,850 diff customers
otherUni <- unique(othertop30dat$HH) # 10,000 diff customers

# create columns for sales per unit
insidersdat$sales_post <- insidersdat$UNITS_POST*insidersdat$PRICE_PER_UNIT_MASKED
insidersdat$sales <- insidersdat$UNITS * insidersdat$PRICE_PER_UNIT_MASKED
othertop30dat$sales_post <- othertop30dat$UNITS_POST * othertop30dat$PRICE_PER_UNIT_MASKED
othertop30dat$sales <- othertop30dat$UNITS * othertop30dat$PRICE_PER_UNIT_MASKED

# merge otherTop30 with customer
othJoinCust <- merge(othertop30dat, custOther, by = 'HH', all = T)
othJoinCust$SALES <- othJoinCust$UNITS * othertop30dat$PRICE_PER_UNIT_MASKED
othJoinCust$SALES_POST <- othJoinCust$UNITS_POST * othertop30dat$PRICE_PER_UNIT_MASKED

custPanel <- custdat[custdat$HH %in% insiderUni,] # panel customers
custOther <- custdat[custdat$HH %in% otherUni,] # other customers

# take transaction records of X-related only
insX <- insidersdat[insidersdat$ALTERNATIVE != 'N',] # insider's transaction
# join transaction records of X-related with survey data
surJoinTransX <- merge(wgm,insX,by = 'HH',all = T) 
length(unique(surJoinTrans$HH)) # 9690 unique HH

# any difference between UNITS & UNITS_POST?
t.test(surJoinTransX$UNITS,surJoinTransX$UNITS_POST) # p-value < 2.2e-16

# join transaction records with customer data(X-related)
transXJoinCustI <- merge(insX, custPanel, by = 'HH', all = T)


length(unique(insJoinT0[insJoinT0$EndPoint=='E5'|insJoinT0$EndPoint=='E6',])) #21
length(unique(surJoinTrans$HH[surJoinTrans$EndPoint=='E5'|surJoinTrans$EndPoint=='E6'])) #2511
21/2511 

#####################################################
# What's the size of prize----aggregate sales
#####################################################
# aggregate by EndPoint(takes means for sales)(T0)
surJoinTransT0 <- surJoinTransX[surJoinTransX$ALTERNATIVE == 'T0',]
endpointDistr <- aggregate(WEEKS~EndPoint, data = surJoinTransT0, FUN = sum)
endpointDistr$UNITS <- aggregate(UNITS~EndPoint, data = surJoinTransT0, FUN = sum)[,2]
endpointDistr$UNITS_POST <- aggregate(UNITS_POST~EndPoint, data = surJoinTransT0, FUN = sum)[,2]
endpointDistr$PRICE <- aggregate(PRICE_PER_UNIT_MASKED~EndPoint, data = surJoinTransT0, FUN = mean)[,2]
endpointDistr$SALES <- aggregate(sales~EndPoint, data = surJoinTransT0, FUN = sum)[,2]
endpointDistr$SALES_POST <- aggregate(sales_post~EndPoint, data = surJoinTransT0, FUN = sum)[,2]
endpointDistr
sum(endpointDistr$UNITS)/45 # 264.5778
sum(endpointDistr$UNITS_POST)/10 # 359.6



#####################################################
# Take a look at transactions of alternatives
#####################################################
length(unique(insidersdat$ITEM_MASKED[insidersdat$ALTERNATIVE == 'T0']))
length(unique(othertop30dat$ITEM_MASKED[othertop30dat$ALTERNATIVE == 'T0']))
# there are 45 different ITEM_X bought by panel customers
# there are 42 different ITEM_X bought by panel customers

length(unique(insidersdat$ITEM_MASKED[insidersdat$ALTERNATIVE == 'T1']))
length(unique(othertop30dat$ITEM_MASKED[othertop30dat$ALTERNATIVE == 'T1']))
# there are 441 different T1 bought by panel customers
# there are 412 different T1 bought by panel customers

length(unique(insidersdat$ITEM_MASKED[insidersdat$ALTERNATIVE == 'T2']))
length(unique(othertop30dat$ITEM_MASKED[othertop30dat$ALTERNATIVE == 'T2']))
# there are 684 different T2 bought by panel customers
# there are 637 different T2 bought by panel customers

prop.table(table(insidersdat$ALTERNATIVE))
# N          T0          T1          T2 
# 0.970950567 0.001821038 0.006062895 0.021165500

# check percentage of each type of alternative
prop.table(table(insX$ALTERNATIVE)) 
# N          T0         T1         T2 
# 0.00000000 0.06268756 0.20870959 0.72860285 
# check if althernatives have attribute M
prop.table(table(insX$ALTERNATIVE[insX$ATTR_M == 'Y']))
# N         T0         T1         T2 
# 0.00000000 0.98981561 0.01018439 0.00000000
# T2 does not have attribute M, very few T1 has attribute M and is neglectible

othX <- othertop30dat[othertop30dat$ALTERNATIVE != 'N',] # others' transaction
prop.table(table(othX$ALTERNATIVE)) # check percentage of each type of alternative
#          N         T0         T1         T2 
# 0.00000000 0.03352158 0.17282775 0.79365067
prop.table(table(othX$ALTERNATIVE[othX$ATTR_M == 'Y']))

# 1) We can regard alternative products as not having attribute M
# 2) sales of ITEM_X only account for a small portion
# 3) sales/week seems to decline for each alternative, but may due to seasonal reasons




#####################################################
# What's the size of prize----aggregate sales
#####################################################
# what's sales for each alternative
# aggregate by Alternatives(takes sum for sales)
alternativeDistr <- aggregate(WEEKS~ALTERNATIVE, data = insidersdat, FUN = sum)
alternativeDistr$UNITS <- aggregate(UNITS~ALTERNATIVE, data = insidersdat, FUN = sum)[,2]
alternativeDistr$UNITS_POST <- aggregate(UNITS_POST~ALTERNATIVE, data = insidersdat, FUN = sum)[,2]
alternativeDistr$ITEM_NBR <- aggregate(ITEM_MASKED~ALTERNATIVE, data = insidersdat, FUN = length)[,2]
alternativeDistr$PRICE <- aggregate(PRICE_PER_UNIT_MASKED~ALTERNATIVE, data = insidersdat, FUN = mean)[,2]
alternativeDistr$SALES <- aggregate(sales~ALTERNATIVE, data = insidersdat, FUN = sum)[,2]
alternativeDistr$SALES_POST <- aggregate(sales_post~ALTERNATIVE, data = insidersdat, FUN = sum)[,2]
alternativeDistr$SALESWK <- alternativeDistr$SALES/alternativeDistr$WEEKS
alternativeDistr$SALES_POSTWK <- alternativeDistr$SALES_POST/alternativeDistr$WEEKS
alternativeDistr

# aggregate by HH for all(X-related)
# create columns for sales(takes means for sales)
surJoinTrans$sales_post <- surJoinTrans$UNITS_POST*surJoinTrans$PRICE_PER_UNIT_MASKED
surJoinTrans$sales <- surJoinTrans$UNITS * surJoinTrans$PRICE_PER_UNIT_MASKED
hhDistr <- aggregate(WEEKS~HH, data = surJoinTrans, FUN = sum)
hhDistr$UNITS <- aggregate(UNITS~HH, data = surJoinTrans, FUN = sum)[,2]
hhDistr$UNITS_POST <- aggregate(UNITS_POST~HH, data = surJoinTrans, FUN = sum)[,2]
hhDistr$PRICE <- aggregate(PRICE_PER_UNIT_MASKED~HH, data = surJoinTrans, FUN = mean)[,2]
hhDistr$SALES <- aggregate(sales~HH, data = surJoinTrans, FUN = mean)[,2]
hhDistr$SALES_POST <- aggregate(sales_post~HH, data = surJoinTrans, FUN = mean)[,2]
hhDistr$SALESWK <- hhDistr$SALES/hhDistr$WEEKS
hhDistr$SALES_POSTWK <- hhDistr$SALES_POST/hhDistr$WEEKS
for (i in hhDistr$HH){
  hhDistr$ENDPOINT[which(hhDistr$HH==i)] <- wgm$EndPoint[which(wgm$HH==i)]
}
hhDistr

# aggregate by HH for those commented on M(takes means for sales)
commentM <- surJoinTrans[surJoinTrans$M=='N-'|surJoinTrans$M=='P+',] #subset transaction records from customers who commented on M
hhDistr.M <- aggregate(WEEKS~HH,data = commentM,FUN = sum) #aggregate WEEKS by HH
hhDistr.M$UNITS <- aggregate(UNITS~HH,data = commentM,FUN = sum)[,2] #aggregate UNITS by HH
hhDistr.M$UNITS_POST <- aggregate(UNITS_POST~HH,data = commentM,FUN = sum)[,2] # aggregate UNITS_POST by HH
hhDistr.M$SALES <- aggregate(sales~HH, data = commentM, FUN = mean)[,2]
hhDistr.M$SALES_POST <- aggregate(sales_post~HH, data = commentM, FUN = mean)[,2]
hhDistr.M$SALESWK <- hhDistr.M$SALES/hhDistr.M$WEEKS
hhDistr.M$SALES_POSTWK <- hhDistr.M$SALES_POST/hhDistr.M$WEEKS
for (i in hhDistr.M$HH){
  hhDistr.M$ENDPOINT[which(hhDistr.M$HH==i)] <- wgm$EndPoint[which(wgm$HH==i)]
}
hhDistr.M

# run a t.test to see if there is difference between those with different sentiments of M
t.test(commentM$WEEKS[commentM$M=='N-'],commentM$WEEKS[commentM$M=='P+']) # p-value = 0.6113
# there is no difference in 'WEEKS' by sentiment on M
t.test(commentM$UNITS[commentM$M=='N-'],commentM$UNITS[commentM$M=='P+']) # p-value = 0.4595
# there is no difference in 'UNITS' by sentiment on M
t.test(commentM$sales[commentM$M=='N-'],commentM$sales[commentM$M=='P+']) # p-value = 0.2631



#####################################################
# What's the size of prize----prediction
#####################################################
transJoinCustI$SALES <- transJoinCustI$UNITS*transJoinCustI$PRICE_PER_UNIT_MASKED
transJoinCustI$SALES_POST <- transJoinCustI$UNITS_POST*transJoinCustI$PRICE_PER_UNIT_MASKED
alterN.ins <- transJoinCustI[transJoinCustI$ALTERNATIVE=='N',] # non-alternative
alterT0.ins <- transJoinCustI[transJoinCustI$ALTERNATIVE=='T0',] # ITEM-X
alterT1.ins <- transJoinCustI[transJoinCustI$ALTERNATIVE=='T1',] # closet alternative
alterT2.ins <- transJoinCustI[transJoinCustI$ALTERNATIVE=='T2',] # further alternatives
t0ToPredict <- alterT0.ins[,c(4,13,14,16,21)]

# predict SALES
trainingData = t0ToPredict
validationData = alterT0.oth
lm1 = lm(SALES~WEEKS+DECILE+ZONE_NBR+HH_INCOME, data = t0ToPredict)
alterT0.oth <- othJoinCust[othJoinCust$ALTERNATIVE=='T0',]
lm1MSE = mean((predict(lm1,alterT0.oth) - alterT0.oth$SALES)^2,na.rm = T)
summary(lm1)
lm1MSE # 55.13489
lm2 = earth(SALES~WEEKS+DECILE+ZONE_NBR+HH_INCOME,data = na.omit(t0ToPredict))
lm2MSE = mean((predict(lm2,alterT0.oth) - alterT0.oth$SALES)^2,na.rm = T)
lm2MSE # 55.65321


# predict SALES_POST
t0ToPredict_post <- alterT0.ins[,c(4,13,14,16,20)]
lm1_post <- lm(SALES_POST~WEEKS+DECILE+ZONE_NBR+HH_INCOME, data = t0ToPredict_post)
lm1MSE_post = mean((predict(lm1_post,alterT0.oth) - alterT0.oth$SALES_POST)^2,na.rm = T)
summary(lm1_post)
lm1MSE_post # 4.679873

model8<-earth(SALES_POST~WEEKS+DECILE+ZONE_NBR+HH_INCOME,data = na.omit(t0ToPredict_post))
valid8 = mean((validationData$SALES_POST-predict(model8,validationData))^2,na.rm = T)
valid8 # 4.666297




#########################
# Which attribute is most influential
######################### 
# 辣鸡
# Key measure = 'UNITS'
# linear?
summary(lm(UNITS~. , data = transJoinCustI))
# weeks, item-masked, decile, zone_bar, income
anova(lm(UNITS~., data = transJoinCustI))
# HH_INCOME, HH_SIZE
# interactions?
install.packages("leaps")
library('leaps')
leapsModelsU = regsubsets(UNITS~.^2, data = transJoinCustI[,c(3,4,7,8,13:19)], method = 'forward')
subsetSummaryU = summary(leapsModelsU)
bestBICU = which.min(subsetSummaryU$bic)
round(coef(leapsModelsU, bestBICU),3)
# nah...
# non-linear?
library('earth')
earthFitU = earth(UNITS~. , data = na.omit(transJoinCustI))
plotmo(earthFitU)
# nah..
# conclu...辣鸡

#####################################################
# 辣鸡
# Key measure = 'WEEKS'
# linear?
summary(lm(WEEKS~., data = transJoinCustI))
# weeks, item-masked, zone_bar, income
anova(lm(WEEKS~., data = transJoinCustI))
# HH_INCOME, HH_SIZE
# interactions?
leapsModelsW = regsubsets(WEEKS~.^2, data = transJoinCustI[,c(3,4,7,8,13:19)], method = 'forward')
subsetSummaryW = summary(leapsModelsW)
bestBICW = which.min(subsetSummaryW$bic)
round(coef(leapsModelsW, bestBICW),3)
# nah...
# non-linear?
library('earth')
earthFitW = earth(WEEKS~. , data = na.omit(transJoinCustI))
plotmo(earthFitW)
# nah..
# conclu...辣鸡


#########################
# Predict on entire dataset
######################### 
# 辣鸡
set.seed(1)
isTraining = runif(nrow(transJoinCustI)) < 0.8
trainingData = subset(transJoinCustI, isTraining)
validationData = subset(transJoinCustI, !isTraining)
lm1 = lm(UNITS~WEEKS+UNITS_POST+DECILE+ZONE_NBR+HOH_AGE+HH_INCOME+HH_SIZE+HH_ADULTS+HH_CHILDREN, data = trainingData)
lm2 = lm(UNITS~.^2, data = trainingData)
lm1MSE = mean((predict(lm1, validationData) - validationData$UNITS)^2) # does not work
lm1MSE #NA???!


#####################################################
# aggregate by EndPoint
endpointDistr <- aggregate(WEEKS~EndPoint, data = surJoinTrans, FUN = sum)
endpointDistr$UNITS <- aggregate(UNITS~EndPoint, data = surJoinTrans, FUN = sum)[,2]
endpointDistr$UNITS_POST <- aggregate(UNITS_POST~EndPoint, data = surJoinTrans, FUN = sum)[,2]
endpointDistr$PRICE <- aggregate(PRICE_PER_UNIT_MASKED~EndPoint, data = surJoinTrans, FUN = mean)[,2]
endpointDistr
# how's endpoint distribution now
prop.table(table(surJoinTrans$EndPoint))
# E1         E2         E3         E4         E5         E6 
# 0.46471299 0.04102048 0.16866734 0.15246056 0.10732461 0.06581403 



#####################################################
# Clustering
# 辣鸡
library(cluster) 
library(fpc)

tocluster <- custdat[,2:8]
wss <- (nrow(tocluster)-1)*sum(apply(tocluster,2,var))
for (i in 2:16) wss[i] <- sum(kmeans(na.omit(tocluster), # omit NAs
                                     centers=i)$withinss) 
par(mar=c(3,5,5,1))
plot(1:16, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# no. of cluster = 2 or 3 or 7 has lower wss
pm1 = pamk(na.omit(tocluster),scaling=TRUE)
pm1$nc #2 suggested cluster
# k = 2
percsize = paste(1:2," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
par(mar=c(3,2,2,1))
pie(km1$size,labels=percsize)
km1m = aggregate(na.omit(tocluster),by=list(km1$cluster),FUN=mean)

# k = 3
km2 = kmeans(na.omit(tocluster),3,iter.max = 20, nstart=2)
percsize = paste(1:3," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)

# k = 7
km3 = kmeans(na.omit(tocluster),7,iter.max = 20, nstart=2)
percsize = paste(1:7," = ",format(km3$size/sum(km3$size)*100,digits=2),"%",sep="")
pie(km3$size,labels=percsize)

# plotting
plotClust = function(km,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  if (nc > 2) {
    clusplot(na.omit(tocluster), km$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,col.clus=nc:1); #plot clusters against principal components
  } else {
    clusplot(na.omit(tocluster), km$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components    
  }
  
  if(discPlot){
    plotcluster(na.omit(tocluster), km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}
par(mar=c(3,3,1,1))
plotClust(km1)
plotClust(km2)
plotClust(km3)
