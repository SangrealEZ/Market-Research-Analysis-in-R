rm(list = ls())
setwd("/00Courses/03 Research/HW/05 Final Project")
# load survey data
load("/00Courses/03 Research/HW/05 Final Project/MKT412R - Final Analysis Case Data.Rdata")
colnames(desmat) <- c('price','height','motion','style')
# check product profile
prodProfile <- desmat[1:16,] 

## calculate partworths for each respondent
desmatf = cbind(rep(1,nrow(desmat)),desmat); ##add column for intercept
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf)) ## create a matrix to store individual partworths
for(i in 1:sampsize){ #for each individual run the regression and fill in the matrix
  partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
} 
colnames(partworths) = c("Intercept","Price","Size","Motion","Style")


###############################################################
## Part D
## Market Simulation
# repeat individual partworths for 16 times for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5) 
pratings = rowSums(desmatf*partworths.full) # predicted rating for each product profile
finalratings = ifelse(is.na(ratings),pratings,ratings) # filling NAs
# tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles); ##this has 16 rows for profiles and sampsize columns

# fill in product information
prodProfile <- as.data.frame(prodProfile)
prodProfile$price <- ifelse(prodProfile$price ==1, 95.99,111.99)
prodProfile$height <- ifelse(prodProfile$height==1,'26','18')
prodProfile$motion <- ifelse(prodProfile$motion==1,'rocking','bouncing')
prodProfile$style <- ifelse(prodProfile$style==1,'glamorous','racing')
prodProfile$VC <- rep(c(21,29,33,41),each = 2)
prodProfile$Num <- 1:16


# create a function to compute market share
simDec = function(inputmat,scen){
  ##inputmat is the ratings matrix with rows as profiles and cols as ratings
  ##secn is the list of products in the market for the scenario (these are rows in inputmat)
  inmkt = inputmat[scen,]
  max = apply(inmkt,2,max)
  firstChoices = (inmkt>rep(max,each=length(scen))-.000000000001)
  shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}
# create a function to calculate profit
## inputmat and scen is as above. myprods are indicators of which prods are the firms,
## prices are the prices for all products, vcosts are the variable costs
## fcosts are the fixed costs for the firm (need to calculate in already the number of products)
simProfit = function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
  mktshr = simDec(inputmat,scen);
  vprofit = mktshr * (prices-vcosts)*mktsize;
  sum(vprofit[myProds])-fcosts
}


# create matrix for all scenarios
install.packages('gtools')
library(gtools)
productChoice <- c(1:16)
comboTwo <- combinations(n=16,r=2,v=productChoice,repeats.allowed=F)
nrow(comboTwo) #120 scenarios
comboThree <- combinations(n=16,r=3,v=productChoice,repeats.allowed=F)
nrow(comboThree) #560
competitor <- c(7,8)

scenTwoHigh <- cbind(rep(7,nrow(comboTwo)),comboTwo)
scenTwoLow <- cbind(rep(8,nrow(comboTwo)),comboTwo)
scenTwo <- as.data.frame(rbind(scenTwoHigh,scenTwoLow))

scenThreeHigh <- cbind(rep(7,nrow(comboThree)),comboThree)
scenThreeLow <- cbind(rep(8,nrow(comboThree)),comboThree)
scenThree <- as.data.frame(rbind(scenThreeHigh,scenThreeLow))
nrow(scenTwo)+nrow(scenThree) # 1360 scenarios

# If EarlyRiders offers only 2 products
# create matrix to store output
simProfTwo <- matrix(NA, nrow(scenTwo),2) 
priceTwo <- matrix(NA,nrow(scenTwo),3)
varCostTwo <- matrix(NA,nrow(scenTwo),3)
# fill in the martix
for(i in 1:nrow(scenTwo)){
  for(j in 1:3){
  varCostTwo[i,j] <- prodProfile[prodProfile$Num==scenTwo[i,j],5]
  priceTwo[i,j] <- prodProfile[prodProfile$Num==scenTwo[i,j],1]
  }
  }
for(i in 1:nrow(scenTwo)){
  simProfTwo[i,1]<- sum(simDec(simDecInput,unlist(scenTwo[i,1:3]))[2:3])
  simProfTwo[i,2]<-simProfit(simDecInput,unlist(scenTwo[i,1:3]),c(2,3),unlist(priceTwo[i,]),
                             unlist(varCostTwo[i,]),40000,4000)
}

scenTwo <- cbind(scenTwo,rep(NA,nrow(scenTwo)))
twoProd <- cbind(scenTwo,simProfTwo)
twoProd[which.max(twoProd[,6]),] #7 10 15 0.975 247629
twoProd[which.max(twoProd[,5]),] #7  6 16 1 194040
colnames(twoProd)<-c('competitor','P1','P2','P3','MktShare','Profit')

# If EarlyRiders offers 3 products
# create matrix to store output
simProfThr <- matrix(NA, nrow(scenThree),2) 
priceThr <- matrix(NA,nrow(scenThree),4)
varCostThr <- matrix(NA,nrow(scenThree),4)

for(i in 1:nrow(scenThree)){
  for(j in 1:4){
  varCostThr[i,j] <- prodProfile[prodProfile$Num==scenThree[i,j],5]
  priceThr[i,j] <- prodProfile[prodProfile$Num==scenThree[i,j],1]
  }
}
for(i in 1:nrow(scenThree)){
  simProfThr[i,1]<- sum(simDec(simDecInput,unlist(scenThree[i,]))[2:4])
  simProfThr[i,2]<-simProfit(simDecInput,unlist(scenThree[i,]),c(2,3,4),unlist(priceThr[i,]),
                             unlist(varCostThr[i,]),60000,4000)
}
thrProd <- cbind(scenThree,simProfThr)
thrProd[which.max(thrProd[,6]),] #max profit = 7  7 10 11 0.962 235185.5
thrProd[which.max(thrProd[,5]),] #max share = 7  1  6 16 1 174152
colnames(thrProd)<-c('competitor','P1','P2','P3','MktShare','Profit')

# combine all simulation results together
simulations <- rbind(twoProd,thrProd)
highP <- simulations[simulations$competitor==7,]
lowP <- simulations[simulations$competitor==8,]

# best senarios
# maximize profit at first
highP[which.max(highP$Profit),] # 10+15,share = 97.5%,profit = 247629
highP[which.max(highP$MktShare),] # 6+16,share = 100%,profit = 194040
simulations[simulations$P1==10&simulations$P2==16,]
# when competitor shows no response, most profitable scenario is to offer product 10+15
# this brings a profit of $247629 and market share = 97.5%
# counterpart low price combo for this is 10&16, if we offer product mix like this after competitor lowers price,
# we can have a market share of 63.3% and a profit of $118594.7
simulations[simulations$P1==2&simulations$P2==15,]
246997.1*0.2+0.8*172887.4 #187709.3 #80% probability to lower the price

# maximize profit afterwards
lowP[which.max(lowP$Profit),] # 4+6+16,share = 98.6%,profit = 185424.6
lowP[which.max(lowP$MktShare),] # same as above
simulations[simulations$P1==3&simulations$P2==5 &simulations$P3=="15",]
# when competitor lowers price, most profitable scenario is to offer product 4+6+16
# this brings a profit of $185424.6 and market share = 98.6%
# counterpart high price combo for this is 3+5+15, if we offer product mix like this before competitor lowers price,
# we can have a market share of 92.1% and a profit of $228199.16
185424.6*0.8+0.2*228199.16 #193979.5 #80% probability to lower the price

write.csv(simulations,file = 'sim.csv')
write.csv(prodProfile,file = 'product.csv')
