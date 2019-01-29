#Set the working directory
setwd("~/Desktop/Rochester/Q-Winter/MR/Assignment/3 Yogurt")

#install and call the ggplot2 library
install.packages("ggplot2", dependencies=T)
library(ggplot2)

install.packages("tm",dependencies=T)
install.packages("wordcloud", dependencies=T)
library(wordcloud)
library(tm)

#call the library for getting data from SPSS
library(foreign)
spssDataLab <- read.spss("Wegmans Survey 1.sav", to.data.frame = TRUE, use.value.labels = TRUE, trim_values = TRUE)
#turning values into numbers, but be sure getting right values to numbers!
spssData <- read.spss("Wegmans Survey 1.sav",to.data.frame=TRUE,use.value.labels=FALSE)
summary(spssDataLab)


#######Q1######
#check gender
#true population values
popSEX <- c(0.87,0.13)
chisq.test(table(spssData$Question33Areyou)[2:3],p=popSEX)
#p-value is 0.02235, which means we should reject the hypothesis.

sexlevel = levels(spssData$Question33Areyou)
cleanData <- subset(spssData, Question33Areyou %in% sexlevel[2:3])

#weights, if reweighting
w = popSEX/prop.table(table(spssData$Question33Areyou)[2:3])
table(w)
w

library(data.table)
dt <- data.table(spssData)

#######Q2######
# find data of Q6
colNames = names(spssData)
impVarQ6 = colNames[grep("Question6",colNames)]
impVarQ6 <- c("Question6Allnatural","Question6Blended","Question6Calorielevel","Question6Consistency",
              "Question6Fatlevel","Question6Fruitonthebottom","Question6Organic",
              "Question6Price","Question6Proteinlevel","Question6rbSTfree","Question6Sidebysidecup",
              "Question6Taste","Question6Texture")
# convert 'unsure' to NA
dfQ6 = spssData[impVarQ6]
for(c in impVarQ6){
        dfQ6[which(dfQ6[,c] == 5),c] = NA
}
#calculate means of Q6
MeansQ6df = colMeans(dfQ6, na.rm =TRUE)
# standard error
SEQ6df <- apply(dfQ6,2,sd,na.rm=TRUE)/
        sqrt(colSums(!is.na(dfQ6)))
Impdf <- data.frame(impVarQ6,MeansQ6df,SEQ6df)
dodge = position_dodge(width=.75); ##to form constant dimensions positioning for all geom's
gp = ggplot(Impdf,aes(y=MeansQ6df,x=impVarQ6,ymax=MeansQ6df+SEQ6df,ymin=MeansQ6df-SEQ6df))
gp + geom_bar(position=dodge,stat="identity",col=1,fill=8,width=.75) +
        geom_errorbar(position=dodge,width=1) + labs(x="Importance",y="Mean Rating (1-7)")
MeansQ6df
summary(dfQ6)

#######Q4######
Q24 <- c("Question24Allnatural","Question24Price","Question24Taste")
Q30 <- c("Question30Allnatural","Question30Price","Question30Taste")
dfQ24 = spssData[,Q24]
dfQ30 = spssData[,Q30]
# convert 'unsure' to NA
for(c in Q24){
        dfQ24[which(dfQ24[,c] == 6),c] = NA
}
for(c in Q30){
        dfQ30[which(dfQ30[,c] == 6),c] = NA
}


# t-test
for (i in 1:3){
        print(t.test(dfQ24[i],dfQ30[i]),paired = TRUE)
}

#######Q5######
wcook = "Question12DoyouuseGreekYogurtforcooking"
# natural/organic/price/rbstfree
cookmean <- colMeans(spssData[spssData[,wcook] == 'Yes',impVarQ6[c(1,7,8,10)]],na.rm = TRUE)
cookmean
ncookmean <- colMeans(spssData[spssData[,wcook] == 'No ',impVarQ6[c(1,7,8,10)]],na.rm = TRUE)
ncookmean
cbind(cookmean,ncookmean)
attributes(spssData[,wcook])


