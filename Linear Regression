#################################################################
## Linear regression in R and interactions
#################################################################
# Preparations works
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("/00Courses/03 Research/Lab/Lab 4 Regression and Cluster Analysis")

#call the library for getting data from other programs
library(foreign)
filnm = "national"; #this is the name of the file
spssData <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE); #turning values into numbers, but be sure getting right values to numbers!

#create indexing for performance and importance variables
perfVars = c("reliavrg","empavrg","tangavrg","respavrg","assuravg")
impVars = c("relimp","empimp","tanimp","resimp","asrimp")

#create full names for performance and importance variables
genVarsf = c("Reliability","Empathy", "Tangibles","Responsiveness","Assurance")
perfVarsf = c("Reliability Perf.","Empathy Perf.",
              "Tangibles Perf.","Responsiveness Perf.","Assurance Perf.")
impVarsf = c("Reliability Imp.","Empathy Imp.",
             "Tangibles Imp.","Responsiveness Imp.","Assurance Imp.")

##Examining relationship between satisfaction dimensions and overall satisfaction

##First correlations: zero-order relationship
cor(cbind(spssData$oq,spssData[,perfVars]),use="complete.obs")

##Then full regression
lm1 = lm(spssData$oq~as.matrix(spssData[,perfVars]))
# lm1 = lm(spssData$oq~spssData[,perfVars])
summary(lm1)
##now add in whether they had a problem - this is main effects model
##First let's recode so that those with problems are 1 and those without are 0
prob = spssData$prob
prob[prob==2]=0;
lm2 = lm(spssData$oq~as.matrix(spssData[,perfVars])+prob)
summary(lm2)

##Does resolving help once controlling for the other variables?
##setting missing cases of resolve to 0 and recoding so 0 is no and 1 is yes (already the case for yes)
resolve = spssData$resolve
resolve[is.na(resolve)]=0;
resolve[resolve==2]=0;
lm3 = lm(spssData$oq~as.matrix(spssData[,perfVars])+prob+resolve)
summary(lm3)
anova(lm2,lm3)
## answer is no. the other variables explain the same thing as resolve, 
##so I will drop the resolve variable

##Then test whether these differ by whether they had a problem
##  hypothesis here is that empathy matters more if they had a problem
lm4 = lm(spssData$oq~as.matrix(spssData[,perfVars])*prob); ##notice using * to indicate crossing
summary(lm4)
##do the interactions lead to model improvement? (compared to lm2, not lm3)
anova(lm2,lm4); ##yes! so what does this mean?
noProb = 1-prob
lm5 = lm(spssData$oq~0+as.matrix(spssData[,perfVars]):(noProb+prob)+noProb+prob); ##notice using * to indicate crossing
summary(lm5)
