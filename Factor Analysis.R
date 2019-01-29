############ Preparation works ############
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("/00Courses/03 Research/Lab/Lab 6 Predictive Analysis and Factor Analysis")

############ Get the data ############
filnm = "Wegmans Data for Factor Analysis"
library(foreign)
spssdat = read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE);

############ Factor analysis ############
# Get colnames for alll the product characteristics
vars = names(spssdat)[c(2,3,4,5,8,9,10,11)]

# Select those variables for factor analysis
toFactor = as.matrix(spssdat[,vars]); #selecting columns
toFactor = toFactor[rowSums(is.na(toFactor))<1,] #dropping missing data

# Investigate correlation
cor(toFactor)

# Install and load the packages for factor analysis
install.packages("GPArotation")
library(GPArotation)
install.packages("psych")
library(psych)
# fa.parallel determine how many factors to use
?fa.parallel
par(mar=c(3,3,1,1))
fa.parallel(toFactor,fa="fa",n.iter=100,main="Scree plots with parallel analysis")
# One way to determine the number of factors or components in a data matrix or a correlation matrix is to examine the “scree" plot of the successive eigenvalues. 
# Sharp breaks in the plot suggest the appropriate number of components or factors to extract. “Parallel" analyis is an alternative technique that 
# compares the scree of factors of the observed data with that of a random data matrix of the same size as the original. 
# fa.parallel.poly does this for tetrachoric or polychoric analyses.
# fa to calculate 
f1 = fa(toFactor,rotate="Varimax",nfactors=2)
plot(f1)
# f2 = fa(toFactor,rotate="none",nfactors=2)
# plot(f2)
