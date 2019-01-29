############ Preparation works ############
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("/00Courses/03 Research/Lab/Lab 4 Regression and Cluster Analysis")
# call the library for getting data from other programs
library(foreign)

############ Cluster analysis in R ############
## Post Hoc Segmentation (cluster analysis)
filnm = "Cluster Analysis Comedy Data"
spssdatalab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssdata <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE)
attr(spssdata, "variable.labels") # get specific attributes of an object

# install.packages("cluster")
# install.packages("fpc")
library(cluster) 
library(fpc)

toclust = spssdata[,1:19]    # select the relevant data for clustering
# weighted sum of squares
wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var)) # number of rows-1乘 column variance
# 计算各种cluster个数情形下的sum of squares
# sum of 'within-cluster sum of squares' for no. of clusters = 2:15
?kmeans
for (i in 2:15) wss[i] <- sum(kmeans(toclust, 
                                     centers=i)$withinss) 

# Past 3 lines equavalent to
# for (i in 1:15) wss[i] <- sum(kmeans(toclust, 
#                                      centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# 2个cluster和3个cluster时候wss明显变小

##use pamk() to determine the optimal number of clusters 
?pamk
pm1 = pamk(toclust,scaling=TRUE)
pm1$nc #2

##Create two new kmeans clusters
km1 = kmeans(toclust,3,iter.max = 20, nstart=2)
km2 = kmeans(toclust,2,iter.max = 20, nstart=2)

##function turns min into 0, max into 1 and scales rest linearly
rscale = function(x){(x-min(x))/(max(x)-min(x));}

km1m = aggregate(toclust,by=list(km1$cluster),FUN=mean)
km2m = aggregate(toclust,by=list(km2$cluster),FUN=mean); #calculate profile means
km1ms = apply(km1m[,2:ncol(km1m)],2,rscale); #rescale profile means for easy presentation
par(mar=c(8.1,4.1,4.1,2.1)); #setting margins to give room for x axis labels
matplot(t(km1ms),col=c(1,4,2),ylab="Mean Value (Range Normalized)",xaxt="n")
axis(side=1,at=1:19,labels=names(toclust),las=2); #putting x axis labels on plot
##plots the three clusters with 1 max and 0 min for each variable
##notice need to know what the variables are!

percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km1$size,labels=percsize)
##plots the relative size of each group

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0); #plot clusters against principal components
##idea of this plot is identify the clusters 
##the axes are the principal components, which are sort of a mixture 
##  of all of the variables used in the clustering

# Centroid Plot against 1st 2 discriminant functions
plotcluster(toclust, km1$cluster); #plot against discriminant functions
## idea of this plot is that the axes are the two best 
## functions to distinguish the clusters;

