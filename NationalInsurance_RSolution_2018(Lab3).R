##Set the working directory (can do using menus)
#setwd("~/Dropbox/Marketing Research/Cases/National Ins")
#setwd("C:/Users/mitch.lovett/Dropbox/Marketing Research/Cases/National Ins")
# Preparations works
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("C:/Dropbox/Rochester/Classes/2018 Winter/Lab 3")
#Need to ajust the setwd() filename (surrounded by "") to match your working directory

#install and call the ggplot2 library, which contains plotting functions we will use
# install.packages("ggplot2", dependencies=T)
library(ggplot2)

#call the library for getting data from other programs
library(foreign)
filnm = "national"; #this is the name of the file
spssDataLab <- read.spss(paste(filnm,".sav",sep=""), to.data.frame = TRUE, use.value.labels = TRUE, trim_values=TRUE)
spssData    <- read.spss(paste(filnm,".sav",sep=""), to.data.frame = TRUE, use.value.labels = FALSE); #turning values into numbers, but be sure getting right values to numbers!

##0) Summarize variables and validate
summary(spssDataLab); #a quick look, but must examine carefully

##1) Test whether sample matches with population for gender and length of use
##check gender:
popSEX = c(.54, .46); #true population values
cbind(popSEX, prop.table(table(spssData$sex))); #creating table as matrix
chisq.test(table(spssData$sex), p = popSEX)

##check length of use
popLU = c(.08, .09, .18, .65); #true population values
cbind(popLU, prop.table(table(spssData$use))); #creating table as matrix
chisq.test(table(spssData$use), p = popLU)

#weights, if reweighting
w = popLU/prop.table(table(spssData$use)); ##formula is w=P/M
#setting weight for each response in sample
wSamp = w[as.numeric(spssData$use)]; #this command indexes into w for each response picking the right value

##2) Describe the performance and importance variables. 
##  How are they doing? What do they do best? 
##  What do customers think is most important?

##When dealing with sets of variables
##Create a "vector" using c() of the labels for the set
## of variables and another for the full labels

#create indexing for performance and importance variables
perfVars = c("reliavrg","empavrg","tangavrg","respavrg","assuravg")
impVars  = c("relimp","empimp","tanimp","resimp","asrimp")

#create full names for performance and importance variables
genVarsf  = c("Reliability","Empathy", "Tangibles","Responsiveness","Assurance")
perfVarsf = c("Reliability Perf.","Empathy Perf.",
              "Tangibles Perf.","Responsiveness Perf.","Assurance Perf.")
impVarsf  = c("Reliability Imp.","Empathy Imp.",
             "Tangibles Imp.","Responsiveness Imp.","Assurance Imp.")

#calculate means and standard errors for performance
perfMeans = colMeans(spssData[, perfVars], na.rm = TRUE)
perfSE = apply(spssData[,perfVars], 2, sd, na.rm = TRUE) / 
  sqrt(colSums(!is.na(spssData[, perfVars])))
print(perfSE)
#calculate Means and standard errors for importance
impMeans = colMeans(spssData[, impVars], na.rm = TRUE)
impSE = apply(spssData[, impVars], 2, sd,na.rm = TRUE)/
  sqrt(colSums(!is.na(spssData[, impVars])))
# create a new data frame based on the new values calculated
perfImpDF = data.frame(genVarsf, perfVars, perfVarsf, perfMeans, perfSE, impVars, impVarsf, impMeans, impSE)


#create error bar plots for the performance variables and the importance variables
dodge = position_dodge(width=.75); ##to form constant dimensions positioning for all geom's
gp = ggplot(perfImpDF,aes(y=perfMeans,x=genVarsf,ymax=perfMeans+perfSE,ymin=perfMeans-perfSE))
gp + geom_bar(position=dodge,stat="identity",col=1,fill=2,width=.75) + 
  geom_errorbar(position=dodge,width=1) + labs(x="Performance",y="Mean Rating (1-7)")

gi = ggplot(perfImpDF,aes(y=impMeans,x=genVarsf,ymax=impMeans+impSE,ymin=impMeans-impSE))
gi + geom_bar(position=dodge,stat="identity",col=1,fill=2,width=.75) + 
  geom_errorbar(position=dodge,width=1) + labs(x="Importance",y="Mean of Allocated Out of 100")

##can run t-tests on these, but with the s.e., we already have the gist
t.test(spssData[,perfVars[1]],spssData[,perfVars[2]],paired=TRUE)


###3) Do perceptions and importances differ between the genders?

##Using a loop here. This repeats the command over the iterator (in this case i)
for(i in 1:5){  #note 5 is how long perfVars and impVars are each
  cat(paste("******",perfVarsf[i],"*****"),fill=TRUE); #notice indexing into perfVarsf using i
#  print(tapply(spssData[,perfVars[i]],spssDataLab$sex,mean,na.rm=TRUE))
  print(t.test(spssData[spssDataLab$sex=="Male",perfVars[i]],spssData[spssDataLab$sex=="Female",perfVars[i]]))
  cat(paste("******",impVarsf[i],"*****"),fill=TRUE)
#  print(tapply(spssData[,impVars[i]],spssDataLab$sex,mean,na.rm=TRUE))
  print(t.test(spssData[spssDataLab$sex=="Male",impVars[i]],spssData[spssDataLab$sex=="Female",impVars[i]]))  
}

#4) Are problems and resolved problems associated with different levels of satisfaction?
###Do people who experience problems have higher or lower overall service quality?
table(spssDataLab$prob,spssDataLab$oq)
prop.table(table(spssDataLab$prob,spssDataLab$oq),1) #prop.table constructs row percentages
chisq.test(spssDataLab$prob,spssDataLab$oq)
##alternatively, treat as interval variable and use t.test
t.test(spssData$oq[spssDataLab$prob=="Yes"],spssData$oq[spssDataLab$prob=="No"],)

###Do people whose problems are resolved have higher or lower overall service quality?
print(prop.table(table(spssDataLab$resolve,spssDataLab$oq),1),digits=2)
chisq.test(spssDataLab$resolve,spssDataLab$oq)
##alternatively, treat as interval variable and use t.test
t.test(spssData$oq[spssDataLab$resolve=="Yes"],spssData$oq[spssDataLab$resolve=="No"],)

############################################################
##Some extra tips you will probably find useful for homework
############################################################

#if you want to recode some columns of to a different value
##create a new variable
osq = spssData$oq
##set the values to the new values
osq[osq < 5] = 0; #subsetting only those variables that have values less than 5 and then setting those values to 0
osq[osq >=5] = 1 #subsetting to only those variables that have values greater than or equal to 5 and then setting those values to 1


##If you want to drop some observations: recode the values to NA and use the na.rm=TRUE
##what is we don't want to include the most satisfied from the analysis? (variable osq2 is without the most satisfied)
osq2 = osq; #notice I am creating a new variable - use the new variable for the analysis!
osq2[osq2==10]=NA;; ##notice you need to use "==" to mean = when you mean equality. that's because = has a different meaning and will cause problems

##if you want to copy text data to a spreadsheet:
write.table(spssDataLab[,cols], file = "MYFILENAME.txt", sep = "\t")
##where cols contains the columns you want to save and
##      MYFILENAME is the file name you want to save your file as
##After running this command, you have to open the file in excel (it is a text file)

##You can use the wordcloud tool from Intro to Business Analytics, too, for 
## quickly plotting/visualizing text frequencies from free responses
install.packages("tm",dependencies=T)
install.packages("wordcloud", dependencies=T)
library(wordcloud)
library(tm)

txt =  "Many years ago the great British explorer George Mallory, who 
was to die on Mount Everest, was asked why did he want to climb 
it. He said, \"Because it is there.\"

Well, space is there, and we're going to climb it, and the 
moon and the planets are there, and new hopes for knowledge 
and peace are there. And, therefore, as we set sail we ask 
God's blessing on the most hazardous and dangerous and greatest 
adventure on which man has ever embarked."
wordcloud(txt,random.order=FALSE)
