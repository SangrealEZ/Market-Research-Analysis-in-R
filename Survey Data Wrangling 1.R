# Preparations before you do anything
# remove everything in the working environment
rm(list = ls())
# set working directory, i.e. where your files and data are located
setwd("~/Dropbox/Rochester/2017 Winter/TA/Lab 2")
setwd("D:/Dropbox/Rochester/2017 Winter/TA/Lab 2")


# The string data type
x = "MKT412"; y = "Marketing Research"
paste(x, " ",y)   # concatenate strings
paste("Today is", date())


# Create a data frame
price = c(1.01,0.99,0.92,1.08)       # build the price column
adv = c(TRUE, FALSE, TRUE, TRUE)     # build the adv column
sales = c(1500,1600,1200,2000)       # build the sales column
regionalSales = data.frame(price,adv,sales) # build data frame
# now add row names
rownames(regionalSales) = c("north","east","south","west") 
names(regionalSales) = c("price","adv","sales")   # change variable names
names(regionalSales)[2] = "adv"                 # just change the 2nd name

## Indexing
regionalSales[2, 3]      # cell of 2nd row and 3rd column 
regionalSales[,1]       # any row, column 1
regionalSales$price
# Column indexing
regionalSales["sales"]  # same as regionalSales[3]
regionalSales[c("sales", "price")] # = regionalSales[c(3,1)]
# Rrow indexing
regionalSales[adv == TRUE,] # only rows with adv equals to TRUE
regionalSales[price > 1.00,] # only rows with price larger than 1
# More indexing
regionalSales[adv == TRUE & sales > 1250,]
regionalSales[adv == TRUE & sales > 1250, c(1,2)]
regionalSales[adv == TRUE & sales > 1250, c(1,3)]


# Import data
dfCsv = read.csv("Lab2_coffee.csv")
dfTxt = read.table("Lab2_coffee.txt", header = TRUE, sep="\t")
# Excel spreadsheet
install.packages("gdata", dep = T)      
library(gdata)
# install.packages('xlsx')     # install the xlsx package
library(xlsx)                # load the xlsx package,it requires rjava and always failed
dfXlsx = read.xls("Lab2_coffee.xlsx", 1)   # 1 is the sheet number
# Spss file
# install.packages('foreign')
library(foreign)
dfSpss  = read.spss("National Insurance-DATA.sav", to.data.frame = T, use.value.labels = TRUE) # last option converts value labels to R factors
dfSpss2 = read.spss("National Insurance-DATA.sav", to.data.frame = T, use.value.labels = FALSE) 


rm(dfTxt, dfXlxs, dfSpss, dfSpss2)  # remove the object
# rm(list = ls())       # remove everything in the working environment

# Save data frame to r data file
save(dfCsv, file="coffeeData.Rdata")
# Save all objects in the workspace to r data file
save(list=ls(), file="allData.Rdata")

# Save to another data frame called sampleData
sampleData = dfCsv
# Data description
head(sampleData)		# First 6 observations
head(sampleData,10)		# First 10 observations
tail(sampleData)		# Last 6 observations
ls(sampleData)			# All variables in the data
summary(sampleData)		# Summary statistics for each variable

# cross tab
table(sampleData$brand)
sampleData$seg_br = paste(sampleData$segment, sampleData$brand, sep='')  # create segment-brand pair
table(sampleData$seg_br, sampleData$month)


## ploting
sampleData = read.csv("Lab2_coffee.csv")
attach(sampleData)
detach(sampleData)
detach()       # detach everything
attach(sampleData)

# one dimensional plot
plot(price)
hist(price)
# two dimensional plot
plot(sale_cup, price)
plot(pcACV, sale_cup)
# bar plot 
# Question: how many brands are available in each segment?
count1 = unique(data.frame(segment, seg_br)) # remove the replicate entries for the segment-brand pair
count2 = table(count1$segment)               # count how many brands in each segment
barplot(count2, main="# brands in each segment")

############################################################
## 
## Review of Statistics using R
##
############################################################
# # Generate a dataset
# set.seed(12345)
# nf = 60
# nm = 50
# scoreFemale = rnorm(nf,mean = 0.65, sd = 0.2)
# scoreMale   = rnorm(nm,mean = 0.6, sd = 0.2)
# sampleData  = data.frame(c(scoreFemale, scoreMale), c(rep("female",nf), rep("male",nm)))
# names(sampleData) = c('score','gender')

load("sampleData.Rdata")

# One sample t-test
t.test(sampleData$score, alternative="two.sided", mu = 0.5)

# Independent two sample t-test
t.test(score ~ gender, data = sampleData, mu = 0, var.equal = T)
scorem = sampleData$score[sampleData$gender == "male"]
scoref = sampleData$score[sampleData$gender == "female"]
t.test(x = scorem, y = scoref, mu = 0, var.equal = T)


# Paired two sample t-test
scoreBefore = sampleData$score
scoreAfter = scoreBefore + rnorm(length(scoreBefore), mean = 0.15, sd = 0.05)
t.test(x = scoreBefore, y = scoreAfter, paired = T, var.equal = T)


# Chisq test: goodness of fit
pfemale = 0.500
chisq.test(c(1200, 1080), p = c(pfemale, 1 - pfemale))

# Chisq test: association
observation= matrix(c(36,30,14,25), 2, 2)
chisq.test(observation, correct = FALSE)
