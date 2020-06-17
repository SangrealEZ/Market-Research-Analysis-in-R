# Preparations works
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("C:/Dropbox/Rochester/Classes/2018 Winter/Lab 3")

library(foreign)
filnm = "national"; #this is the name of the file
spssDataLab = read.spss(paste(filnm, ".sav", sep = ""), to.data.frame = TRUE, use.value.labels = TRUE, trim_values = TRUE)
spssDataLab$oq = as.numeric(spssDataLab$oq)
library(ggplot2)
# Histogram for reliability rating
# hist(spssDataLab$reliavrg)
plotRelia = ggplot(spssDataLab, aes(reliavrg))
plotRelia + geom_histogram()
# Change the bin width to 1
plotRelia + geom_histogram(binwidth = 1)
# See the frequencies for different gender
plotRelia = ggplot(spssDataLab, aes(reliavrg, fill = sex))
plotRelia + geom_histogram()


# Barplot
# barplot(table(spssDataLab$age))
gp = ggplot(spssDataLab, aes(age))
gp + geom_bar()
# Barplot with error bar
# See the national insurance case file


# Scatter plot
# Single variable plot
# plot(1:length(spssDataLab$oq), spssDataLab$oq)
plotOq = ggplot(spssDataLab, aes(x = 1:length(oq), y = oq))
plotOq + geom_point(shape = 1)

# Two variables plot
# plot(spssDataLab$reliavrg, spssDataLab$oq)
plotOqRelia = ggplot(spssDataLab, aes(x = reliavrg, y = oq))
# Simple two-variable plot
plotOqRelia + geom_point(shape = 1)
# With regression line
# plot(spssDataLab$reliavrg, spssDataLab$oq)
# abline(lm(oq ~ reliavrg, spssDataLab))
# preds <- predict(lm(oq ~ reliavrg, spssDataLab), newdata = spssDataLab, 
#                  interval = 'confidence')
# polygon(c(rev(spssDataLab$reliavrg), spssDataLab$reliavrg), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
plotOqRelia + geom_point(shape = 1) + geom_smooth(method = lm)
# No confidence region
# plot(spssDataLab$reliavrg, spssDataLab$oq)
# abline(lm(oq ~ reliavrg, spssDataLab))
plotOqRelia + geom_point(shape = 1) + geom_smooth(method = lm, se = FALSE)
# With flexibly fitted line
# plot(spssDataLab$reliavrg, spssDataLab$oq)
# lines(lowess(spssDataLab$reliavrg, spssDataLab$oq, delta = 0.01 * diff(range(spssDataLab$reliavrg, na.rm = TRUE))))
plotOqRelia+ geom_point(shape = 1) + geom_smooth()


# Loops
# Ex1
# Repeat some action
t = 0
for(i in 1:1000){
	t = t + 1
}
# Ex2
# Go through a variable/data frame using begin:end
for(i in 1:length(spssDataLab$age)){
	print(spssDataLab$age[i])
}
# Ex3
# Go through possible values in a variable using unique()
for(curAge in unique(spssDataLab$age)){
	subData = subset(spssDataLab, spssDataLab$age == curAge)
	# Do something with the subdata
}

# Aggregation
# Average reliability rating for genders
tapply(spssDataLab$reliavrg, spssDataLab$sex, mean, na.rm = TRUE)
aggregate(spssDataLab$reliavrg, by = list(spssDataLab$sex), mean, na.rm = TRUE)
aggregate(reliavrg ~ sex, spssDataLab, mean, na.rm = TRUE)
# Average realiability rating for gender-age groups
aggregate(reliavrg ~ sex + age, spssDataLab, mean, na.rm = TRUE)

# Data.table example
install.packages("data.table")
library(data.table)
setDT(spssDataLab)

# Row indexing
spssDataLab[1:2]
spssDataLab[sex == "Male" & mstat == "Married"]

# Column indexing
spssDataLab[, sex]
spssDataLab[, list(sex)]
spssDataLab[, "sex", with = FALSE]
spssDataLab[, .(gender = sex, education = ed)]

# Calculation using data.table
# Average reliability rating
spssDataLab[, mean(reliavrg, na.rm = TRUE)]

# Average reliability rating for female
spssDataLab[sex == "Female", mean(reliavrg, na.rm = TRUE)]

# Aggregation: number of observations for each gender
spssDataLab[, .N, by = sex] # equivalent to spssDataLab[, length(reliavrg), by = sex]
# Aggregation: average reliability rating for genders using data.table
spssDataLab[, mean(reliavrg, na.rm = TRUE), by = sex]
spssDataLab[, list(avg.reliavrg = mean(reliavrg, na.rm = TRUE)), by = sex]

# Aggregation for multiple columns
spssDataLab[, list(avg.reliavrg = mean(reliavrg, na.rm = TRUE), avg.tangavrg = mean(tangavrg, na.rm = TRUE)), by = sex]
# Using .SD for a large number of columns
spssDataLab[, lapply(.SD, mean, na.rm = TRUE), by = sex, .SDcols = c("reliavrg", "tangavrg")]

# Assignment in data.table
# Create a new variable called per.replimp, which converts importance of reliability into percentage value
spssDataLab[, per.relimp := relimp/100]
# Or create a new variable based on aggregation results (really cool!)
# Create a new variable called gender.reliavrg, which is the average reliability rating for the current gender
spssDataLab[, gender.reliavrg := mean(reliavrg, na.rm = TRUE), by = sex] # avg rating for female
# Think about how you can do this with built-in R functions
seq(from = 1, to = nrow(spssDataLab), by = 2) # by=2 means every 2 observations

    
# Quick introduction to merge
# Also check https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html
dt1 = data.table(ID = 1:10, value1 = rnorm(10))
dt2 = data.table(ID = 1:5,  value2 = rnorm(5))
dt2[dt1, on = "ID"] # merge(dt1, dt2, all = TRUE)
dt2[dt1, nomatch = 0, on = "ID"] # merge(dt2, dt1, all = FALSE)
dt2[dt1, on = "ID"] # merge(dt1, dt2, all.x = TRUE)
dt1[dt2, on = "ID"] # merge(dt1, dt2, all.y = TRUE)

