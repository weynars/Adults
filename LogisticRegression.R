rm(list = ls())

#LOAD DATA

path <- "C:/Users/lucas/OneDrive/Lucas/Programming/R/Algorithms/Logistic Regression/Salary"
setwd(path) #set path
# read the column names file
col_names <- fread("col_name.csv",sep=",",header=FALSE)
col_names <- col_names$V1
# load the CSV file from the local directory
filename <- "adults.csv" # define the filename
library(data.table) # Library for the fread function
dataset <- fread(filename,sep=",",header=FALSE,na.strings=c("?"),	
	stringsAsFactors=T, col.names=col_names)

#DATA CHARACTERISTICS

# dimensions of dataset
dim(dataset)
# struct of dataset
str(dataset)
# variable classes
sapply(dataset, class)
# list levels for each factor
sapply(Filter(is.factor,dataset), levels)
# take a peek at the first 5 rows of the data
head(dataset)
# summarized information
summary(dataset)

# summarize the class distribution
percentage <- prop.table(table(dataset$salary)) * 100
cbind(freq=table(dataset$salary), percentage=percentage)

#DATA CLEANING

#Based on the data description fnlwgt and education_num columns can be removed
dataset <- dataset[,-c(3,5)]

#Checking the NA values and removing them
summary(dataset)

attach(dataset) # attaching the columns to the working directory
#Checking by column
prop.table(table(is.na(workclass),salary),1)
prop.table(table(is.na(occupation),salary),1)
table(is.na(workclass),is.na(occupation))
prop.table(table(is.na(native_country),salary),1)
detach(dataset)

#Checking complete rows on the dataset
prop.table(table(complete.cases(dataset)))

# number of test rows will be left after cleaning
test.rows <- table(complete.cases(dataset[(nrow(dataset)-16280):nrow(dataset)]))[[2]]

#Cleaning
dataset <- dataset[complete.cases(dataset),]


#DATA EXPLORATION

#Factors - Table Vizualization
factors.dt <- Filter(is.factor, dataset)

#Numerical
numeric.dt <- Filter(is.numeric, dataset)

library(caret)
samp <- 5000
x <- numeric.dt[1:samp]
y <- dataset$salary[1:samp]
# scatterplot matrix
featurePlot(x=x, y=y, plot="pairs", 
   key=list(title="Scatter Plot",
   columns=2,
   points=list(pch=1,col=c("blue","magenta")),
   text=list(c("<=50K",">50K"))))
scales <- list(x=list(relation="free"), y=list(relation="free"))
# box plots for each attribute
featurePlot(x=x, y=y, plot="box", scales=scales)
# density plots for each attribute by class value
featurePlot(x=x, y=y, plot="density", scales=scales)

# Scatterplot Matrices from the lattice Package 
library(lattice)
splom(x, groups=y)

# boxplot for each attribute on one image
par(mfrow=c(2,2))
  for(i in 1:4) {
  boxplot(numeric.dt[[i]]~dataset$salary, main=names(numeric.dt)[i])
}

#Histogram
par(mfrow=c(2,2))  # divide graph area 
for(i in 1:4) {
  hist(numeric.dt[[i]], main=names(numeric.dt)[i])
}
#Density Plot
par(mfrow=c(2,2))  # divide graph area
for(i in 1:4) {
  plot(density(numeric.dt[[i]]), main=names(numeric.dt)[i])
}

lowIncome <- dataset$salary=="<=50K"
#Histogram
par(mfrow=c(4,2))  # divide graph area 
for(i in 1:4) {
  hist(numeric.dt[[i]][lowIncome], main=c(names(numeric.dt)[i]," <=50K"), breaks=50)
  hist(numeric.dt[[i]][!lowIncome], main=c(names(numeric.dt)[i]," >50K"), breaks=50)
}
#Density Plot
par(mfrow=c(4,2))  # divide graph area
for(i in 1:4) {
  plot(density(numeric.dt[[i]][lowIncome]), main=c(names(numeric.dt)[i]," <=50K"))
  plot(density(numeric.dt[[i]][!lowIncome]), main=c(names(numeric.dt)[i]," >50K"))
}

#NUMERICAL VARIABLES TO FACTORS

#Age
dataset$age <- cut(dataset$age, breaks=c(-Inf,25,35,60,+Inf),labels=c("young","young_adults","adults","old"))
tab <- table(dataset$age,dataset$salary)
barplot(tab,beside=TRUE, col=topo.colors(nrow(tab)), legend = rownames(tab))

#Capital Gain
nonzerocapitalgain <- subset(dataset, capital_gain!=0)
summary(nonzerocapitalgain$capital_gain)
capgainmed <- median(nonzerocapitalgain$capital_gain)
dataset$capital_gain <- cut(dataset$capital_gain, breaks=c(-Inf,0,capgainmed,+Inf),labels=c("Zero","low","high"))
tab <- table(dataset$capital_gain,dataset$salary)
barplot(tab,beside=TRUE, col=topo.colors(nrow(tab)), legend = rownames(tab))

#Capital Loss
nonzerocapitalloss <- subset(dataset, capital_loss!=0)
summary(nonzerocapitalloss$capital_loss)
caplossmed <- quantile(nonzerocapitalloss$capital_loss,0.25)
dataset$capital_loss <- cut(dataset$capital_loss, breaks=c(-Inf,0,caplossmed,+Inf),labels=c("Zero","low","high"))
tab <- table(dataset$capital_loss,dataset$salary)
barplot(tab,beside=TRUE, col=topo.colors(nrow(tab)), legend = rownames(tab))

#Hours Per Week
boxplot(dataset$hours_per_week~dataset$salary)
dataset$hours_per_week <- cut(dataset$hours_per_week, breaks=c(-Inf,39,41,+Inf),labels=c("Lazy","Normal","Workaholic"))
tab <- table(dataset$hours_per_week,dataset$salary)
barplot(tab,beside=TRUE, col=topo.colors(nrow(tab)), legend = rownames(tab))

#PLOT THE MAP

filename <- "countries.csv" # define the filename
countries <- fread(filename,sep=",",header=FALSE,stringsAsFactors=F)
library(plyr)
dataset$native_country <- revalue(dataset$native_country, 
	c("Holand-Netherlands"="Netherlands","Outlying-US(Guam-USVI-etc)"="Guam","Hong"="Hong Kong"))

library(RecordLinkage)
ClosestMatch = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
}
find.code <- function(x)countries$V1[countries$V2==ClosestMatch(x, countries$V2)]
levels(dataset$native_country) <- sapply(levels(dataset$native_country),find.code)
levels(dataset$native_country)

library(rworldmap)

tab <- prop.table(table(dataset$native_country, dataset$salary),1)[,2] # The perceentage of low income and high income for each country
countriesDF <- data.frame(country=names(tab),probability=tab) # data.frame with the ISO 3 country names plus a variable to merge to the map data
# This will join your countriesDF data.frame to the country map data
countriesMap <- joinCountryData2Map(countriesDF , joinCode="ISO3", nameJoinColumn="country") 
# Plot the map
mapCountryData(countriesMap, nameColumnToPlot="probability", colourPalette="heat", missingCountryCol = gray(.8), oceanCol="lightblue",mapTitle="Probability of High Income Map")

#GROUP FACTORS

# Show the levels distributions based on the salary
for (i in 1:(ncol(factors.dt)-1)) {
	print(table(factors.dt[[i]],dataset$salary))
}

# Show the levels probability distributions based on the salary
for (i in 1:(ncol(factors.dt)-1)) {
	print(prop.table(table(factors.dt[[i]],dataset$salary),1))
}

grouping <- function (p1,p2,df,cat){
   probs <- prop.table(table(df,dataset$salary),1)[,2] # Get the probability of having high income for each level
   ld <- names(which(probs >= p1 & probs < p2)) # Get the name of the levels that have the probability inside the stipulated interval
   levels(df)[levels(df) %in% ld] <- cat # Group the levels into a new level
   df # return the new data
}

#Work Class
prop.table(table(dataset$workclass,dataset$salary),1)[,2]
levels(dataset$workclass)[c(1,2,7)] <- "Gov"
levels(dataset$workclass)[c(2,6)] <- "Unemployed"
levels(dataset$workclass)[c(4,5)] <- "Self"

#Education
prop.table(table(dataset$education,dataset$salary),1)[,2]
dataset$education <- grouping(0,0.1,dataset$education,"low-education")
dataset$education <- grouping(0.1,0.3,dataset$education,"mid-education")
dataset$education <- grouping(0.3,1,dataset$education,"high-education")

#Marital Status
prop.table(table(dataset$marital_status,dataset$salary),1)[,2]
dataset$marital_status <- grouping(0,0.2,dataset$marital_status,"not-married")
dataset$marital_status <- grouping(0.2,1,dataset$marital_status,"married")

#Occupation
prop.table(table(dataset$occupation,dataset$salary),1)[,2]
dataset$occupation <- grouping(0,0.15,dataset$occupation,"bad-job")
dataset$occupation <- grouping(0.15,0.3,dataset$occupation,"mid-job")
dataset$occupation <- grouping(0.3,1,dataset$occupation,"good-job")

#Relationship
prop.table(table(dataset$relationship,dataset$salary),1)[,2]
dataset$relationship <- grouping(0,0.2,dataset$relationship,"not-married")
dataset$relationship <- grouping(0.2,1,dataset$relationship,"married")

#Race
prop.table(table(dataset$race,dataset$salary),1)[,2]
dataset$race <- grouping(0,0.25,dataset$race,"other")
dataset$race <- grouping(0.25,1,dataset$race,"white-asian")

#Native Country
prop.table(table(dataset$native_country,dataset$salary),1)[,2]
dataset$native_country<- grouping(0,0.1,dataset$native_country,"low-country")
dataset$native_country<- grouping(0.1,0.3,dataset$native_country,"mid-country")
dataset$native_country<- grouping(0.3,1,dataset$native_country,"high-country")

#TRAINING AND VALIDATION PARTITION

rows <- nrow(dataset)
training <- dataset[1:(rows-test.rows)]
validation <- dataset[(rows-test.rows+1):rows]

#Checking proportions
percentage <- prop.table(table(training$salary)) * 100
cbind(freq=table(training$salary), percentage=percentage)
percentage <- prop.table(table(validation$salary)) * 100
cbind(freq=table(validation$salary), percentage=percentage)

#LOGISTIC REGRESSION
model <- glm(salary~., data=training, family=binomial)
summary(model)

#Predicting validation
x_validation <- validation[,1:(ncol(validation)-1)]
y_validation <- validation$salary
predict <- predict(model, x_validation, type="response")
#confusion matrix
tab <- table(ifelse(predict>0.4,1,0), ifelse(validation$salary=="<=50K",0,1))
confusionMatrix(tab)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, validation$salary)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))