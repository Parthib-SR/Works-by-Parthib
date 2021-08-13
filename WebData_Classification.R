#importing the dataset
#target variable is 'Cliced'
WebData=read.csv('D:/Ivy/R/Web_data.csv', na.strings =c(""," ","NA","NULL"))
View(WebData)                 
dim(WebData)
class(WebData)
names(WebData)

#exploring the dataset
str(WebData)
head(WebData,10)
summary(WebData)
unique(WebData[c("Country_Name")])
length(WebData$Country_Name)
length(WebData$City_code)
#categorical columns ad topic, weekday, male, city code, country, time period, month,clicked
#converting the categorical columns into factors
factor_cols=c("Ad_Topic","City_code","Male","Time_Period","Weekday","Month","Clicked","Country_Name")
for (categorical_col in factor_cols) {
  WebData[, categorical_col]=as.factor(WebData[, categorical_col])
}
str(WebData)

#chekcing for missing values
colSums(is.na(WebData))
##there are no missing values in the data

#plotting histogram for the continuous columns
library(RColorBrewer)
ColsForHist=c("Age","Avg_Income","Internet_Usage","Time_Spent")
par(mfrow=c(2,2))
##converting the continuous columns into int
for (int_col in ColsForHist) {
  WebData[, int_col]=as.integer(WebData[, int_col])
}
str(WebData)
for (ColumnName in ColsForHist){
  hist(WebData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Dark2"))
}
#plotting bar graphs for categorical columns
ColsForBar=c("Month","Weekday","Time_Period","Male","City_code","Ad_Topic")
par(mfrow=c(3,2))
for (ColumnName in ColsForBar){
  barplot(table(WebData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}
par(mfrow=c(3,2))
#Visualising relationship between target variable and the predictor
##We will use boxplot to visualize the relationship between a continuous and a categorical column
boxplot(Time_Spent~Clicked, data = WebData, col=brewer.pal(8, "Dark2"))
boxplot(Age~Clicked, data=WebData, col=brewer.pal(8, "Dark2"))
boxplot(Avg_Income~Clicked, data=WebData, col=brewer.pal(8, "Dark2"))
boxplot(Internet_Usage~Clicked, data=WebData, col=brewer.pal(8, "Dark2"))

##We will use multiple barplot to visualize the relation between two categorical columns
CrossTabResult1=table(WebData[, c('Clicked','Time_Period')])
CrossTabResult1
barplot(CrossTabResult1, beside = T, col=brewer.pal(12,"Paired"))
CrossTabResult2=table(WebData[,c('Clicked','Male')])
CrossTabResult2
barplot(CrossTabResult2, beside=T, col=brewer.pal(8,"Dark2"))
CrossTabResult3=table(WebData[,c('Clicked','Weekday')])
CrossTabResult3
barplot(CrossTabResult3, beside=T, main=paste('Barplot for: ', 'Weekday'), col=brewer.pal(8,"Dark2"))
CrossTabResult4=table(WebData[,c('Clicked','Month')])
CrossTabResult4
barplot(CrossTabResult4, beside=T, main=paste('Barplot for: ', 'Month'), col=brewer.pal(8, "Dark2"))
CrossTabResult5=table(WebData[,c('Clicked','Ad_Topic')])
CrossTabResult5
barplot(CrossTabResult5, beside=T, main=paste('Barplot for: ','Ad_Topic'), col=brewer.pal(12,"Paired"))
CrossTabResult6=table(WebData[,c('Clicked','City_code')])
CrossTabResult6
barplot(CrossTabResult6, beside=T, main=paste('Barplot for:', 'City Code'), col=brewer.pal(9,"Pastel1"))

#statistical relationship between the predictors and the target variable
##We will use ANOVA for categorical and continuous columns
##The null hypothesis is H0:variables are NOT correlated
summary(aov(Age~Clicked, data=WebData))
summary(aov(Time_Spent~Clicked, data=WebData))
summary(aov(Avg_Income~Clicked, data=WebData))
summary(aov(Internet_Usage~Clicked, data=WebData))

##We will use chi-squared test in case of 2 categorical columns
##The null hypothesis is H0:variables are NOT correlated
chisq.test(CrossTabResult1)
chisq.test(CrossTabResult2)
chisq.test(CrossTabResult3)
chisq.test(CrossTabResult4)
chisq.test(CrossTabResult5)
chisq.test(CrossTabResult6)

InputData=WebData
#specifying the target variable
TargetVariableName='Clicked'
##Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)
PredictorVariables=InputData[, !names(InputData) %in% TargetVariableName]
str(PredictorVariables)
DataForML=data.frame(TargetVariable,PredictorVariables)
str(DataForML)
head(DataForML)

#splitting the data for training and testing for ML
##We wll use 70% of the data for training and the rest for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

#creating the logistic regression model
startTime=Sys.time()
LogisticReg_Model=glm(TargetVariable ~ .-Year , data=DataForMLTrain, family='binomial')

summary(LogisticReg_Model)
library(car)
vif(LogisticReg_Model)
endTime=Sys.time()
endTime-startTime
LogisticReg_Model_2=glm(TargetVariable ~ .-Country_Name-Year, data=DataForMLTrain, family='binomial')
summary(LogisticReg_Model_2)
vif(LogisticReg_Model_2)
LogisticReg_Model_3=glm(TargetVariable ~ .-Country_Name-VistID-Year, data=DataForMLTrain, family='binomial')
summary(LogisticReg_Model_3)
vif(LogisticReg_Model_3)
nrow(DataForMLTrain)

#checking the model accuracy
PredictionProb=predict(LogisticReg_Model_3,DataForMLTest,type = "response")
PredictionProb

#considering a threshold of 0.55
DataForMLTest$Prediction=ifelse(PredictionProb>0.55, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)

#Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
#install.packages('e1071', dependencies=TRUE)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]
print(paste('Overall Accuracy of Logistic Reg Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))
