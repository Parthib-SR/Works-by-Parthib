#We are considering the LIFE EXPECTANCY DATA for our analysis and the target variable has been specified.
#Importing the raw data
##Target variable is life expectancy
LifeExpectancyData=read.csv('D:/Ivy/R/Life Expectancy Data.csv', na.strings = c(""," ","NA","NULL"))
#By using the function "na.strings" we are converting the 'blanks', 'extra spaces', cells containing the
#word NA or NULL in the string columns to NA
View(LifeExpectancyData)
dim(LifeExpectancyData)
class(LifeExpectancyData)
names(LifeExpectancyData)
#Exploring the dataset to obtain the descriptive statistics of the data
str(LifeExpectancyData)
#Since our TARGET VARIABLE IS CONTINUOUS, we have a REGRESSION PROBLEM here.
#Changing the required categorical variables into factor
##the categorical column is 'Status'
LifeExpectancyData$Status=as.factor(LifeExpectancyData$Status)
str(LifeExpectancyData)
#checking for missing values and treating them
View(is.na(LifeExpectancyData))
colSums(is.na(LifeExpectancyData))
##multiple columns have missing values but missing values in all columns are <30%
#So we can treat the missing values
#We will treat the missing values with the MEDIAN because median is not affected by outliers.
LifeExpectancyData$Life_Expectancy[is.na(LifeExpectancyData$Life_Expectancy)]=median(LifeExpectancyData$Life_Expectancy, na.rm = TRUE)
LifeExpectancyData$Adult_Mortality[is.na(LifeExpectancyData$Adult_Mortality)]=median(LifeExpectancyData$Adult_Mortality, na.rm = TRUE)
LifeExpectancyData$Population[is.na(LifeExpectancyData$Population)]=median(LifeExpectancyData$Population, na.rm = TRUE)
LifeExpectancyData$Thinness_1.19_Years[is.na(LifeExpectancyData$Thinness_1.19_Years)]=median(LifeExpectancyData$Thinness_1.19_Years, na.rm = TRUE)
LifeExpectancyData$Thinness_5.9_Years[is.na(LifeExpectancyData$Thinness_5.9_Years)]=median(LifeExpectancyData$Thinness_5.9_Years, na.rm = TRUE)
LifeExpectancyData$Per_Capita_GDP[is.na(LifeExpectancyData$Per_Capita_GDP)]=median(LifeExpectancyData$Per_Capita_GDP, na.rm = TRUE)
LifeExpectancyData$GDP[is.na(LifeExpectancyData$GDP)]=median(LifeExpectancyData$GDP, na.rm = TRUE)
LifeExpectancyData$Income_Composition_of_Resources[is.na(LifeExpectancyData$Income_Composition_of_Resources)]=median(LifeExpectancyData$Income_Composition_of_Resources, na.rm = TRUE)
LifeExpectancyData$Schooling[is.na(LifeExpectancyData$Schooling)]=median(LifeExpectancyData$Schooling, na.rm = TRUE)
LifeExpectancyData$Diphtheria[is.na(LifeExpectancyData$Diphtheria)]=median(LifeExpectancyData$Diphtheria, na.rm = TRUE)
LifeExpectancyData$Total_Expenditure[is.na(LifeExpectancyData$Total_Expenditure)]=median(LifeExpectancyData$Total_Expenditure, na.rm = TRUE)
LifeExpectancyData$Alcohol[is.na(LifeExpectancyData$Alcohol)]=median(LifeExpectancyData$Alcohol, na.rm = TRUE)
LifeExpectancyData$BMI[is.na(LifeExpectancyData$BMI)]=median(LifeExpectancyData$BMI, na.rm = TRUE)
LifeExpectancyData$Hepatitis_B[is.na(LifeExpectancyData$Hepatitis_B)]=median(LifeExpectancyData$Hepatitis_B, na.rm=TRUE)
LifeExpectancyData$Polio[is.na(LifeExpectancyData$Polio)]=median(LifeExpectancyData$Polio, na.rm = TRUE)
##Missing values in categorical columns are to be treated using MODE.
#Checking for outliers in each of the continuous columns and treating them
#We use winsorization method to treat the outliers.
#Life_Expectancy
boxplot(LifeExpectancyData$Life_Expectancy)
quantiles_LED_LifeExpectancy=quantile(LifeExpectancyData$Life_Expectancy, c(0.01,0.00015,0.0001,0.001,0.002,0.003,0.004,0.005,0.006,.007))
quantiles_LED_LifeExpectancy
quantiles_LED_LifeExpectancy=quantile(LifeExpectancyData$Life_Expectancy, 0.0001)
quantiles_LED_LifeExpectancy
min(LifeExpectancyData$Life_Expectancy)
LifeExpectancyData$Life_Expectancy=ifelse(LifeExpectancyData$Life_Expectancy< quantiles_LED_LifeExpectancy,
                                          quantiles_LED_LifeExpectancy, LifeExpectancyData$Life_Expectancy)
min(LifeExpectancyData$Life_Expectancy)
#Polio
boxplot(LifeExpectancyData$Polio)
quantiles_LED_Polio=quantile(LifeExpectancyData$Polio, c(0.06,0.0621,0.0615,0.016,0.0647,0.00349,0.00346,0.00351,0.00352,0.00356,0.00355))
quantiles_LED_Polio
quantiles_LED_Polio=quantile(LifeExpectancyData$Polio,0.06)
quantiles_LED_Polio
min(LifeExpectancyData$Polio)
LifeExpectancyData$Polio=ifelse(LifeExpectancyData$Polio<quantiles_LED_Polio, quantiles_LED_Polio,
                                LifeExpectancyData$Polio)
min(LifeExpectancyData$Polio)
#Hepatitis_B
boxplot(LifeExpectancyData$Hepatitis_B)
##no outlier.
#BMI
boxplot(LifeExpectancyData$BMI)
##no outlier
#Alcohol
boxplot(LifeExpectancyData$Alcohol)
#Total_Expenditure
boxplot(LifeExpectancyData$Total_Expenditure)
#Diphtheria
boxplot(LifeExpectancyData$Diphtheria)
quantiles_LED_Diphtheria=quantile(LifeExpectancyData$Diphtheria, c(0.0581,0.0621,0.0615,0.016,0.0647,0.00349,0.00346,0.00351,0.00352,0.00356,0.00355))
quantiles_LED_Diphtheria
quantiles_LED_Diphtheria=quantile(LifeExpectancyData$Diphtheria, 0.0581)
quantiles_LED_Diphtheria
min(LifeExpectancyData$Diphtheria)
LifeExpectancyData$Diphtheria=ifelse(LifeExpectancyData$Diphtheria<quantiles_LED_Diphtheria, 
                                     quantiles_LED_Diphtheria, LifeExpectancyData$Diphtheria)
min(LifeExpectancyData$Diphtheria)
#Schooling
boxplot(LifeExpectancyData$Schooling)
quantiles_LED_Schooling=quantile(LifeExpectancyData$Schooling, c(0.0001,0.0095))
quantiles_LED_Schooling
quantiles_LED_Schooling=quantile(LifeExpectancyData$Schooling, 0.0095)
quantiles_LED_Schooling
min(LifeExpectancyData$Schooling)
LifeExpectancyData$Schooling=ifelse(LifeExpectancyData$Schooling<quantiles_LED_Schooling,
                                    quantiles_LED_Schooling, LifeExpectancyData$Schooling)
min(LifeExpectancyData$Schooling)
#Income_Composition_of_Resources
boxplot(LifeExpectancyData$Income_Composition_of_Resources)
quantiles_ICR=quantile(LifeExpectancyData$Income_Composition_of_Resources, c(0.045,0.22))
quantiles_ICR
quantiles_ICR=quantile(LifeExpectancyData$Income_Composition_of_Resources, 0.045)
quantiles_ICR
LifeExpectancyData$Income_Composition_of_Resources=ifelse(LifeExpectancyData$Income_Composition_of_Resources<quantiles_ICR, quantiles_ICR,
                     LifeExpectancyData$Income_Composition_of_Resources)
min(LifeExpectancyData$Income_Composition_of_Resources)
#GDP
boxplot(LifeExpectancyData$GDP)
quantiles_LED_GDP=quantile(LifeExpectancyData$GDP, c(0.95,0.995,0.96,0.965,0.94,0.945))
quantiles_LED_GDP
quantiles_LED_GDP=quantile(LifeExpectancyData$GDP, 0.995)
quantiles_LED_GDP
max(LifeExpectancyData$GDP)
LifeExpectancyData$GDP=ifelse(LifeExpectancyData$GDP>quantiles_LED_GDP, quantiles_LED_GDP,
                         LifeExpectancyData$GDP)
max(LifeExpectancyData$GDP)
#Per_Capita_GDP
boxplot(LifeExpectancyData$Per_Capita_GDP)
quantiles_LED_PCGDP=quantile(LifeExpectancyData$Per_Capita_GDP, c(0.9999,0.9998,0.9997))
quantiles_LED_PCGDP
quantiles_LED_PCGDP=quantile(LifeExpectancyData$Per_Capita_GDP, 0.9997)
max(LifeExpectancyData$Per_Capita_GDP)
LifeExpectancyData$Per_Capita_GDP=ifelse(LifeExpectancyData$Per_Capita_GDP>quantiles_LED_PCGDP,
                                         quantiles_LED_PCGDP, LifeExpectancyData$Per_Capita_GDP)
max(LifeExpectancyData$Per_Capita_GDP)
#Thinness_5.9_Years
boxplot(LifeExpectancyData$Thinness_5.9_Years)
quantiles_LED_Thinnes5.9=quantile(LifeExpectancyData$Thinness_5.9_Years, c(0.75,0.85,0.992))
quantiles_LED_Thinnes5.9
quantiles_LED_Thinnes5.9=quantile(LifeExpectancyData$Thinness_5.9_Years, 0.992)
LifeExpectancyData$Thinness_5.9_Years=ifelse(LifeExpectancyData$Thinness_5.9_Years>quantiles_LED_Thinnes5.9,
                                             quantiles_LED_Thinnes5.9, LifeExpectancyData$Thinness_5.9_Years)
max(LifeExpectancyData$Thinness_5.9_Years)
#Thinness_1.19_Years
boxplot(LifeExpectancyData$Thinness_1.19_Years)
quantiles_LED_Thinness_1.19_Years=quantile(LifeExpectancyData$Thinness_1.19_Years, c(0.75,0.85,0.992,0.993,0.994,0.9941,0.9942,0.9943,0.9944,0.9945,
                                                                                     0.9946,0.9948,0.9947,0.995))
quantiles_LED_Thinness_1.19_Years=quantile(LifeExpectancyData$Thinness_1.19_Years, 0.9946)
max(LifeExpectancyData$Thinness_1.19_Years)
LifeExpectancyData$Thinness_1.19_Years=ifelse(LifeExpectancyData$Thinness_1.19_Years>quantiles_LED_Thinness_1.19_Years,
                                              quantiles_LED_Thinness_1.19_Years, LifeExpectancyData$Thinness_1.19_Years)
max(LifeExpectancyData$Thinness_1.19_Years)
#Population
par(mfrow=c(1,1))
boxplot(LifeExpectancyData$Population)
quantiles_LED_Population=quantile(LifeExpectancyData$Population, c(0.75,0.85,0.9,0.93,0.931,0.932,
                                                                   0.935,0.948,0.95,0.96,0.995,0.97,
                                                                   0.98,0.9892,0.99))
quantiles_LED_Population=quantile(LifeExpectancyData$Population, 0.9892)
LifeExpectancyData$Population=ifelse(LifeExpectancyData$Population>quantiles_LED_Population, quantiles_LED_Population,
                                     LifeExpectancyData$Population)
max(LifeExpectancyData$Population)
#Adult_Mortality
boxplot(LifeExpectancyData$Adult_Mortality)
##no outlier 
#Infant_Deaths
boxplot(LifeExpectancyData$Infant_Deaths)
quantiles_LED_InfantDeaths=quantile(LifeExpectancyData$Infant_Deaths, c(0.99,0.9955,0.9946,0.9927))
quantiles_LED_InfantDeaths
quantiles_LED_InfantDeaths=quantile(LifeExpectancyData$Infant_Deaths, 0.9946)
LifeExpectancyData$Infant_Deaths=ifelse(LifeExpectancyData$Infant_Deaths>quantiles_LED_InfantDeaths,
                                        quantiles_LED_InfantDeaths, LifeExpectancyData$Infant_Deaths)
max(LifeExpectancyData$Infant_Deaths)
#Plotting the histograms
#Plotting histograms for the continuous columns will help us understand the distribution of the column
library(RColorBrewer)
##RColorBrewer is a library that generates professional colors
##We will run a for loop to obtain the histograms of the desired columns at once.
ColsForHistogram=c("Adult_Mortality","Population","Infant_Deaths","Thinness_1.19_Years","Thinness_5.9_Years","Per_Capita_GDP",
                   "GDP","Income_Composition_of_Resources","Schooling","Diphtheria","Total_Expenditure",
                   "Alcohol","BMI","Polio","Hepatitis_B")
ColsForHistogram
##We are now splitting the plot window into 4 rows and 4 columns
par(mfrow=c(4,4))
for (hist_cols in ColsForHistogram){
  hist(LifeExpectancyData[,c(hist_cols)], main=paste('Histogram for ',hist_cols), col=brewer.pal(12, "Set3"))
}
#Plotting for bar graphs
#We are plotting the bar plots for categorical columns.
par(mfrow=c(1,1))
barplot(table(LifeExpectancyData$Status), beside = T, main=paste('Barplot for Status'), col=brewer.pal(3,"Dark2"))

## BIVARRIATE ANALYSIS
#We will now check the strength of the relationship between the target variable and the predictors
#We will find correlation coefficient to check the strength of relationship between two continuous columns
#finding correlation coefficients
ColsForCorr=c("Life_Expectancy","Infant_Deaths","Adult_Mortality","Population","Thinness_1.19_Years","Thinness_5.9_Years","Per_Capita_GDP",
              "GDP","Income_Composition_of_Resources","Schooling","Diphtheria","Total_Expenditure",
              "Alcohol","BMI","Polio","Hepatitis_B")
CorrLED=cor(LifeExpectancyData[,ColsForCorr], use = "complete.obs")
## use="complete.obs" means we only the columns with no missing values are considered
CorrLED
##Reducing the threshold for correlation coefficient to 0.2
##Obtaining the columns which have correlation coefficient higher than 0.5
names(CorrLED[,'Life_Expectancy'][abs(CorrLED[,'Life_Expectancy'])>0.5])

##We now check the strength of the relationship between the continuous and categorical columns
#We use ANOVA (Analysis of Variance)
#The NULL HYPOTHESIS is H0: the variables are NOT CORRELATED
#We are doing ANOVA of LIFE EXPECTANCY based on STATUS
summary(aov(Life_Expectancy~Status, data=LifeExpectancyData))
##Status is highly significant for our analysis

#Generating Data Frame for ML
#Extractig Target and Predictor Variables from the data to create generic dataset
TargetVariableName=c('Life_Expectancy')
BestPredictorName=c("Adult_Mortality","Infant_Deaths","Population","Thinness_1.19_Years","Thinness_5.9_Years","Per_Capita_GDP",
              "GDP","Income_Composition_of_Resources","Schooling","Diphtheria","Total_Expenditure",
              "Alcohol","BMI","Polio","Hepatitis_B","Status")
TargetVariable=LifeExpectancyData[,c(TargetVariableName)]
str(TargetVariable)
PredictorVariableName=LifeExpectancyData[,c(BestPredictorName)]
str(PredictorVariableName)
DataForML=data.frame(TargetVariable,PredictorVariableName)
head(DataForML)

#SAMPLING
#Splitting 70% data for training and 30% for testing
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
#We will use the rows that are not used for the training
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

head(DataForMLTrain)
head(DataForMLTest)

#Creating predictive model for checking the accuracy
#Linear regression
startTime=Sys.time()
LinearModel_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(LinearModel_Reg)
endTime=Sys.time()
endTime-startTime

#Dropping the variables which are not significant
#We will drop the insignificant variables one at a time
LinearModel_Reg2=lm(TargetVariable~.-Thinness_5.9_Years,data=DataForMLTrain)
summary(LinearModel_Reg2)
##Droping Thinness_5.9_Years increased the adjusted r-squared
LinearModel_Reg3=lm(TargetVariable~.-Thinness_5.9_Years-Total_Expenditure,data=DataForMLTrain)
summary(LinearModel_Reg3)
##Dropping Total Expenditure raised the value of adjusted r=squared
LinearModel_Reg4=lm(TargetVariable~.-Thinness_5.9_Years-Total_Expenditure-Alcohol,
                    data=DataForMLTrain)
summary(LinearModel_Reg4)
##all variables are now significant
##All the variables are significant in our model now
#testing for MULTICOLLINEARITY
library(car)
VIF=vif(LinearModel_Reg4)
VIF
#VIF for Population is >5. So we drop Population and run the regression again.
LinearModel_Reg5=lm(TargetVariable~.-Thinness_5.9_Years-Total_Expenditure-Alcohol-Population,
                    data=DataForMLTrain)
summary(LinearModel_Reg5)
VIF_2=vif(LinearModel_Reg5)
VIF_2
#The variables Thinness_1.19_Years and Per_Capita_GDP are insignificant. so, we drop them.
LinearModel_Reg6=lm(TargetVariable~.-Thinness_5.9_Years-Total_Expenditure-Alcohol-Population
                    -Thinness_1.19_Years-Per_Capita_GDP,
                    data=DataForMLTrain)
summary(LinearModel_Reg6)
#All the variables are now highly significant.
VIF3=vif(LinearModel_Reg6)
VIF3
#No variable has a VIF higher than 5. So, there is no multicollinearity.
#Testing for HETEROSCEDASTICITY
#We perform the Breusch-Pagan test for the above
##Null hypothesis: No heteroscedasticity in the model
library(lmtest)
bptest(LinearModel_Reg6)
##Heteroscedasticity is present because p<0.05
#Testing for autocorrelation
#We perform the Durbin-Watson test
##Null hypothesis: no autocorrelation
dwtest(LinearModel_Reg6)
##no autocorrelation because p=0.1157
#Testing for normality
#We perform the Anderson-Darling test
##Null hypothesis: errors are normally distributed
library(nortest)
ad.test(LinearModel_Reg6$residuals)
##errors are not normally distributed
#Checking ACCURACY of the model on test data
DataForMLTest$Pred_LM=predict(LinearModel_Reg6, DataForMLTest)
head(DataForMLTest)
#Calculating APE for each prediction
DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)
MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('# Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('# Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

