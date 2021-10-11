
rm(list=ls()) #to clear the workspace
#All the required packages are listed below. Do not install them if you already have done it.
#install.packages("corrplot")
library("corrplot")
#install.packages("scales")
library("scales")
#install.packages("rpart")
library(rpart) # decision tree
#install.packages("rpart.plot")
library(rpart.plot) # decision tree visualization
#install.packages("caTools")
library(caTools) # data split
#install.packages("caTools")
#install.packages("class")
library(class) # KNN
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library("ggplot2")
#install.packages("ggcorrplot")
library("ggcorrplot")
#install.packages("GGally")
library("GGally")
#install.packages("cowplot")
library("cowplot")


#part1 of the assignment


#I recommend you to set the working directory before running the script



#loading the data
data <- read.csv("Housing_data.csv", header = TRUE, sep = ",")
#descriptive statistic to our data
View(data)
str(data)

#plots for each variable


for (i in colnames(data)) {
  p <- ggplot(data, aes(x=data[, i]))+
    geom_histogram()+
    xlab(i)
  print(p)
}

#summary statistics for each variable
for (x in colnames(data)) {
  print(x)
  x = data[, x]
  print(summary(x))
  
}



#correlation analysis
correlation <- cor(data)
ggcorrplot(correlation)
corrplot(correlation)

#linear regression
linear_regression <- lm(MEDV ~ PCCR + PRLZ + INDUS + NOX +AVR + AGE + DIS + RAD + TAX, data = data)
summary(linear_regression)

#model without INDUS variable

linear_regression2 <- lm(MEDV ~ PCCR + PRLZ + NOX +AVR + AGE + DIS + RAD + TAX + 0, data = data)
summary(linear_regression2)
residuals <- resid(linear_regression2)
fittedvalues <- fitted(linear_regression2)

#adding residuals to our original data frame

data$residuals <- residuals
data$fittedvalues <- fittedvalues

#residual vs fitted plots
par(mfrow = c(2,2))
plot(linear_regression2)
resfit <- ggplot(data, aes(x = fitted(data) , y = resid(data) )) + 
  geom_point(size = 1.5) +
  geom_smooth(aes(colour = fitted(data), fill = fitted(data))) +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(title = "Actual vs Fitted values")
resfit + geom_line(aes(y = max(resid(data))), color = "red", linetype = "dashed")+
  geom_line(aes(y = min(resid(data))), color = "red", linetype = "dashed")


#plot to question 8


blotti <- ggplot(data, aes(x = 1:length(MEDV), y = MEDV))+
  geom_point(col = "blue")+
  geom_line(col="blue")+
  geom_point(aes(x=1:length(MEDV), y=fittedvalues), col = "red")+
  geom_line(aes(x=1:length(MEDV), y=fittedvalues), col = "red")
blotti + geom_line(aes(y = max(MEDV)), color = "red", linetype = "dashed")+
  geom_line(aes(y = min(MEDV)), color = "red", linetype = "dashed")+
  xlab("Observations")+
  ylab("Prices")+
  ggtitle("Actual = Blue, Fitted = Red")

#histogram of residuals

plot1 <- ggplot(data, aes(x = residuals)) + geom_histogram(color = "darkblue", fill="lightblue",linetype="dashed")
plot2 <- ggplot(data, aes(x=residuals))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

plot_grid(plot1, plot2)  


#shapiro-wilk test for normality on residuals

shapiro.test(data$residuals)




#part2


#loading the data from our working directory
dataa <- read.csv("Thyroid_data.csv", header = TRUE, sep = ",")

#checking for missing values

any(is.na(dataa))

#plotting columns & histograms
for (j in colnames(dataa)) {
  p <- ggplot(dataa, aes(x=dataa[, j]))+
    geom_histogram()+
    xlab(j)
  print(p)
}

#summary statistics for each variable
for (z in colnames(dataa)) {
  print(z)
  x = dataa[, z]
  print(summary(z))
}  

#scaling the values between zero and one and normalizing it using min max method
CLASS_new <- rescale(dataa$CLASS, to = c(0,1))
t3_new <- rescale(dataa$T3, to = c(0,1))
TST_new <- rescale(dataa$TST, to = c(0,1))
TSTR_new <- rescale(dataa$TSTR, to = c(0,1))
TSH_new <- rescale(dataa$TSH, to = c(0,1))
MAD_TSH_new <- rescale(dataa$MAD.TSH, to = c(0,1))

#formatting scaled attributes to a dataframe
new_data <- data.frame(CLASS_new, t3_new, TST_new, TSTR_new, TSH_new, MAD_TSH_new)

#formatting class_new attribute to a factor datatype
new_data$CLASS_new <- as.factor(new_data$CLASS_new)


#splitting data into a training and testing set with splitratio of 0.7
sample=sample.split(new_data$CLASS_new,SplitRatio=0.7)
train=subset(new_data,sample==TRUE)
test=subset(new_data,sample==FALSE)

#decisiontree

dectree=rpart(CLASS_new~.,data=train,method="class")

dectree
dectree2=rpart(CLASS_new~.,data=test,method="class")
rpart.plot(dectree)
treeTrain=predict(dectree,train,type="class")
tab <- table(train$CLASS_new, treeTrain)
tab
#this calculates the accurate rate from the confusion matrix
sum(diag(tab)) /sum(tab)
treeTest=predict(dectree,test,type="class")
treeTest
tab2 <- table(test$CLASS_new, treeTest)
tab2
#this calculates the accurate rate from the confusion matrix
sum(diag(tab2)) /sum(tab2)
rpart.rules(dectree)


#Optimal size of leaves is calculated by iterating the error rates via
#trained & tested data

#creating empty vectors
iteraatio <-vector()
vektori <- vector()

for(i in 1:50) {
  treemdl1=rpart(train$CLASS_new~.,data=train,method="class",minbucket=i)
  treeTrain=predict(treemdl1,train,type="class")
  table1 <-table(train$CLASS_new, treeTrain)
  vektori[i] <- 1-(sum(diag(table1))/sum(table1))
  iteraatio[i] <- i
  print(vektori[i])
    
}
print(data.frame(vektori,iteraatio))

vektori2 <- vector()
iteraatio2 <- vector()
#this line of code also prints the iterations vs error rates so we can see how the error rates arises
for(i in 1:50) {
  treem=rpart(CLASS_new~.,data=train,method="class",minbucket=i)
  treeT=predict(treem,test,type="class")
  table1 <-table(test$CLASS_new, treeT)
  vektori2[i] <- 1-(sum(diag(table1))/sum(table1))
  iteraatio2[i] <- i
  print(vektori2[i])
  
}

print(data.frame(vektori2, iteraatio2))
iteraatio
vektori
vektori2
iteraatio2
datafreimi <-data.frame(vektori,vektori2,iteraatio, iteraatio2)


#plotting test vs train predicitons against error rates
g <- ggplot(data = datafreimi, aes(x=iteraatio, y= vektori))+
  geom_line(aes(x=iteraatio, y=vektori), col="blue", size=2)+
  ylab("Error rate")+
  xlab("Number of leaves")+
  ggtitle("Red = test prediction, Blue = Train prediction")
  
  
g+geom_line(aes(x=iteraatio2, y=vektori2), col="red", size=2)



#KNN classifier

sample=sample.split(new_data$CLASS_new,SplitRatio=0.7)
train=subset(new_data,sample==TRUE)
test=subset(new_data,sample==FALSE)


trainpred = knn(train[,1:2], train[,1:2], factor(train$CLASS), 10)
testpred = knn(train[,1:2], test[,1:2], factor(train$CLASS), 10)


#looking at the error(1-accuracy)
KNNTrain=1-mean(train$CLASS_new==trainpred)
KNNTest=1-mean(test$CLASS_new==testpred)
#construct a loop to see how the number of neighbors
#impacts the training and test performance

KNNTrain=matrix(0, nrow=50, ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)
#we will use cross validation to split the data in every iteration so the training data will not overfit or be too noisy

for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(new_data$CLASS_new, SplitRatio = 0.7)
    train=subset(new_data, sample==TRUE)
    test=subset(new_data,sample==FALSE)
    trainpred = knn(train[,2:6], train[,2:6],factor(train$CLASS_new),i)
    testpred = knn(train[,2:6], test[,2:6], factor(train$CLASS_new),i)
    KNNTrain[i,j]=1-mean(train$CLASS_new==trainpred)
    KNNTest[i,j]=1-mean(test$CLASS_new==testpred)
  }
}

KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)

#printing the results
results=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
print(results)
ggplot(results,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')




#lets redo the KNN classification for the original data without scaling

#lets use the original data frame
originaldata <- dataa
KNNTrain=matrix(0, nrow=50, ncol=200)
KNNTest=matrix(0,nrow=50,ncol=200)
for(i in 1:50){
  for(j in 1:200){
    sample=sample.split(originaldata$CLASS, SplitRatio = 0.7)
    train=subset(originaldata, sample==TRUE)
    test=subset(originaldata,sample==FALSE)
    trainpred = knn(train[,2:6], train[,2:6],factor(train$CLASS),i)
    testpred = knn(train[,2:6], test[,2:6], factor(train$CLASS),i)
    KNNTrain[i,j]=1-mean(train$CLASS==trainpred)
    KNNTest[i,j]=1-mean(test$CLASS==testpred)
  }
}

KNNTrain=rowMeans(KNNTrain)
KNNTest=rowMeans(KNNTest)

#printing the results
resultsoriginal_Dataframe=data.frame(neighbors=1:50,train_perf=KNNTrain, test_perf=KNNTest)
resultsoriginal_Dataframe
ggplot(resultsoriginal_Dataframe,aes(x=neighbors, y=train_perf))+
  geom_line(col='blue')+
  geom_line(aes(x=neighbors, y=test_perf),col='red')




