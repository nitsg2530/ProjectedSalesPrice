#******************Housing Price estimation Project*****
# Submitted by : Nitin Kumar Garg
# ID: 2508212
# email: 2508212G@student.gla.ac.uk
# Date of submittion: 20.03.2020
#*******************************************************


# loading the required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(readr)
require(dplyr)
require(ggplot2)
require(GGally)
library(olsrr)
library(ggfortify)

#loading the CSV to envirnment, makesure "housing.csv" and source code R files are at the working directory 

housing <- read.csv("housing.csv")
head(housing)
summary(housing)


#*********Remove Outliers
print("Housing records before removing Outliers..")
nrow(housing)
housing$bath<-as.factor(housing$bath)
# housing <- mutate(housing, perSq_price = price/sqft)
result <- by(housing,housing$bath,function(housing) {
  qnt <- quantile(housing$price, probs=c(.25, .75))
  H <- 1.5 * IQR(housing$price)
  outlierCheck <- (housing$price) > qnt[1]-H & (housing$price<qnt[2]+H)
  noOutliers <- housing[outlierCheck,]
})

housing <- do.call("rbind",result)

ggplot(housing, aes(x=bath, y=price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)


result <- by(housing,housing$parking,function(housing) {
  qnt <- quantile(housing$price, probs=c(.25, .75))
  H <- 1.5 * IQR(housing$price)
  outlierCheck <- (housing$price) > qnt[1]-H & (housing$price<qnt[2]+H)
  noOutliers <- housing[outlierCheck,]
})

housing <- do.call("rbind",result)

ggplot(housing, aes(x=parking, y=price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
#converting bath back to a numeric variable, as it is a numeric represantation 
#of number of bath in the house
housing$bath<-as.numeric(housing$bath)
print("Housing records after removing Outliers..")
nrow(housing)


#*********Remove parking values "not provded" from data set 

# housing <- filter(housing,parking != "Not Provided")
# 
# print("Housing records after removing parking == Not Provided..")
# nrow(housing)


#*********Save Test Records

#Remove a few observatios that we will use as test data
housing.test <- housing[c(6,70,175,290,325,360),]

#Let's focus on the rest of the data this is train data
housing <- housing[-c(6,70,175,290,325,360),]


#*********Expolatory Data Analysis
pairing_col = 9

Cat<-c() 
for (i in 1:ncol(housing))
{
   if(i != pairing_col)
  {
    if (!is.factor(housing[,i])) {Cat[i]<-0}
    if (is.factor(housing[,i])) {Cat[i]<-1}
  }
}

head(housing[,which(Cat==1)])
head(housing[,which(Cat==0)])


glimpse(housing)

#Lets see some plots
ggpairs(housing,cardinality_threshold=30,diag=list(continuous="density",   discrete="bar"), axisLabels="show")
ggpairs(housing[,c(pairing_col,which(Cat==0))],cardinality_threshold=30,diag=list(continuous="density",   discrete="bar"), axisLabels="show")
ggpairs(housing[,c(pairing_col,which(Cat==1))],cardinality_threshold=30,diag=list(continuous="density",   discrete="bar"), axisLabels="show")

#Plot the response as it is
ggplot(housing, aes(x=price)) + geom_histogram(bins=50,fill="blue",color="white") +
  labs(x="House Price", y="Count")

#Plot the new transformed response
ggplot(housing, aes(x=log(price))) + geom_histogram(bins=50,fill="blue",color="white") +
  labs(x="log(House Price)", y="Count")
# the plot is a bit flatter in case of non log values of price

#try linear regression for all the variables
model0 <- lm(price~., housing)
summary(model0)
autoplot(model0,which=c(1,2,4))

#try linear regression for all the variables with log 
model1 <- lm(log(price) ~ ., housing)
summary(model1)
autoplot(model1,which=c(1,2,4))

#Let's use the transformed price variable to develop the further model

#************Predictive modeling and fine tune..
#Can we actually use this model to predict prices?
housing.test$bath<-as.numeric(housing.test$bath)
housing.test$parking<-as.factor(housing.test$parking)
#housing.test <- select(housing.test, -c(perSq_price))

predictions.test<- exp(predict(model1, newdata=housing.test,interval="prediction"))
housing.predictions<-cbind(Observed=housing.test$price,predictions.test)
housing.predictions<-as.data.frame(housing.predictions)

plot.fits<-ggplot(housing.predictions, aes(x=Observed, y=fit)) + 
  geom_point(color="blue",size=3) +
  geom_abline() +
  xlab("Observed Value") +
  ylab("Fitted Value")
plot.fits

cor.test(housing.predictions$Observed,housing.predictions$fit)


#Forward selection using p-values as criterion
forward.p<-ols_step_forward_p(model1,data=housing)
model.forward.p<-forward.p$model
summary(model.forward.p)
AIC(model.forward.p)
predictions.model.forward.p<- exp(predict(model.forward.p, newdata=housing.test,interval="prediction"))

#Let's see how well this model actually predicts new values
#Remember the few observations we removed at the beginning
#They were removed just so we can see if our model can predict new observations.
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.forward.p)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)


#Let's plot the predicted values against the true known observed values.
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="green")
plot.fits

#Forward selection using p-values as criterion this time reducing the p-value cut off to 0.05
forward.p<-ols_step_forward_p(model1,data=housing, penter=0.05)
model.forward.p2<-forward.p$model
summary(model.forward.p2)
AIC(model.forward.p2)
predictions.model.forward.p2<- exp(predict(model.forward.p2, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.forward.p2)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)


#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="purple")
plot.fits



#Backward selection using p-values as criterion with the p-value cut off to 0.05
backward.p<-ols_step_backward_p(model1,data=housing, prem=0.05)
model.backward.p<-backward.p$model
summary(model.backward.p)
AIC(model.backward.p)
predictions.model.backward.p<- exp(predict(model.backward.p, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.backward.p)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="orange")
plot.fits

#Forward selection using AIC as criterion
forward.aic<-ols_step_forward_aic(model1,data=housing)
model.forward.aic<-forward.aic$model
summary(model.forward.aic)
predictions.model.forward.aic<- exp(predict(model.forward.aic, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.forward.aic)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="pink")
plot.fits


#Backward selection using AIC as criterion
backward.aic<-ols_step_backward_aic(model1,data=housing)
model.backward.aic<-backward.aic$model
summary(model.backward.aic)
predictions.model.backward.aic<- exp(predict(model.backward.aic, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.backward.aic)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="cyan")
plot.fits


#Forward-Backward selection using AIC as criterion
both.aic<-ols_step_both_aic(model1)
model.both.aic<-lm(formula=paste("log(price)",paste(both.aic$predictors,collapse="+"),sep="~"),data=housing)
summary(model.both.aic)
predictions.model.both.aic<- exp(predict(model.both.aic, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.both.aic)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="magenta")
plot.fits


#Have we just missed a better model?  This section takes a while to run

all.subset<-ols_step_best_subset(model1,print_plot=FALSE)
#Let's think about this problem logically instead.  What variables do you think are important?
ggpairs(housing[,c(pairing_col,which(Cat==0))],cardinality_threshold=20,diag=list(continuous="density",   discrete="bar"), axisLabels="show")
ggpairs(housing[,c(pairing_col,which(Cat==1))],cardinality_threshold=20,diag=list(continuous="density",   discrete="bar"), axisLabels="show")

# #Le's try a transformation on log of price.
model2<-lm(log(price)~.,data=housing)
summary(model2)
#Check asumptions
autoplot(model2,which=c(1,2,4))

all.subset<-ols_step_best_subset(model2,print_plot=FALSE)
all.subset

#Which variables  are really not important? I think we can remove these variable 
#elevation
#dist_am1
#dist_am2
#dist_am3
#precip

housing_red<-housing[,!colnames(housing) %in% c("elevation","dist_am1","dist_am2","dist_am3","precip")]


#Let's try a transformation on price.
model3<-lm(log(price)~.,data=housing_red)
summary(model3)
#Check asumptions
autoplot(model3,which=c(1,2,4))

all.subset<-ols_step_best_subset(model3,print_plot=FALSE)
all.subset

model.all.aic<-lm(formula=paste("log(price)",paste(strsplit(all.subset$predictors[3]," ")[[1]],collapse="+"),sep="~"),data=housing)
summary(model.all.aic)
predictions.model.all.aic<- exp(predict(model.all.aic, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.all.aic)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="brown")
plot.fits


model.all.aic<-lm(formula=paste("log(price)",paste(strsplit(all.subset$predictors[2]," ")[[1]],collapse="+"),sep="~"),data=housing)
summary(model.all.aic)
predictions.model.all.aic<- exp(predict(model.all.aic, newdata=housing.test,interval="prediction"))

#Predict new observations
housing.predictions<-cbind(Observed=housing.test$price,predictions.model.all.aic)
housing.predictions<-as.data.frame(housing.predictions)
cor.test(housing.predictions$Observed,housing.predictions$fit)

#Add these predictions to our plot
plot.fits<- plot.fits +
  geom_point(data=housing.predictions, aes(x=Observed, y=fit), color="red")
plot.fits


#Let's take a step back and think about all the models we have considered so far.
AIC(model1)
AIC(model2)
AIC(model.forward.p)
AIC(model.backward.p)
AIC(model.forward.aic)
AIC(model.backward.aic)
AIC(model.both.aic)
AIC(model.all.aic)


#The AIC and Adj R Sq  would be the input for our model selection

