
# Forecasting bikes demand
Participating in Kaggle competition to forecast bikes sharing demand system, made the following code presenting some supervising methods.

'''{r}
train <- read.csv("C:\\Users\\ofer\\Dropbox\\Data mining and Machine Learning\\Bike rental competition - Kaggle\\train.csv")
test <- read.csv("C:\\Users\\ofer\\Dropbox\\Data mining and Machine Learning\\Bike rental competition - Kaggle\\test.csv")
'''

# str(train)

# View(train)

train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)

test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

#View(train_factor)

# create time column
train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)

str(train_factor)


train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)


#create day of week column
install.packages("lubridate")
library(lubridate)

train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

aggregate(train_factor[,"count"],list(train_factor$day),mean)

plot(train_factor$count~train_factor$day, type = 'c', xlab = 'hello')


#create Sunday variable
train_factor$sunday[train_factor$day == "Sunday"] <- "1"                   
train_factor$sunday[train_factor$day != "Sunday"] <- "0"

test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "Sunday"] <- "0"

#convert to factor
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#convert time and create $hour as integer to evaluate for daypart
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))

#create daypart column, default to 4 to make things easier for ourselves
train_factor$daypart <- "5"
test_factor$daypart <- "5"

install.packages("car")
library(car)

# Plots

#scatterplot(train_factor$count~train_factor$hour , main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

#scatterplot(train_factor$registered~train_factor$hour , main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

#scatterplot(train_factor$casual~train_factor$hour , main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=10)

#plot(train_factor$casual~train_factor$hour, type = "h")

plot(train_factor$count~train_factor$hour, type = "h")

#23PM - 6AM = 1
train_factor$daypart[(train_factor$hour < 7) | (train_factor$hour > 22)] <- 1
test_factor$daypart[(test_factor$hour < 7) | (test_factor$hour > 22)] <- 1


#7AM - 10AM = 2
train_factor$daypart[(train_factor$hour > 6 ) & (train_factor$hour < 11)] <- 2
test_factor$daypart[(test_factor$hour > 6) & (test_factor$hour < 11)] <- 2


#11AM - 15PM = 3
train_factor$daypart[(train_factor$hour > 10) & (train_factor$hour < 16)] <- 3
test_factor$daypart[(test_factor$hour > 10) & (test_factor$hour < 16)] <- 3

#16PM - 19PM = 4
train_factor$daypart[(train_factor$hour > 15) & (train_factor$hour < 20)] <- 4
test_factor$daypart[(test_factor$hour > 15) & (test_factor$hour < 20)] <- 4

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)

#convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)

#making year a new variable

train_factor$year <- as.integer(substr(train_factor$datetime,	1,4))
train_factor$year <- as.factor(train_factor$year)

test_factor$year <- as.integer(substr(test_factor$datetime,  1,4))
test_factor$year <- as.factor(test_factor$year)

str(train_factor)

# View(train_factor)

# deleting NVA cols

train_factor$datetime <- NULL
train_factor$time <- NULL

# test_factor$datetime <- NULL
# test_factor$time <- NULL


### Start evaluating models! ###

# Difinitions
library(magrittr)
library(dplyr)
MSE <- function(x) x^2 %>% mean



# OLS
rm(ols.1)
ols.1 <- lm(train_factor$count~season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + sunday + hour + daypart ,data = train_factor)

# Train error:
predict(ols.1)
summary (ols.1)
ols.1$coef
# plot(ols.1)

MSE( predict(ols.1)- train_factor$count)
# MSE( predict(ols.1)- prostate.train$lcavol)
# Test error:

MSE( predict(ols.1, newdata = test_factor)- test_factor$count)



### OLS Regression Model Selection 
Best subset selection: find the best model of each size:
  ```{r best subset}


str(train_factor)

library(leaps)

regfit.full <- train_factor %>% 
  regsubsets(train_factor$count~season + holiday + workingday + weather + temp + atemp + humidity + windspeed + day + sunday + hour + daypart, 
             data = ., method = 'exhaustive')
summary(regfit.full)

plot(regfit.full, scale = "Cp")

regfit.1 <- lm(train_factor$count~ workingday + temp + hour+ daypart ,data = train_factor)

MSE(predict(regfit.1)- train_factor$count)


# AIC model selection: 
  ```{r OLS AIC}
# Forward search:
ols.2 <- lm(count~1 ,data = train_factor)
model.scope <- list(upper=ols.1, lower=ols.2)
step(ols.2, scope=model.scope, direction='forward', trace = TRUE)

MSE( predict(ols.2)- train_factor$count)

# Backward search:
step(ols.1, scope=model.scope, direction='backward', trace = TRUE)
```
MSE( predict(ols.1)- train_factor$count)


# Ridge Regression

```{r Ridge I}
# install.packages('ridge')
library(ridge)
library(MASS)
rm(ridge.1)
str(train_factor)
ridge.1 <- linearRidge(count~. ,data = train_factor)
# Note that if not specified, lambda is chosen automatically by linearRidge.
#rm(ridge.1)
predict(ridge.1)

summary(ridge.1)


# Ridge with lm.ridge function

ridge.2 <-lm.ridge(count~. ,data = train_factor)


summary(ridge.2)

plot(ridge.2)
select(ridge.2)
ridge.2$coef

coef(ridge.final)[1] + coef(ridge.final)[2]*test[,1] + 
  coef(ridge.final)[3]*test[,2] + coef(ridge.final)[4]*test[,4]


# Train error:
MSE( predict(ridge.2)- train_factor$count)


# Ridge Regression II with glmnet 
library(glmnet)

rm(gl.train)
str(train_factor)
gl.x_train <- train_factor  %>% as.matrix
gl.y_train <- train_factor$count

gl.x_train<- subset(gl.x_train, select = season:daypart)
str(gl.x_train)
View(gl.x_train)
View(gl.y_train)

gl.y_train <- as.numeric(gl.y_train)

fit1 = glmnet(gl.x_train, gl.y_train)
ridge.2 <- glmnet(x=gl.x_train, y=gl.y_train, alpha = 0)
class(gl.x_train)
class(gl.y_train)


# Neural Net

## Regression
```{r NNET regression}
library(nnet)
nnet.1 <- nnet(count~., size=20, data=train_factor, linout =1,rang = 0.1, decay = 5e-4, maxit = 1000, MaxNWt =11000)

summary(nnet.1)

View(train_factor)
# Train error:
MSE( predict(nnet.1)- train_factor$count)


# upload results

test_factor$predictions10<- predict(nnet.1, newdata=test_factor)

#install.packages('xlsx')
library(xlsx)

str(dfrm)
str(test_factor)
dfrm <- data.frame(datetime = test_factor$datetime, count = test_factor$predictions10)
write.csv(dfrm, "mydata.csv")
```

# Random forest

#install.packages('randomForest')
library(randomForest)

class(train_factor)
str(train_factor)
set.seed(415)

str(train_factor)

myNtree = 200
myMtry  =	5
myImportance  =	TRUE

cor(train_factor[c('temp', 'atemp', 'humidity', 'windspeed')])
pairs(train_factor)



RandForest <- randomForest(train_factor$count~season + holiday + workingday + weather + atemp + humidity + windspeed + day + sunday + hour + daypart + year, data=train_factor, 
                    importance=TRUE, ntree=200, maxnodes = 10, replace = TRUE)

varImpPlot(RandForest)

RandForPredict <- predict(RandForest, test_factor)

dfrm <- data.frame(datetime = test_factor$datetime, count = RandForPredict)
write.csv(dfrm, "RandomForestDataVer2.csv")



# Random forest to predict registered + casual

#RandForestCasu <- randomForest(train_factor$casual~season + holiday + workingday + weather + temp + humidity + windspeed + day + sunday + hour + daypart + year, data=train_factor, 
                              importance=TRUE, ntree=400,mtry = myMtry,  replace = TRUE)

#varImpPlot(RandForestCasu)

RandForestCasu.fin <- randomForest(train_factor$casual~season + workingday + weather + temp + humidity + day + hour + daypart + year, data=train_factor, 
                                   importance=TRUE, ntree=400,mtry = myMtry,  replace = TRUE)

#RandForestReg <- randomForest(train_factor$registered~season + holiday + workingday + weather + temp + humidity + windspeed + day + sunday + hour + daypart + year, data=train_factor, 
                              importance=TRUE, ntree=400,  mtry = myMtry, replace = TRUE)
#varImpPlot(RandForestReg)

RandForestReg.fin <- randomForest(train_factor$registered~season + workingday + weather + temp + humidity + hour + daypart + year, data=train_factor, 
                              importance=TRUE, ntree=400,  mtry = myMtry, replace = TRUE)

RandCasu <- predict (RandForestCasu.fin, test_factor)
RandReg <-predict(RandForestReg.fin, test_factor)

RandForPredict.2 <- round(RandReg + RandCasu, 0)

dfrm <- data.frame(datetime = test_factor$datetime, count = RandForPredict.2)
write.csv(dfrm, "RandomForestDataVer6.csv")

