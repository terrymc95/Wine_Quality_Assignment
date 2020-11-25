## Install packages 

#install.packages("dplyr")
library(dplyr)

#install.packages("raster")
library(raster)

#install.packages("MASS")
library(MASS)

library(class)

library(boot)

#install.packages("tree")
library(tree)

#install.packages("lmtest")
library(lmtest)

library(car)

#install.packages("PredPsych")
library(PredPsych)



##### Custom Fuctions ################################################




clamped <- function(x){ ## This clamps the upper and lower bounds of a variable to be the mean +/- 3 Sdev
  clamp(x, lower = (mean(x, na.rm = T) - 3*sd(x, na.rm = T)),
        upper = (mean(x, na.rm = T) + 3*sd(x, na.rm = T)))
  
}





compare <- function(x1,x2 = NULL, title = NULL){
  p1 <- hist(x1)
  p2 <- hist(x2)
  plot(p1, col = "orange", main = paste("Comparison of ",
  title), xlab = title)
  plot(p2, col = "green", add = T)
}



##### Reading Data ####################################################

## csv file is seperated with ; insteand of , need to define seperator
vino <- read.csv("winequality.csv", sep = ";")

dim(vino) ## 4898  white wines with 12 variables 

NormalVino <- list()

str(vino)

###### Checking the data quality ######################################


## Fixed Acidity 
compare(vino$fixed.acidity, clamped(vino$fixed.acidity), "Fixed Acidity")
NormalVino$Fixed <- clamped(vino$fixed.acidity)


## Volatile Acidity 
compare(vino$volatile.acidity, clamped(vino$volatile.acidity), "Volatile Acidity")
NormalVino$Volatile <- clamped(vino$volatile.acidity)


## CitricAcid

compare(vino$citric.acid, clamped(vino$citric.acid), "Citric Acid")
NormalVino$Citric <- clamped(vino$citric.acid)


## Sugar

compare(vino$residual.sugar, clamped(vino$residual.sugar), "Sugar")
NormalVino$Sugar <- clamped(vino$residual.sugar)


## Chlorides

compare(vino$chlorides, clamped(vino$chlorides), "Chlorides")
NormalVino$Chlorides <-  clamped(vino$chlorides)


## Free Sulphur

compare(vino$free.sulfur.dioxide, clamped(vino$free.sulfur.dioxide), "Free Sulphur")
NormalVino$FreeS <- clamped(vino$free.sulfur.dioxide)


## Total Sulphur 

compare(vino$total.sulfur.dioxide, clamped(vino$total.sulfur.dioxide), "Total Sulphur")
NormalVino$TotalS <- clamped(vino$total.sulfur.dioxide)


## Density

compare(vino$density, clamped(vino$density), "Density")
NormalVino$Density <- clamped(vino$density)


## pH levels

compare(vino$pH, clamped(vino$pH), "pH Levels")
NormalVino$pH <- clamped(vino$pH)


## Sulphates 

compare(vino$sulphates, clamped(vino$sulphates), "SO4") ## There is no difference between clamped and unclamped this won't be changed
NormalVino$SO4 <- vino$sulphates


## Alcohol

compare(vino$alcohol, clamped(vino$alcohol), "Boozyness") ## There is no difference between clamped and unclamped this won't be changed
NormalVino$Alco <-  vino$alcohol


## The Target Variable 
## Quality 

## This needs to be categorical data

NormalVino$Quality <- as.factor(vino$quality)


## Converting to a dataframe

vino <- data.frame(NormalVino)
NormalVino <- NULL
summary(vino$Quality)

summary(vino)


# 
# The business problem with the wine data is the price of wine is 
# directly correlated to the quality. Lambrini is of poor quality 
# and is very cheap whereas ChÃ¢teauneuf-du-Pape is regarded as one
# of the finest wines from France and the price corresponds to 
# that. Therefore in this analysis it of high importance to be able
# to determine the difference between the Low, medium, and High 
# quality wines to save money in getting a professional to test 
# and losing out in stock with wasted wine plus the expert could 
# be biased in their testing so being able to use pysiochemical 
# tests will remove this potential bias.


summary(vino$Quality)

plot(vino$Quality)
## The majority of the wine is in 5 , 6, or 7 this is the mid range wine 
## 8 and 9 is the high quality wine
## 3 and 4 is the low quality wine






##### Good Wine BAd Wine Analysis #####################################


## Using Logistic and LDA methods

## Need to reduce quality to two levels good and  bad 

LogVino <- vino

LogVino$Quality <- as.factor(LogVino$Quality)
summary(LogVino$Quality)


levels(LogVino$Quality)[1:3] <- 0 ## 0 is bad 
levels(LogVino$Quality)[2:5] <- 1 ## 1 is good
summary(LogVino$Quality)




LogMod <- glm(Quality ~ ., data = LogVino, family = "binomial")
summary(LogMod)
## Fixed, Citric, Chlorides and Total sulphur are not significant in this model 
vif(LogMod) ## Density is higly correlating should be removed from the model
LogForm <- Quality ~ Volatile + Sugar + FreeS + pH + SO4 + Alco
LogMod <- glm(LogForm, data = LogVino, family = "binomial")
vif(LogMod) ## No multicollinearity 
summary(LogMod) ## pH has become insignificant 

LogForm2 <- Quality ~ Volatile + Sugar + FreeS +  SO4 + Alco
LogMod2 <- glm(LogForm2, data = LogVino, family = "binomial")
vif(LogMod2) ## No multicollinearity 

summary(LogMod2)



## Getting the odds ratio to see which variables are most significant to Quality 

exp(LogMod$coefficients)

## Sugar, Free sulphur, pH, Sulphates and Alcohol are key to good wine  Volitility is not 




Log_prob <- predict(LogMod2, type = "response")
Log_prob[1:10]
contrasts(LogVino$Quality)
Log_pred <- rep(0, 4898)
Log_pred[Log_prob > 0.5266] <- 1

table(Log_pred, LogVino$Quality)
mean(Log_pred == LogVino$Quality)  ## 75.8812% correctly classified 


## Developing a training dataset
LogVino$ID <-  c(1:nrow(LogVino))
summary(LogVino[[13]])
##Using Q3 as the cut off

train <- (LogVino$ID < 3974)
summary(train)  ## Creates a testing set of 925

LogVino.3974 <- LogVino[!train,]
Quality.3974 <- LogVino$Quality[!train]

LogMod3 <- glm(LogForm2, data = LogVino, family = "binomial", subset = train)
Log_prob2 <- predict(LogMod3, LogVino.3974, type = "response")

Log_pred2 <- rep(0,nrow(LogVino.3974))
Log_pred2[Log_prob2 > 0.46] <- 1
table(Log_pred2, Quality.3974)
mean(Log_pred2 == Quality.3974)  ## 78.6% correctly classfied from test dataset

exp(LogMod3$coefficients)

##### Linear Discriminant Analysis Model

## LDA works very sinilarly to Logisitic regression 


## Using the same dataframe used in logistic regression 

ldaMod <- lda(Quality~.-ID, data = LogVino, subset = train)
ldaMod  ## Probability that bad is 0.3451 good is 0.6549


plot(ldaMod)


lda_pred <- predict(ldaMod, LogVino.3974)

lda_class <- lda_pred$class
table(lda_class, Quality.3974)
mean(lda_class == Quality.3974) ## 76.54% accurate

cv.glm(data = LogVino,glmfit = ldaMod,K = 10)$delta

 

##### Determining if the wine is mid range or not #############################

## Splitting making the vino list into mid range or other quallity 

vino_2 <- vino

levels(vino_2$Quality)[1:2] <- 0 ## 0 is other 
levels(vino_2$Quality)[2:4] <- 1 ## 1 is mid range quality
levels(vino_2$Quality)[3:4] <- 0
summary(vino_2$Quality)

## Binary decision therefore Logistic regression can be used 

MidMod <- glm(Quality ~ ., data = vino_2, family = "binomial")
summary(MidMod)
vif(MidMod)


MidProb <- predict(MidMod, type = "response")
MidPred <- rep(0,4898)
MidPred[MidProb > 0.8] <- 1
table(MidPred, vino_2[[12]])
mean(MidPred == vino_2[[12]]) ## 91%

exp(MidMod$coefficients)

## Partioning the data into test and training 

vino_2$ID <- c(1:nrow(vino_2))
set.seed(40126429)
MidTrain <- sample(vino_2$ID, nrow(vino_2)*.75)
summary(MidTrain)
# x <- nrow(vino_2)-length(MidTrain)
# MidTrain <- c(MidTrain, rep(0,x))


train <- vino_2 %>%  ## Training dataset 
            filter(vino_2$ID %in% MidTrain) 

test <- vino_2 %>%  ## Testing dataset
            filter(!vino_2$ID %in%  MidTrain)
MidMod2 <- glm(Quality~. - ID, data = train, family = "binomial")

MidProb2 <- predict(MidMod2, test, type = "response")
MidPred2 <- rep(0, nrow(test))
MidPred2[MidProb2 >0.8] <- 1
table(MidPred2, test$Quality)
mean(MidPred2 == test$Quality) ## 91.9%

exp(MidMod2$coefficients)

MidPredAccuracy <- mean(MidPred2 == test$Quality)



##### Developing a model that is either Low or High quality wine #########

### Creating the data frame first 
HiLoQual <- c(4,3,8,9)
HiLo <- vino %>% 
          filter(Quality %in% HiLoQual)
HiLo$Quality <- as.factor(HiLo$Quality)


levels(HiLo$Quality)[1:5] <- 0 ## 1 to 5 to remove the missing levels for midrange
levels(HiLo$Quality)[2:3] <- 1 ## 1 is high quality wine


dim(HiLo)
## 363 eneteries 
## LDA works better for smaller dataframes 

HiLoMod <- lda(Quality~., HiLo)
HiLoMod

HiLopre <- predict(HiLoMod)
HiLoclass <- HiLopre$class
table(HiLoclass, HiLo$Quality)
mean(HiLoclass == HiLo$Quality) ## 84.57% accurate

## Change probabilities 
HiLoPos <- HiLopre$posterior
HiLoProb <- as.factor(HiLoPos[,1]< 0.46 ) ## more accurate cutoff
summary(HiLoProb)
levels(HiLoProb)[1] <- 0
levels(HiLoProb)[2] <- 1
table(HiLoProb, HiLo$Quality)
mean(HiLoProb == HiLo$Quality) ## 86.23% accurate 


## create a training and testing set
##set.seed(40126429)
HiLo$ID <- c(1:nrow(HiLo))
HiLoSampID <-  sample(HiLo$ID, nrow(HiLo)*0.75)

trainHiLo <- HiLo %>%   ## Traing dataset for the HiLo model 
              filter(ID %in% HiLoSampID)
testHiLo <- HiLo %>%    ## Testing datset for the HiLo Model 
              filter(!ID %in% HiLoSampID)

HiLoMod2 <- lda(Quality~. -ID, trainHiLo)
x <- HiLoMod2$scaling
View(x)
HiLopre2 <- predict(HiLoMod2,testHiLo)
HiLoclass2 <- HiLopre2$class
table(HiLoclass2, testHiLo$Quality)
mean(HiLoclass2 == testHiLo$Quality) ## 83.51% accurate




## Comparing it to a LOG regression model 

HiLoMod3 <- glm(Quality ~. -ID, data = trainHiLo, family = "binomial")
summary(HiLoMod3)
exp(HiLoMod3$coefficients)

HiLoProb3 <- predict(HiLoMod3, testHiLo, type = "response")
HiLopre3 <- rep(0,nrow(testHiLo))
HiLopre3[HiLoProb3 > 0.585] <- 1
table(HiLopre3, testHiLo$Quality)
mean(HiLopre3 == testHiLo$Quality) ## 90.11% accuracy 

HiLoAccuracy <- mean(HiLopre3 == testHiLo$Quality)

### Log regression is a suoerior model in this case 

## The Accuracy of predicting the wether a high or low quality wine is given by 
HiLoAccuracy * MidPredAccuracy ## = 82.83%


##### Modeling the mid quality wines ################################

## Creating the dataframe

MidWine <- vino %>% 
            filter(!Quality %in% HiLoQual)
MidWine$Quality <- as.factor(MidWine$Quality)
summary(MidWine$Quality)

levels(MidWine$Quality)[1:3] <- 5 ## to remove the empty levels of 2 and 3 
levels(MidWine$Quality)[3:5] <- 7 ## to remove the empty levels of 8 and 9


### Multilevel classification allows 3 methods; LDA, QDA, and Classification Tree


### LDA for mid wine 


MidLda <- lda(Quality ~., MidWine)
MidLda

MidLda.pred <- predict(MidLda)
lda.class <- MidLda.pred$class
table(lda.class, MidWine$Quality)
mean(lda.class == MidWine$Quality) ## 58.26%


## Creating a training and testing datasets

MidWine$ID <- c(1:nrow(MidWine))
MidSample <- sample(MidWine$ID, nrow(MidWine)*0.75)

trainMid <- MidWine %>% 
              filter(ID %in% MidSample)
testMid <- MidWine %>% 
              filter(!ID %in% MidSample)
nrow(trainMid)+nrow(testMid)==nrow(MidWine) ## True

MidLda2 <- lda(Quality~. -ID, data = trainMid)
MidLda.pred2 <- predict(MidLda2, testMid)
lda.class2 <- MidLda.pred2$class
table(lda.class2, testMid$Quality)
mean(lda.class2 == testMid$Quality)  ##58.46% accuracy in predictability 

plot(MidLda2)
## Low accuracy is probably because the boundary is not very well defined by linear means


### QDA approach 

MidQDA <- qda(Quality ~. -ID, data = trainMid)
MidQDA


QDApred <- predict(MidQDA, testMid)
qda.class <- QDApred$class
table(qda.class, testMid$Quality)
mean(qda.class == testMid$Quality) ## Less accurate meaning discrimnant analysis may not be the best way to distinguish between these qualites


## Trying tree classification 

Midtree <- tree(Quality~.-ID, data = trainMid)
summary(Midtree)
plot(Midtree)
text(Midtree)


TreePred <- predict(Midtree, testMid, type = "class")
table(TreePred, testMid$Quality)
mean(TreePred == testMid$Quality) ## 57.2% accurate 


### Crossvalidation LDA


## K-fold crossvalidation of all data in the MIDWINE dataframe, Quality is being predicted, there is 100 samples taken with 75 of them used to train the model and 25 to test it 
MIDWINEACCURACY<- LinearDA(Data = MidWine, classCol = 12, selectedCols = c(1:12), cvType = "folds", nTrainFolds = 100,ntrainTestFolds = 75, SetSeed = T) ##66% testing accuracy

## Changing the formula  inputted into LDA

## Using the formula from the good vs bad wine section 

## LogForm2 = Quality ~ Volatile + Sugar + FreeS + SO4 + Alco


MidLda3 <- lda(LogForm2, data = trainMid)
MidPred3 <- predict(MidLda3, testMid)
lda.class3 <- MidPred3$class
table(lda.class3, testMid$Quality)
mean(lda.class3 == testMid$Quality)  ## 59.35% accuracy in test which is slightly more accurate 

## trying with the QDA and Tree method 

MidQDA2 <- qda(LogForm2, data = trainMid)
QDApred2 <- predict(MidQDA2, testMid)
qda.class2 <- QDApred2$class
table(qda.class2,testMid$Quality)
mean(qda.class2 == testMid$Quality) ## less accurate than before 

Midtree2 <- tree(LogForm2, data = trainMid)
summary(Midtree2)
plot(Midtree2)
text(Midtree2)


TreePred2 <- predict(Midtree2, testMid, type = "class")
table(TreePred2, testMid$Quality)
mean(TreePred2 == testMid$Quality) ## 57.2% accurate  same accuracy 

## crossvalidation LDA

MIDWINEACCURACY2<- LinearDA(Data = MidWine, classCol = 12, selectedCols = c(2,4,6,10,11,12), cvType = "folds", nTrainFolds = 100,ntrainTestFolds = 75, SetSeed = T) ##68.3% testing accuracy
MIDWINEACCURACY2 

