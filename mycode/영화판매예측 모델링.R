library('xgboost')
library(caret)
library(MASS)
library(leaps)
library(car)
library(lmtest)
library(randomForest)
library(dummies)
library(dplyr)
library(gam)

library(AppliedPredictiveModeling)

movies_data<-read.csv('movie_data.csv')
str(movies_data)

colnames(movies_data) # 15개 변수 


# Remove "NA" obs (변화 없음)
library(TestDataImputation)
movies_data<-Listwise(movies_data,Mvalue = "NA") 
str(movies_data) 

y<-movies_data$revenue- movies_data$budget
idx<- which(y>=0)
y[idx]<-1 # 흑자 
y[-idx]<-0 # 적자
y<-as.factor(y)
str(y)

movies_data$y<-y

################ 추가적으로 안쓰는 변수 빼기 #############
movies_data$overview<-NULL
movies_data$production_companies<-NULL
movies_data$revenue<-NULL
movies_data$budget<-NULL
movies_data$X<-NULL



######### train/test data #########
set.seed(1234)
i<-createDataPartition(y=movies_data$y,p=0.7,list = F)
traindata<-movies_data[i,]
testdata<-movies_data[-i,]
nrow(traindata)
nrow(testdata)
head(traindata)

##### cutoff 설정 ######
# 1인 obs / 전체 obs 
length(which(movies_data$y==1))/length(movies_data$y) # 0.75

cutoff<- 0.70


######### Random Forest #########

RFTrain = randomForest(traindata[,-10],traindata$y, ntree=200, mtry = 4,importance = T)
par(mfrow=c(1,1))
plot(RFTrain) # choose best tree numbers


RF_pred=predict(RFTrain,newdata=testdata) # predict

RF_pred<-as.numeric(RF_pred)
RF_pred<-RF_pred-1
RF_pred<-ifelse(RF_pred>cutoff,1,0)
RF_pred<-as.factor(RF_pred)

confusionMatrix(RF_pred, testdata$y)


######## XGboost ##########

# dummy

dat.n<-dummy.data.frame(movies_data)
dat.n$y0<-NULL
m<-ncol(dat.n)

#train/test data
set.seed(1234)
traindata.n<-dat.n[i,]
testdata.n<-dat.n[-i,]
train.label<-traindata.n[,m]
test.label<-testdata.n[,m]
train = xgb.DMatrix(as.matrix(traindata.n[,-m]),label=train.label)
test = xgb.DMatrix(as.matrix(testdata.n[,-m]),label=test.label)

watchlist = list(train = train, test = test)
param = list(booster = 'gbtree',
             lambda = 1,
             gamma=0,
             subsample = 0.3,
             colsample_bytree =0.5,
             eta = 0.025,   
             max_depth = 6,
             max_leaf_nodes=64,
             eval_metric = "error",
             objective = 'binary:logistic',
             seed=1234)

XGtr = xgb.train(params = param, data = train,
                 nrounds = 295, watchlist=watchlist)
#295에서 에러 최소 

xgpre <- as.numeric( predict(XGtr,test) >cutoff)
xgpre<-as.factor(xgpre)
confusionMatrix(xgpre,testdata$y)
confusionMatrix(xgpre,testdata$y)$byClass




####### GAM ##########
fit_gam<- gam(y ~ genres  + s(popularity) + s(production_countries) +
                release_date + runtime + spoken_languages + s(vote_average) + s(vote_count)
              + LDA,data=movies_data,family ='binomial') 

summary(fit_gam)
par(mfrow=c(3,3))
plot(fit_gam)


gampre<- predict(fit_gam,newdata=testdata,type = 'response')



gampre_bi<-ifelse(gampre>cutoff,1,0)
gampre_bi<-as.factor(gampre_bi)

confusionMatrix(gampre_bi,testdata$y)
confusionMatrix(gampre_bi,testdata$y)$byClass




######### glm #######

fit_glm<- glm(y ~ .,data=movies_data,family ='binomial') 
glmpre<-predict(fit_glm,newdata = testdata,type='response')

glmpre_bi<-ifelse(glmpre>cutoff,1,0)
glmpre_bi<-as.factor(glmpre_bi)
confusionMatrix(glmpre_bi,testdata$y)
confusionMatrix(glmpre_bi,testdata$y)$byClass


####### stacking Model ########

predDF <- data.frame(RF_pred, gampre_bi , glmpre_bi , xgpre,
                     y = testdata$y, stringsAsFactors = F)
modelStack <- train(y ~ ., data = predDF, method = "rf")
combPred <- predict(modelStack, predDF)

combPred<-as.numeric(combPred)
combPred<-combPred-1
combPred<-ifelse(combPred>cutoff,1,0)
combPred<-as.factor(combPred)

confusionMatrix(combPred, testdata$y)
confusionMatrix(combPred, testdata$y)$byClass



###3#

#svm#########
fits<-best.tune(svm,y~. , data=traindata, kernel = 'radial',family='binominal')
summary(fits)
#predict(fits,testdata)
svm_tr<-svm(y~. , data=traindata, cost = 1 , gamma = 0.03846154  )
svm_pre<-predict(svm_tr, newdata = testdata)
confusionMatrix(svm_pre,testdata$y)$byClass


################################################
#XGboost

# 0.74
###########################################



i<-createDataPartition(y=movies_data$y,p=0.8,list = F)
l<-nrow(movies_data) - length(i)
traindata_s<-movies_data[i,]
testdata_s<-movies_data[-i,]
i2<-sample(nrow(traindata),l)
newtraindata<-traindata_s[-i2,]
validationdata<-traindata_s[i2,]


predRF <- predict(RFTrain, newdata = validationdata)
predGam <- predict(fit_gam, newdata = validationdata)
predglm<-predict(fit_glm, newdata = validationdata )
predDF <- data.frame(predRF, predGam, predglm, y = validationdata$y
                     , stringsAsFactors = F)
modelStack <- train(y ~ ., data = predDF, MethodsList = c('rf','gam','glm') )

# Train the ensemble
#modelStack <- train(diagnosis ~ ., data = predDF, method = "rf")


#redDF <- data.frame(RF_pred, gampre_bi, xgpre , glmpre_bi ,
                     #y = testdata$y, stringsAsFactors = F)

testPredRF <- predict(RFTrain, newdata = testdata_s)
testPredGam <- predict(fit_gam, newdata = testdata_s)
testPredglm <- predict(fit_glm, newdata = testdata_s)

testPredLevelOne <- data.frame(testPredRF, testPredGam, testPredglm, y = testdata_s$y, stringsAsFactors = F)
combPred <- predict(modelStack, testPredLevelOne)

confusionMatrix(combPred, testdata$y)





###############################

library(mlbench)
library(caret)
library(caretEnsemble)

head(train_beer)
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('qda', 'rpart', 'glm', 'svmRadial')
set.seed(1234)
models <- caretList(Alelager ~., data=dataset, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)




library(datasets)
library(caret)
library(caretEnsemble)

# load data
data("iris")

# establish cross-validation structure
set.seed(32)
trainControl <- trainControl(method="repeatedcv", 
                             number=5, repeats=3, # 3x 5-fold CV
                             search="random")

algorithmList <- c('lda',         # Linear Discriminant Analysis 
                   'rpart' ,      # Classification and Regression Trees
                   'svmRadial')   # SVM with RBF Kernel

# cross-validate models from algorithmList
models <- caretList(Species~., data=iris, trControl=trainControl, methodList=algorithmList)


