setwd("~/Desktop/ST443_Project")

##### READING AND CLEANING DATA #####
data = read.csv("IndianLiverPatient.csv", header = TRUE)
attach(data)
dim(data)
#583 observations with 11 variables
head(data)
#2 categorical variables - yvar: "is_patient" and xvar: "gender"
summary(data)
#4 NA's observed in alkphos
#check again total number of missing values
sum(is.na(data))
#4 in total
sum(is.na(data)) / dim(data)[1] *100
#since the NA's only make up 0.69% of the total sample size, we will remove the 4 rows containing missing values
data = na.omit(data)
sum(is.na(data))
#0 now


##### CHECKING AND CORRECTING TYPES OF VARIABLES #####
str(data)
#age              : int
#gender           : factor
#tot_bilirubin    : num
#direct_bilirubin : num
#tot_proteins     : int
#albumin          : int
#ag_ratio         : int
#sgpt             : num
#sgot             : num
#alkphos          : num
#is_patient       : int

#want to change the type of is_patient to factor 
#relevel gender so that its baseline is the level with the most observations, i.e. Male
#change types of other variables to numeric
data$is_patient = ifelse(data$is_patient==2,0,1)
data$is_patient = as.factor(data$is_patient) 
data$gender = relevel(as.factor(data$gender), "Male") #setting Male as baseline as it is the most commonly occurring level
for(i in c(1,5:7)){
  data[,i]<-as.numeric(as.character(data[,i]))
}
str(data)
#age              : num
#gender           : factor 
#tot_bilirubin    : num
#direct_bilirubin : num
#tot_proteins     : num
#albumin          : num
#ag_ratio         : num
#sgpt             : num
#sgot             : num
#alkphos          : num
#is_patient       : factor


##### INITIAL PLOTS #####
pairs(data, col=data$is_patient, main="First Look at Data - Scatterplots")
#from the plots, noticed some linear correlations between some pairs of covariates, check as below
plot(data$is_patient, data$gender, xlab="Patient Status", ylab="Gender", main="Plot of Gender vs. Patient Status")
#imbalance sample for both, is_patient and gender

library(reshape2)
library(ggplot2)
data.melt = melt(data[,-c(2)], id.var = "is_patient")
ggplot_box = ggplot(data=data.melt, aes(x=is_patient, y=value, fill=is_patient)) 
ggplot_box = ggplot_box + geom_boxplot() + facet_wrap(~variable, scales="free_y", ncol = 3)
ggplot_box = ggplot_box + xlab("Patient Status (is_patient: 0=not patient, 1=patient)")
ggplot_box = ggplot_box + ylab("Covariates") + ggtitle("Boxplots of Quantitative Features vs. Patient Status")
ggplot_box = ggplot_box + guides(fill=guide_legend(title="Patient Status"))
ggplot_box = ggplot_box + theme(plot.title = element_text(hjust=0.5))
ggplot_box


##### CORRELATION ANALYSIS #####
library(corrplot)
data_corr = cor(data[ ,-c(2,11)]) #create an object of the features
dev.off()
corrplot.mixed(data_corr, tl.cex=0.55)
title("Correlations Between Numeric Covariates", line = 2.5)

#high correlations between:
#tot_bilirubin, direct bilirubin -- expected, as both are just manipulations of the same thing
#ag_ratio, albumin -- expected, albumin is part of ag_ratio
#sgpt, sgot -- expected, as both have one element that's the same
#alkphos, sgot -- don't know why
#to be wary of these variables, if they are in the final model


##### LOGISTIC REGRESSION MODEL #####
## SPLIT INTO TRAINING AND TESTING DATA ##
library(MASS)
set.seed(1)
train = sample(x = 579, size = 463, replace=FALSE)
#using 80:20 as training:testing ratio
train_data = data[train,]
test_data = data[-train,]
#to ensure a balanced spread of outcome across train_data and test_data
table(data$is_patient)
#split ratio is 165:414 = 0.399
table(train_data$is_patient)
#split ratio is 130:333 = 0.390
table(test_data$is_patient)
#split ratio is 35:81 = 0.432
#acceptable split ratios of outcomes in dataset

## FULL MODEL ##
full.model = glm(is_patient~., data=train_data, family=binomial)
summary(full.model)
#4 predictors are significant at 5% significance level
#4 predictors are: age, direct_bilirubin, albumin, sgpt
confint(full.model)
#the 4 significant predictors have confidence intervals that do not cross zero
#produce odds ratios to enable model interpretability
exp(coef(full.model))
#to interpret -- change in the outcome odds resulting from a unit change in the feature
#all the features will decrease the log odds except sgot 
#now to investigate further the potential multicollinearity issue with having a full model
library(car)
vif(full.model)
#vif for all variables are below 2, except sgpt, sgot and alkphos, all exceeding the VIF rule of thumb statistic of 5
#vif for sgpt = 15.81
#vif for sgot = 30.12
#vif for alkphos = 8.97

#test for model fit on train_data
full.model.train.probs=predict(full.model, type="response")
#inspect the first 5 predicted probabilities
full.model.train.probs[1:5]
contrasts(train_data$is_patient)
#probabilities closer to 0 would be classified as 0("not a patient")
#probabilities closer to 1 would be classified as 1("is a patient")
full.model.train.pred = ifelse(full.model.train.probs>0.5, 1,0)
table(pred=full.model.train.pred, true=train_data$is_patient)
mean(full.model.train.pred==train_data$is_patient)
#accuracy: 0.7365
#sensitivity: 0.919
#specificity: 0.269

#now, test performance on test_data
full.model.test.probs=predict(full.model, newdata=test_data, type="response")
full.model.test.pred = ifelse(full.model.test.probs>0.5, 1,0)
table(pred=full.model.test.pred, true=test_data$is_patient)
mean(full.model.test.pred==test_data$is_patient)
#accuracy: 0.7328 
#sensitivity: 0.951
#specificity: 0.229

## BEST SUBSET SELECTION -- FOR MORE PARSIMONIOUS MODEL ##
library(bestglm)

#best subsets using BIC
best.subset.bic = bestglm(Xy = train_data, family=binomial, IC = "BIC", method = "exhaustive")
best.subset.bic
#only includes 3 variables, i.e. age, direct_bilirubin and albumin -- 3 of the 4 significant predictors in full model
names(best.subset.bic)
bic_for_plot <- best.subset.bic$Subsets
#need to refit in order to predict probabilities
best.subset.fit.bic = glm(is_patient~age+direct_bilirubin+albumin, data=train_data, family=binomial)
#test for model fit on train_data
best.subset.train.bic.probs = predict(best.subset.fit.bic, type="response")
best.subset.train.bic.pred=ifelse(best.subset.train.bic.probs>0.5, 1, 0)
table(best.subset.train.bic.pred, train_data$is_patient)
mean(best.subset.train.bic.pred==train_data$is_patient)
#accuracy: 0.728 
#sensitivity: 0.931
#specificity: 0.208
#test performance on test_data
best.subset.test.bic.probs = predict(best.subset.fit.bic, newdata=test_data, type="response")
best.subset.test.bic.pred=ifelse(best.subset.test.bic.probs>0.5, 1, 0)
table(best.subset.test.bic.pred, test_data$is_patient)
mean(best.subset.test.bic.pred==test_data$is_patient)
#accuracy: 0.724
#sensitivity: 0.963
#specificity: 0.171

#best subsets using AIC
best.subset.aic = bestglm(Xy = train_data, family=binomial, IC = "AIC", method = "exhaustive")
best.subset.aic
#includes 4 variables, i.e. age, direct_bilirubin, tot_proteins(which was not significant in the full model) and albumin
aic_for_plot <- best.subset.aic$Subsets
#need to refit in order to predict probabilities
best.subset.fit.aic = glm(is_patient~age+direct_bilirubin+tot_proteins+albumin, data=train_data, family=binomial)
#test for model fit on train_data
best.subset.train.aic.probs = predict(best.subset.fit.aic, type="response")
best.subset.train.aic.pred=ifelse(best.subset.train.aic.probs>0.5, 1, 0)
table(best.subset.train.aic.pred, train_data$is_patient)
mean(best.subset.train.aic.pred==train_data$is_patient)
#accuracy: 0.737
#sensitivity: 0.931
#specificity: 0.238
#test performance on test_data
best.subset.test.aic.probs = predict(best.subset.fit.aic, newdata=test_data, type="response")
best.subset.test.aic.pred=ifelse(best.subset.test.aic.probs>0.5, 1, 0)
table(best.subset.test.aic.pred, test_data$is_patient)
mean(best.subset.test.aic.pred==test_data$is_patient)
#accuracy: 0.7241
#sensitivity: 0.963
#specificity: 0.171

#best subsets using CV, K=10
#need outcome to be called y, so add variable y, later can remove
train_data$y = rep(0, 463)
train_data$y[train_data$is_patient=="1"]=1
head(train_data[ ,12])
#remove the original is_patient column as now we're replacing it with y
train_data.cv = train_data[ ,-11]
head(train_data.cv)
#conduct best subset selection using CV
set.seed(2)
best.subset.cv = bestglm(Xy = train_data.cv, family=binomial, IC = "CV", CVArgs = list(Method="HTF", K=10, REP=1))
best.subset.cv
##using 1 std. error rule, 2 is the number of variables
#need to refit in order to predict probabilities
train_data = train_data[, -12]
best.subset.fit.cv = glm(is_patient~direct_bilirubin+albumin, data=train_data, family=binomial)
#test for model fit on train_data
best.subset.train.cv.probs = predict(best.subset.fit.cv, type="response")
best.subset.train.cv.pred=ifelse(best.subset.train.cv.probs>0.5, 1, 0)
table(best.subset.train.cv.pred, train_data$is_patient)
mean(best.subset.train.cv.pred==train_data$is_patient)
#accuracy: 0.721 
#sensitivity: 1.000
#specificity: 0.008
#test performance on test_data
best.subset.test.cv.probs = predict(best.subset.fit.cv, newdata=test_data, type="response")
best.subset.test.cv.pred=ifelse(best.subset.test.cv.probs>0.5, 1, 0)
table(best.subset.test.cv.pred, test_data$is_patient)
mean(best.subset.test.cv.pred==test_data$is_patient)
#accuracy: 0.698
#sensitivity: 1.000
#specificity: 0.000

#penalized logistic regression using LASSO
library(glmnet)
#preparing data
x.lasso.train <-model.matrix(is_patient~.-1, data=train_data)
y.lasso.train <- train_data$is_patient

#generating model
#use CV to select the best lambda, and then find the model with the best lambda
set.seed(1)
cv.logit_lasso <-cv.glmnet(x=x.lasso.train, y=as.factor(y.lasso.train), alpha=1, family="binomial", nfolds=10)
best_lambda <- cv.logit_lasso$lambda.1se  
#0.0251
final_logit_lasso <- glmnet(x=x.lasso.train, y=as.factor(y.lasso.train), lambda=best_lambda, family = "binomial")
coef(final_logit_lasso)
#5 variables chosen

#make predictions on the test data
x.lasso.test <- model.matrix(is_patient~.-1, data=test_data)
final_logit_lasso.probs <- predict(final_logit_lasso, newx = x.lasso.test, type="response")
final_logit_lasso.pred <- ifelse(final_logit_lasso.probs>0.5, 1, 0)
table(final_logit_lasso.pred, test_data$is_patient)
mean(final_logit_lasso.pred==test_data$is_patient)
#accuracy: 0.698
#sensitivity: 1.000
#specificity: 0.000

#plot all 4 methods in one screen
par(mfrow=c(2,2))
##BIC
plot(x=0:10, y=bic_for_plot$BIC, xlab="Number of Features", ylab="BIC", main="BIC vs. Number of Features", pch=19, col="blue")
abline(v=3)
##AIC
plot(x=0:10, y=aic_for_plot$AIC, xlab="Number of Features", ylab="AIC", main="AIC vs. Number of Features", pch=19, col="red")
abline(v=4)
##CV
cverrs <- best.subset.cv$Subsets[,"CV"]
sdCV<- best.subset.cv$Subsets[,"sdCV"]
CVLo <- cverrs - sdCV
CVHi<- cverrs + sdCV
ymax <- max(CVHi)
ymin <- min(CVLo)
k <- 0:(length(cverrs)-1)
plot(k, cverrs, xlab="Number of Features", ylab="CV Error", ylim=c(ymin,ymax), type="n", main="CV Error vs. Number of Features")
points(k, cverrs, cex=2, col="red", pch=16)
lines(k, cverrs, col="red", lwd=2)
axis(2, yaxp=c(0.6,1.8,6))
segments(k, CVLo, k, CVHi, col="blue", lwd=2)
eps <- 0.15
segments(k-eps, CVLo, k+eps, CVLo, col="blue", lwd=2)
segments(k-eps, CVHi, k+eps, CVHi, col="blue", lwd=2)
#cf. oneSDRule
indBest <- oneSDRule(best.subset.cv$Subsets[,c("CV", "sdCV")])
abline(v=indBest-1, lty=2)
indMin <- which.min(cverrs)
fmin <- sdCV[indMin]
cutOff <- fmin + cverrs[indMin]
abline(h=cutOff, lty=2)
indMin <- which.min(cverrs)
fmin <- sdCV[indMin]
cutOff <- fmin + cverrs[indMin]
##LASSO
plot(cv.logit_lasso) 
title("LASSO - Feature Selection", line = 2.5)


##### LINEAR DISCRIMINANT ANALYSIS #####
##full model
lda.fit.full = lda(is_patient~., data=train_data)
lda.fit.full
#prior probabilities of groups 0 and 1 are 28.1% and 71.9% respectively
#group means show the average of each feature by their class
#coefficients of linear discriminants are the standardized linear combination of the features that are used to determine an observation’s discriminant score 
#the higher the score, the more likely that the classification is 1, i.e. a patient
dev.new()
plot(lda.fit.full, type = "both")
title("Spread of LDA fit across Patient Status")
#a lot of overlap between the 2 groups, indicating that there will be many incorrectly classified observations
#test performance on train_data
lda.full.train.predict = predict(lda.fit.full)
names(lda.full.train.predict)
#use "class" to predict
lda.full.train.pred = lda.full.train.predict$class
table(lda.full.train.pred, train_data$is_patient)
mean(lda.full.train.pred==train_data$is_patient)
#accuracy: 0.728 
#sensitivity: 0.982
#specificity: 0.077
#test performance on test_data
lda.full.test.predict = predict(lda.fit.full, newdata = test_data)
lda.full.test.pred = lda.full.test.predict$class
table(lda.full.test.pred, test_data$is_patient)
mean(lda.full.test.pred==test_data$is_patient)
#accuracy: 0.690 
#sensitivity: 0.975
#specificity: 0.029


##### QUADRATIC DISCRIMINANT ANALYSIS #####
##full model
qda.fit.full = qda(is_patient~., data=train_data)
qda.fit.full
#prior probabilities of groups 0 and 1 are 28.1% and 71.9% respectively
#group means show the average of each feature by their class
#however, qda does not have the coefficients as it is a quadratic function 
#test performance on train_data
qda.full.train.predict = predict(qda.fit.full)
qda.full.train.pred = qda.full.train.predict$class
table(qda.full.train.pred, train_data$is_patient)
mean(qda.full.train.pred==train_data$is_patient)
#accuracy: 0.562
#sensitivity: 0.420
#specificity: 0.923
#test performance on test_data
qda.full.test.predict = predict(qda.fit.full, newdata = test_data)
qda.full.test.pred = qda.full.test.predict$class
table(qda.full.test.pred, test_data$is_patient)
mean(qda.full.test.pred==test_data$is_patient)
#accuracy: 0.552
#sensitivity: 0.383
#specificity: 0.943


##### KNN #####
library(class)
#prepare data
x.knn.train <-model.matrix(is_patient~.-1, data=train_data)
x.knn.test <- model.matrix(is_patient~.-1, data=test_data)
y.knn.train <- train_data$is_patient

#perform knn on different values of k to find k that minimizes test error
k = seq(1:100)
misclass_error = rep(0, 100)
for(k in 1:100){
  set.seed(1)
  knn_pred = knn(x.knn.train, x.knn.test, y.knn.train, k = k) 
  misclass_error[k] = mean(knn_pred != test_data$is_patient) 
}
which(misclass_error==min(misclass_error))
#when k=38, misclass_error is minimized
plot(1:k, misclass_error, pch=19, type="b", ylab="Misclassification Error Rate", xlab = "k")
abline(h=min(misclass_error),col="red")
title("Misclassification Error Rate across Different k for KNN")

#fit KNN with k=38
#get the classification predictions
set.seed(1)
knn_pred = knn(x.knn.train, x.knn.test, y.knn.train, k = 38)
table(knn_pred, test_data$is_patient)
mean(knn_pred == test_data$is_patient)
#accuracy: 0.733
#sensitivity: 0.988
#specificity: 0.143

#get the predicted probabilities
knn_pred_prob = knn(x.knn.train, x.knn.test, y.knn.train, k = 38, prob = T)
knn_pred_prob = attributes(knn_pred_prob)$prob


##### RANDOM FOREST / BAGGING #####
library(randomForest)
#perform random forest with m=3 (done automatically, no need to indicate mtry), trees=500 set automatically
set.seed(1)
rf.fit <- randomForest(is_patient~., data=train_data, importance=TRUE)
rf.fit
#test accuracy on test_data
pred_rf_test_prob = predict(rf.fit, test_data, type = "prob")
pred_rf_test=predict(rf.fit, test_data, type="class")
table(pred_rf_test, test_data$is_patient)
mean(pred_rf_test == test_data$is_patient)
#accuracy: 0.716
#sensitivity: 0.901
#specificity: 0.286
importance(rf.fit)
varImpPlot(rf.fit)
title("Random Forest - Variable Importance Plot", line = 0.8)

#perform bagging with m=p=10
set.seed(1)
bagging.fit <- randomForest(is_patient~., data=train_data, mtry=10, importance=TRUE)
bagging.fit
#test accuracy on test_data
pred_bagging_test_prob=predict(bagging.fit, test_data, type="prob")
pred_bagging_test=predict(bagging.fit, test_data, type="class")
table(pred_bagging_test, test_data$is_patient)
mean(pred_bagging_test == test_data$is_patient)
#accuracy: 0.707
#sensitivity: 0.889
#specificity: 0.286
importance(bagging.fit)
varImpPlot(bagging.fit)
title("Bagging - Variable Importance Plot", line = 0.8)

## Since p=10 here, could try all 10 possible values of `mtry`. Will do so, record the results, and make a plot.
set.seed(1)
oob.err = double(10) #set up variable to record the errors
test.err=double(10)  #set up variable to record the errors
for(mtry in 1:10){
  fit=randomForest(is_patient~. ,data=train_data, mtry=mtry, ntree=500, importance=TRUE) #there is no good reason to choose 400 trees, but chosen here because it is sufficient
  oob.err[mtry]=fit$err.rate[500]
  pred_rf=predict(fit, test_data, type="class")
  test.err[mtry]=mean(pred_rf != test_data$is_patient)
  cat(mtry, " ")
}
matplot(1:mtry,cbind(test.err,oob.err), pch=19, col=c("red","blue"), type="b", ylab="Misclassification Error", xlab="m - Number of Predictors", main="Misclassification Error vs. m")
legend("bottomright", legend=c("Test","OOB"), pch=19, col=c("red","blue"))
abline(v=3)
text(4.7, 0.242, "m = 3 -- Random Forest")
#random forest provides the best predictions


##### BOOSTING #####
library(caret)
set.seed(1)
boost_fit <- caret::train(is_patient ~ .,
                          data = train_data,
                          method = "gbm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 3, 
                                                   verboseIter = FALSE),
                          verbose = 0)
boost_fit
#plot relative influence 
par(mar = c(5, 8, 1, 1))
summary(boost_fit, cBars=10, las = 2)
title("Relative Influence Plot for Gradient Boosting")

#n.trees = 50, interaction.depth = 1, shrinkage = 0.1, n.minobsinnode = 10
caret::confusionMatrix(
  data = predict(boost_fit, test_data),
  reference = test_data$is_patient,
  positive = "1"
)
pred_boosting_test_prob=predict(boost_fit, test_data, type="prob")
#accuracy: 0.681
#sensitivity: 0.901
#specificity: 0.171


##### SVM #####
library(e1071)
#preparing data
x.svm.train <- model.matrix(is_patient~.-1, data=train_data)
x.svm.test <- model.matrix(is_patient~.-1, data=test_data)
train.dat = data.frame(x = x.svm.train, y = train_data$is_patient)
test.dat = data.frame(x = x.svm.test, y = test_data$is_patient)

#fitting SVC
svc = svm(y~., data = train.dat, kernel = "linear", cost = 10, scale = TRUE)
summary(svc)
table(fitted = svc$fitted, true = train.dat$y)
#predicts all non-patients as patients even on training data 
#try to vary cost to see if results can improve
set.seed(1)
tune.svc = tune(svm, y~., data = train.dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.svc)
#errors are the same across all cost values -- linear kernel may not be a good fit to the data

#fitting SVM with radial kernel
set.seed(1)
tune.svm = tune(svm, y~., data = train.dat, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5,1,2,3,4)))
summary(tune.svm)
best.svm <- tune.svm$best.model
tune.svm$best.parameters
#performance on training dataset
table(fitted = best.svm$fitted, true = train.dat$y)
mean(best.svm$fitted==train.dat$y)
#accuracy: 0.955
#sensitivity: 0.997
#specificity: 0.846

#But we have to check performance on test dataset 
pred.svm = predict(best.svm, test.dat)
table(pred = pred.svm, truth=test.dat$y)
mean(pred.svm==test.dat$y)
#accuracy: 0.698
#sensitivity: 0.975
#specificity: 0.057

#for the purpose of plotting ROC curve:
svmfit=svm(y~., data=train.dat, kernel="radial", gamma=4, cost=1, decision.values=T)
svm.radial.decisionvalues <- attributes(predict(svmfit, test.dat, decision.values=T))$decision.values


##### MODEL EVALUATION #####
library(pROC)
#to calculate auc later
roc(predictor = full.model.test.probs, response = test_data$is_patient, positive = 1)$auc
#0.7651
roc(predictor = best.subset.test.bic.probs, response = test_data$is_patient, positive = 1)$auc
#0.7354
roc(predictor = best.subset.test.aic.probs, response = test_data$is_patient, positive = 1)$auc
#0.7432
roc(predictor = best.subset.test.cv.probs, response = test_data$is_patient, positive = 1)$auc
#0.7578
roc(predictor = final_logit_lasso.probs, response = test_data$is_patient, positive = 1)$auc
#0.7199
roc(predictor = lda.full.test.predict$posterior[,1], response = test_data$is_patient, positive = 1)$auc
#0.7295
roc(predictor = qda.full.test.predict$posterior[,1], response = test_data$is_patient, positive = 1)$auc
#0.7460
roc(predictor = knn_pred_prob, response = test_data$is_patient, positive = 1)$auc
#0.7231
roc(predictor = pred_rf_test_prob[,1], response = test_data$is_patient, positive = 1)$auc
#0.7944
roc(predictor = pred_bagging_test_prob[,1], response = test_data$is_patient, positive = 1)$auc
#0.7965
roc(predictor = pred_boosting_test_prob[,1], response = test_data$is_patient, positive = 1)$auc
#0.7545
roc(predictor = svm.radial.decisionvalues, response = test_data$is_patient, positive = 1)$auc
#0.6377

#plot ROC curves
par(pty = "s")
#full logistic model
roc(predictor = full.model.test.probs, response = test_data$is_patient, positive = 1, plot = TRUE, legacy.axes = TRUE, xlab="False Positive Rate / 1-Specificity", ylab="True Positive Rate / Sensitivity", col="red", main="ROC Curves for Multiple Machine Learning Algorithms")
#BIC logistic model
plot.roc(test_data$is_patient, predictor = best.subset.test.bic.probs, positive = 1, add=TRUE, col="chocolate")
#AIC logistic model
plot.roc(test_data$is_patient, predictor = best.subset.test.aic.probs, positive = 1, add=TRUE, col="antiquewhite1")
#CV logistic model
plot.roc(test_data$is_patient, predictor = best.subset.test.cv.probs, positive = 1, add=TRUE, col="chartreuse4")
#LASSO logistic model
plot.roc(test_data$is_patient, predictor = final_logit_lasso.probs, positive = 1, add=TRUE, col="aquamarine4")
#LDA
plot.roc(test_data$is_patient, predictor = lda.full.test.predict$posterior[,1], positive = 1, add=TRUE, col="blue")
#QDA
plot.roc(test_data$is_patient, predictor = qda.full.test.predict$posterior[,1], positive = 1, add=TRUE, col="blue4")
#KNN
plot.roc(test_data$is_patient, predictor = knn_pred_prob, positive = 1, add=TRUE, col="cadetblue1")
#Random Forest
plot.roc(test_data$is_patient, predictor = pred_rf_test_prob[,1], positive = 1, add=TRUE, col="black")
#Bagging
plot.roc(test_data$is_patient, predictor = pred_bagging_test_prob[,1], positive = 1, add=TRUE, col="azure4")
#Boosting
plot.roc(test_data$is_patient, predictor = pred_boosting_test_prob[,1], positive = 1, add=TRUE, col="darkgoldenrod2")
#SVM Radial
plot.roc(test_data$is_patient, predictor = svm.radial.decisionvalues, positive = 1, add=TRUE, col="blueviolet")
legend("bottomright", legend = c("Bagging             : 0.7965", "Random Forest : 0.7944", "Logistic Full       : 0.7651", "Logistic CV        : 0.7578", 
                                 "Boosting            : 0.7545", "QDA                  : 0.7460", "Logistic AIC       : 0.7432", "Logistic BIC       : 0.7354",   
                                 "LDA                   : 0.7295", "KNN                  : 0.7231", "Logistic Lasso   : 0.7199", 
                                 "SVM Radial       : 0.6377"), col=c("azure4","black","red","chartreuse4","darkgoldenrod2","blue4","antiquewhite1","chocolate","blue","cadetblue1","aquamarine4","blueviolet"), lwd = 3, title="AUROC (sorted)")


### FURTHER IMPROVEMENTS ###

## UPSAMPLE TECHNIQUE FOR 2 SELECTED MODELS -- Logistic Regression (BSS using CV), and Random Forest ##
train_data.new <- train_data
train_data.new$is_patient <- factor(train_data.new$is_patient)
set.seed(2)
train_data.upSample <- upSample(x = train_data.new, train_data.new$is_patient)
prop.table(table(train_data.upSample$is_patient))
#0.5 each -- balanced now
str(train_data.upSample)
View(train_data.upSample)
#666 observations of 12 variables, new variable created, i.e. Class, so exclude that in analysis because same as is_patient
train_data.upSample <- train_data.upSample[ ,-12]
sum(train_data.upSample$is_patient==0)
sum(train_data.upSample$is_patient==1)
#333 observations each -- follows the number of majority class in is_patient, i.e. 1

#perform logistic regression - BSS using CV using new dataset, and then test performance on test dataset
train_data.upSample$y = rep(0, 666)
train_data.upSample$y[train_data.upSample$is_patient=="1"]=1
#remove the original is_patient column as now we're replacing it with y
train_data.upSample.cv = train_data.upSample[ ,-11]
#conduct best subset selection using CV
set.seed(2)
best.subset.upSample.cv = bestglm(Xy = train_data.upSample.cv, family=binomial, IC = "CV", CVArgs = list(Method="HTF", K=10, REP=1))
best.subset.upSample.cv
#need to refit in order to predict probabilities
train_data.upSample = train_data.upSample[, -12]
best.subset.fit.upSample.cv = glm(is_patient~age+direct_bilirubin+albumin, data=train_data.upSample, family=binomial)
#test for model fit on train_data
best.subset.train.upSample.cv.probs = predict(best.subset.fit.upSample.cv, type="response")
best.subset.train.upSample.cv.pred=ifelse(best.subset.train.upSample.cv.probs>0.5, 1, 0)
table(best.subset.train.upSample.cv.pred, train_data.upSample$is_patient)
mean(best.subset.train.upSample.cv.pred==train_data.upSample$is_patient)
#accuracy: 0.710 
#sensitivity: 0.583
#specificity: 0.838
#test performance on test_data
best.subset.test.upSample.cv.probs = predict(best.subset.fit.upSample.cv, newdata=test_data, type="response")
best.subset.test.upSample.cv.pred=ifelse(best.subset.test.upSample.cv.probs>0.5, 1, 0)
table(best.subset.test.upSample.cv.pred, test_data$is_patient)
mean(best.subset.test.upSample.cv.pred==test_data$is_patient)
#accuracy: 0.638
#sensitivity: 0.580
#specificity: 0.771

#plot ROC curves to see if there are improvements
par(pty = "s")
roc(predictor = best.subset.test.upSample.cv.probs, response = test_data$is_patient, positive = 1, plot=TRUE, legacy.axes = TRUE, xlab="False Positive Rate / 1-Specificity", ylab="True Positive Rate / Sensitivity", col="blue4", main="ROC Curves Comparing Performance of Logistic Regression Model (BSS = CV) across 2 Sampling Methods")
plot.roc(test_data$is_patient, predictor = best.subset.test.cv.probs, positive = 1, add=TRUE, col="chartreuse4")
legend("bottomright", legend = c("upSample          : 0.7344", "Original Sample : 0.7578"), col=c("blue4","chartreuse4"), lwd = 3, title="AUROC")
#No improvements - model made worse


#perform random forest using new dataset, and then test performance on test dataset
set.seed(1)
rf.fit.upSample <- randomForest(is_patient~., data=train_data.upSample, importance=TRUE)
rf.fit.upSample
#test accuracy on test_data
pred_rf_test.upSample.prob = predict(rf.fit.upSample, test_data, type = "prob")
pred_rf_test.upSample = predict(rf.fit.upSample, test_data, type="class")
table(pred_rf_test.upSample, test_data$is_patient)
mean(pred_rf_test.upSample == test_data$is_patient)
#accuracy: 0.698
#sensitivity: 0.790
#specificity: 0.486
importance(rf.fit.upSample)
varImpPlot(rf.fit.upSample)

#plot ROC curves to see if there are improvements
par(pty = "s")
roc(predictor = pred_rf_test.upSample.prob[,1], response = test_data$is_patient, positive = 1, plot=TRUE, legacy.axes = TRUE, xlab="False Positive Rate / 1-Specificity", ylab="True Positive Rate / Sensitivity", col="blue4", main="ROC Curves Comparing Performance of Random Forest across 2 Sampling Methods")
plot.roc(test_data$is_patient, predictor = pred_rf_test_prob[,1], positive = 1, add=TRUE, col="chartreuse4")
legend("bottomright", legend = c("upSample          : 0.7912", "Original Sample : 0.7944"), col=c("blue4","chartreuse4"), lwd = 3, title="AUROC")
#No improvements - model made slightly worse


## USE NEYMAN-PEARSON CLASSIFIER WITH LOGISTIC REGRESSION AND RANDOM FOREST ##
library(nproc)
#prepare data -- for NP, important class has to be labelled 0, and non-important class is 1
#here, create y that inverts the assignments for is_patient
#training data
train_data.NP <- train_data
train_data.NP$y = rep(2,463)
train_data.NP$y[train_data.NP$is_patient=="1"]=0
train_data.NP$y[train_data.NP$y=="2"]=1
train_data.NP$y <- as.factor(train_data.NP$y)
train_data.NP <- train_data.NP[,-11]
x.train_data.NP <- model.matrix(y~.-1, data = train_data.NP)
#testing data
test_data.NP <- test_data
test_data.NP$y = rep(2,116)
test_data.NP$y[test_data.NP$is_patient=="1"]=0
test_data.NP$y[test_data.NP$y=="2"]=1
test_data.NP$y <- as.factor(test_data.NP$y)
test_data.NP <- test_data.NP[,-11]
str(test_data.NP)
x.test_data.NP <- model.matrix(y~.-1, data = test_data.NP)

#nproc for logistic
nproc_full_logistic = nproc(x=x.train_data.NP, y=train_data.NP$y, method = "logistic", randSeed = 1, delta = 0.5)
plot(nproc_full_logistic, col="black", main="Neyman-Pearson ROC Curves for Different Machine Learning Techniques")
#nproc for random forest
nproc_randomforest = nproc(x=x.train_data.NP, y=train_data.NP$y, method = "randomforest", randSeed = 1, delta = 0.05)
lines(nproc_randomforest, col="red")
#nproc for penalized logistic regression using LASSO
nproc_penlz_logistic = nproc(x=x.train_data.NP, y=train_data.NP$y, method = "penlog", randSeed = 1, delta = 0.5)
lines(nproc_penlz_logistic, col="darkblue")
#nproc for SVM
nproc_svm = nproc(x=x.train_data.NP, y=train_data.NP$y, method = "svm", randSeed = 1, delta = 0.5)
lines(nproc_svm, col="darkgreen")
#nproc for LDA
nproc_lda = nproc(x=x.train_data.NP, y=train_data.NP$y, method = "lda", randSeed = 1, delta = 0.5)
lines(nproc_lda, col="brown")
legend("bottomright", legend = c("Full Logistic", "Random Forest", "Penalized Logistic", "SVM", "LDA"), col=c("black","red","darkblue","darkgreen","brown"), lwd = 3)

#nproc for logistic using variables selected by CV
nproc_logistic_cv <- nproc(x=x.train_data.NP[,c(5,7)], y=train_data.NP$y, method = "logistic", randSeed = 1, delta = 0.05)
nproc_logistic_cv$auc.l
nproc_logistic_cv$auc.u
nproc_randomforest$auc.l
nproc_randomforest$auc.u
compare(nproc_logistic_cv, nproc_randomforest, plot = TRUE)
legend("bottomright", legend = c("Logistic - CV     : 0.589 / 0.742", "Random Forest : 0.570 / 0.727"), col=c("black","red"), lwd = 3, title="AUROC (lower / upper)")
title("Neyman-Pearson ROC Bands")
abline(v=0.136, col="blue", lty=2)
abline(h=0.135, col="blue", lty=2)
abline(h=0.35, col="blue", lty=2)
text(0.21, 0.0, "alpha = 0.136", cex=0.7)
text(0.3, 0.155, "specificity - lower bound = 0.135", cex=0.7)
text(0.3, 0.37, "specificity - upper bound = 0.350", cex=0.7)

## CHOOSING FINAL MODEL ##
#logistic using CV
roc_logistic_cv <- roc(predictor = best.subset.test.cv.probs, response = test_data$is_patient, positive = 1)
plot(roc_logistic_cv, legacy.axes=TRUE)
roc_logistic_cv$auc
#0.7578
# look at TPR and TNR distribution over threshold
matplot(data.frame(roc_logistic_cv$sensitivities, roc_logistic_cv$specificities), x = roc_logistic_cv$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)
title("TPR, TNR vs. Thresholds for Logistic Regression using BSS - CV", cex=0.4)
abline(v=0.5647181, col="blue", lty=2)
text(0.625, 0.0, "Threshold = 0.565")
abline(h=0.40000000, col="blue", lty=2)
text(0.625, 0.38, "Specificity = 0.400")
abline(h=0.86419753, col="blue", lty=2)
text(0.625, 0.88, "Sensitivity = 0.864")


#random forest
roc_randomforest <- roc(predictor = pred_rf_test_prob[,1], response = test_data$is_patient, positive = 1)
plot(roc_randomforest, legacy.axes=TRUE)
roc_randomforest$auc
#0.7944
# look at TPR and TNR distribution over threshold
matplot(data.frame(roc_randomforest$sensitivities, roc_randomforest$specificities), x = roc_randomforest$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR')
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)
title("TPR, TNR vs. Thresholds for Random Forest", cex=0.4)


### to improve the model, use ROC curve and change the threshold
### another way is to assign weights to a specific type of error
### ensure that the split ratios are okay -- done

npc.logistic.cv <- npc(x=x.train_data.NP[,c(5,7)], y=train_data.NP$y, method="logistic", alpha = 0.136, delta=0.05)
npc.logistic.cv.pred = predict(npc.logistic.cv, x.test_data.NP[,c(5,7)])
mean(npc.logistic.cv.pred$pred.label == test_data.NP$y)
table(npc.logistic.cv.pred$pred.label, test_data.NP$y)
head(npc.logistic.cv.pred$pred.score)
head(npc.logistic.cv.pred$pred.label)

classic <- best.subset.test.cv.probs
neyman <- npc.logistic.cv.pred$pred.score
datcomp <- data.frame(classic, neyman)
diffr <- rep(0, 116)
for(i in 1:nrow(datcomp)){
  datcomp$diffr[i] <- 1-datcomp$neyman[i]
}
prop <- rep(0,116)
for(i in 1:nrow(datcomp)){
  datcomp$prop[i] <- datcomp$diffr[i]/datcomp$classic[i]
}
View(datcomp)
min(datcomp$prop)
mean(datcomp$prop)
max(datcomp$prop)


##### FINAL MODEL #####
## 2 MODELS FOR 2 PURPOSES ##

#for classification, use npc with logistic regression but using subset of variables as selected by CV
#it has class predictions and scores -- write how the scores are obtained
#however, there are no coefficients -- after comparing the results of both, for the purpose of model interpretation, use classical logistic regression
#but for classification purpose, use npc as it guarantees you can achieve the type I error rate specified at population level 

classic <- ifelse(best.subset.test.cv.probs>0.565, 1, 0)
neyman <- npc.logistic.cv.pred$pred.label
datcomp <- data.frame(classic, neyman)
sum(datcomp$classic==datcomp$neyman)
dim(datcomp)

summary(best.subset.fit.cv)


