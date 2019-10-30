#################################
#PREDICTING SPATIAL ABILITIES USING PSYCHOLOGICAL FEATURES
#-STATISTICAL LEARNING MOD B-
#-GRIMALDI FRANCESCO-
#################################


#########################
##1. PRELIMINAR OPERATIONS
#########################
myData <- read.csv(file = "C:\\Users\\fgrim\\Documents\\Università\\Data Science\\1.2 Statistical Learning B\\Project\\Dataset.csv")
#1.1 Visualize the dataset
View(myData)
summary(myData)
#1.2 Fix errors in dataset
#1.2.a Take out last two rows (mean and sd of features)
myData <- myData[-c(223, 224), ]
View(myData)
#1.2.b Take out NAs features columns
myData <- myData[1:29]
View(myData)
#1.2.c Look for NA values in the dataset
any(is.na.data.frame(myData)) #Check if there is any
which(is.na(myData), arr.ind = T) #Get the positions of NA's in the dataset
##A possibility is to drop the rows with NA's or to fill NA's with the mean.
##In case of mean we can use:
myData[which(is.na(myData), arr.ind = T)[1:7], 26] <- mean(as.numeric(unlist(myData[26])), na.rm = TRUE)
##In case of "drop" we can use:
##myData <- myData[-which(is.na(myData), arr.ind = T)[1:7], ]
#1.3 Transforming to factor the categorical variables
myData$GENDER <- as.factor(myData$GENDER)
#myData$Map.Use <- as.factor(myData$Map.Use)
#myData$GPS.Use <- as.factor(myData$GPS.Use)
#myData$Verbal.Indication <- as.factor(myData$Verbal.Indication)

##############################
##2. EXPLORATIVE DATA ANALYSIS
##############################
par(mfrow = c(2,2))
#2.1 Checking distribution of the data
for (i in 1:29){
  if (is.numeric(myData[, i])){
    hist(myData[, i], main = names(myData[i]), col = "steelblue", xlab = names(myData[i]))}
  else{
    plot(myData[, i], main = names(myData[i]), col = "steelblue")
  }}

#2.2 Checking normality with qqnorm
for (i in 2:29){
  if (is.numeric(myData[, i])){
    qqnorm(myData[, i], main = names(myData[i]), col='steelblue')}}

#2.3 Applying log transformation to PPT
par(mfrow = c(2, 2))
qqnorm(myData$PTT_tot, main = "Before log transformation")
hist(myData$PTT_tot, main = "Before log transformation", col='steelblue', xlab = "PTT")
qqnorm(log(myData$PTT_tot), main = "After log transformation")
hist(log(myData$PTT_tot), main = "After log transformation", col='steelblue', xlab = "log.PTT")
par(mfrow = c(1, 1))
myData["logPTT"] <- log(myData$PTT_tot)
summary(myData$logPTT)

#2.4 Transforming logPTT and MRT in binary variable
myData["BinaryPTT"] <- myData$logPTT
M <- mean(myData$logPTT)
myData$BinaryPTT[myData$BinaryPTT <= M] = 1
myData$BinaryPTT[myData$BinaryPTT > M] = 0
myData["BinaryMRT"] <- myData$MRT_TOT
M <- mean(myData$MRT_TOT)
myData$BinaryMRT[myData$BinaryMRT <= M] = 0
myData$BinaryMRT[myData$BinaryMRT > M] = 1

########################
##3. DATA REDUCTION
########################
#3.1 Removing Big Traits of Personality
InitData <- myData
par(mfrow = c(2, 3))
plot(myData$EXTRAVERSION, myData$X1.DYNAMISM + myData$X1.DOMINANCE, 
     main = "Collinearity between Facets and their Trait", 
     xlab = "Extraversion", ylab = "Facets", col='steelblue', pch = 20)
plot(myData$AGREEABLENESS, myData$X2.CORDIALITY + myData$X2.COOPERATIVITY, 
     main = "Collinearity between Facets and their Trait", 
     xlab = "Extraversion", ylab = "Facets", col='steelblue', pch = 20)
plot(myData$CONSCIENTIOUSNESS, myData$X3.PERSEVERANCE + myData$X3.SCRUPOLOSITY, 
     main = "Collinearity between Facets and their Trait", 
     xlab = "Extraversion", ylab = "Facets", col='steelblue', pch = 20)
plot(myData$EMOTIONAL.STABILITY, myData$X4.EMOTION.CONTROL + myData$X4.PULSE.CONTROL, 
     main = "Collinearity between Facets and their Trait", 
     xlab = "Extraversion", ylab = "Facets", col='steelblue', pch = 20)
plot(myData$MENTAL.OPPENNESS, myData$X5.CULTURE.OPENING + myData$X5.EXPERIENCE.OPENING, 
     main = "Collinearity between Facets and their Trait", 
     xlab = "Extraversion", ylab = "Facets", col='steelblue', pch = 20)
myData <- myData[, -c(2:6)]
View(myData)

#3.2 Removing Positive and Negative Factors
par(mfrow = c(1, 2))
plot(myData$Positive.Factor, myData$QACOexploration + myData$LandmarkRouteMode + myData$SenseOfDirection + myData$CardinalPoints, 
     main = "Collinearity of Positive Factor", xlab = "Positive Factor", ylab = "Original Dimensions",
     col='steelblue', pch = 20)
plot(myData$Negative.Factor, myData$QAS + myData$QACOknown, 
     main = "Collinearity of Negative Factor", xlab = "Negative Factor", ylab = "Original Dimensions",
     col='steelblue', pch = 20)
myData <- myData[, -c(23:24)]
View(myData)

#3.3 Plotting MRT and PTT against other variables
par(mfrow = c(1, 2))
for(i in 1:20){
  if(is.numeric(myData[, i]) == T){
    plot(y = jitter(myData$MRT_TOT), jitter(x = myData[, i]),
         main = paste("R:", round(cor(myData$MRT_TOT, (myData[, i])), 2)),
         xlab = paste(names(myData[i])), ylab = "MRT",
         col='steelblue', pch = 20)
    abline(lm(myData$MRT_TOT~myData[, i]), lwd = 2.8)
    #lines(lowess(myData[, i], myData$logPTT), col = "red")
    plot(y = jitter(myData$logPTT), jitter(x = myData[, i]),
         main = paste("R:", round(cor(myData$logPTT, (myData[, i])), 2)),
         xlab = paste(names(myData[i])), ylab = "PTT",
         col='steelblue', pch = 20)
    abline(lm(myData$logPTT~myData[, i]), lwd = 2.8)
    #lines(lowess(myData[, i], myData$logPTT), col = "red")
  }else{
    plot(y = myData$MRT_TOT, x = myData[, i],
         main = paste(names(myData[i]), "-", "MRT"),
         xlab = paste(names(myData[i])), ylab = "MRT", col='steelblue')
    plot(y = myData$logPTT, x = myData[, i],
         main = paste(names(myData[i]), "-", "PTT"),
         xlab = paste(names(myData[i])), ylab = "PTT", col='steelblue')}
}
for(i in 1:20){
  if(is.factor(myData[, i]) == TRUE){
    plot(x = as.factor(myData$BinaryMRT), y = myData[, i],
         main = paste(names(myData[i]), "-", "MRT"),
         ylab = paste(names(myData[i])), xlab = "MRT")
    plot(x = as.factor(myData$BinaryPTT), y = myData[, i],
         main = paste(names(myData[i]), "-", "PTT"),
         ylab = paste(names(myData[i])), xlab = "PTT")
  }else{
    plot(x = as.factor(myData$BinaryMRT), y = myData[, i],
         main = paste(names(myData[i]), "-", "MRT"),
         ylab = paste(names(myData[i])), xlab = "MRT", col='steelblue')
    plot(x = as.factor(myData$BinaryPTT), y = myData[, i],
         main = paste(names(myData[i]), "-", "PTT"),
         ylab = paste(names(myData[i])), xlab = "PTT", col='steelblue')}
}
par(mfrow = c(1, 1))
##4 MODEL SELECTION
library(class)
library(MASS)
library(pROC)
library(boot)
library(caret)
library(glmnet)

X <- myData[, 1:20]
InitX <- InitData[, -c(21, 22, 25, 26)]
toNumeric <- function(X){
  for (i in 1:length(X)){
    X[, i] <- as.numeric(X[, i])}
  return(X)
}
Xf <- X
X <- toNumeric(X)
standardized.X <- scale(X[,1:20])
PTT <- as.factor(myData[,24])
MRT <- as.factor(myData[,25])

######################################
##4. K-NEAREST NEIGHBOURS (KNN)
######################################
#Using Leave-One-Out-Cross-Validation procedure
options(warn=-1)
set.seed(1)
#Testing 10 model with different k
knn.results <- function(X, y, from = 1, to = 10){
  results <- vector(mode = "numeric", length = to - from + 1)
  pvalue <- vector(mode = "numeric", length = to - from + 1)
  max_acc = 0
  best_k = -1
  best_res = 0
  for (i in from:to){
    res <- knn.cv(X, y, k = i)
    cm <- confusionMatrix(res, y)
    results[i] <- cm$overall[1]
    pvalue[i] <- cm$overall[6]
    if(results[i]>max_acc){
      max_acc <- results[i]
      best_k <- i
      best_res <- res}
  }
  Predicted <- best_res
  Truth <- y
  fourfoldplot(table(Predicted, Truth), main = paste("K-NN Confusion Matrix of", deparse(substitute(y)), "with K =" , best_k))
  pROC <- roc(as.numeric(Predicted),as.numeric(Truth),
              smoothed = TRUE,
              # arguments for ci
              ci=TRUE, ci.alpha=0.9, stratified=FALSE,
              # arguments for plot
              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
              print.auc=TRUE, show.thres=TRUE, main = paste("ROC of K-NN:", deparse(substitute(y))))
  sens.ci <- ci.se(pROC)
  plot(sens.ci, type="shape", col="steelblue")
  plot(sens.ci, type="bars")
  return(cbind(results, pvalue))
}
set.seed(1)
par(mfrow = c(1, 2))
MRT.knn <- round(knn.results(standardized.X, MRT, to = 25), 3)
par(mfrow = c(1, 1))
paste("Accuracy and probability of model performin like a raondom classificator for MRT from k = 1 to k = 15 and LOOVC procedure: ")
MRT.knn[, c(1,2)]
qplot(seq(1:25), MRT.knn[, 1], main = "K-NN score by K: MRT", xlab = "Number of k", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(MRT)[2]/length(MRT), col = "steelblue", lwd = 1.5)
par(mfrow = c(1, 2))
PTT.knn <- round(knn.results(standardized.X, PTT, to = 25), 3)
par(mfrow = c(1, 1))
paste("Accuracy and probability of model performin like a raondom classificator for PTT from k = 1 to k = 15 and LOOVC procedure: ")
PTT.knn[, c(1,2)]
qplot(seq(1:25), PTT.knn[, 1], main = "K-NN score by K: PTT", xlab = "Number of k", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(PTT)[1]/length(PTT), col = "steelblue", lwd = 1.5)

######################################
#5. LINEAR DISCRIMINANT ANALYSIS (LDA)
######################################
#5.1) Testing the Full-Model
#MRT
formula.MRT = MRT~.
lda.class.MRT <- lda(formula.MRT, as.data.frame(X), CV = T)$class
table.LDA.MRT <- confusionMatrix(lda.class.MRT,MRT)
table.LDA.MRT$table
Accuracy.LDA.MRT <- table.LDA.MRT$overall[1]
Pvalue.LDA.MRT <- table.LDA.MRT$overall[6]
#PPT
formula.PTT = PTT~.
lda.class.PTT <- lda(formula.PTT, as.data.frame(X), CV = T)$class
table.LDA.PTT <- confusionMatrix(lda.class.PTT,PTT)
table.LDA.PTT$table
Accuracy.LDA.PTT <- table.LDA.PTT$overall[1]
Pvalue.LDA.PTT <- table.LDA.PTT$overall[6]
#Result
paste("Accuracy and pvalue on MRT with a LDA and a LOOCV procedure")
round(cbind(Accuracy.LDA.MRT, Pvalue.LDA.MRT), 3)
paste("Accuracy and pvalue on PTT with a LDA and a LOOCV procedure")
round(cbind(Accuracy.LDA.PTT, Pvalue.LDA.PTT), 3)
#PLOT ROC AND CONFUSION MATRIX
options(warn=-1)
Predicted = lda.class.MRT
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: full-features LDA MRT")
pROC_MRT <- roc(as.numeric(lda.class.MRT),as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-features LDA MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

Predicted = lda.class.PTT
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: full-features LDA PTT")
pROC_PTT <- roc(as.numeric(lda.class.PTT),as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.90, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-featuers LDA PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
options(warn=0)

#5.2) Features Selection
lda.fit.MRT <- lda(formula.MRT, data = as.data.frame(X))
lda.fit.PTT <- lda(formula.PTT, data = as.data.frame(X))
lda.means.MRT <- lda.fit.MRT$means
lda.means.PTT <- lda.fit.PTT$means
#Now we want to see how big is the distance bewtween the means
lda.diff.MRT <- vector(mode = "numeric", length = 20)
lda.diff.PTT <- vector(mode = "numeric", length = 20)
for (i in 1:19){
  lda.diff.MRT[i] <- abs((lda.means.MRT[, i][1] - (lda.means.MRT[, i][2])))/(lda.means.MRT[, i][1] + (lda.means.MRT[, i][2]))
  lda.diff.PTT[i] <- abs((lda.means.PTT[, i][1] - (lda.means.PTT[, i][2])))/(lda.means.PTT[, i][1] + (lda.means.PTT[, i][2]))}
round(lda.diff.MRT, 3)
round(lda.diff.PTT, 3)
Init.diff.MRT.lda <- lda.diff.MRT
Init.diff.PTT.lda <- lda.diff.PTT
#5.3) Using a backward step-wise features selection
lda.diff.MRT <- Init.diff.MRT.lda
lda.diff.PTT <- Init.diff.PTT.lda
MRT.features <- seq(1:20)
PTT.features <- seq(1:20)
x_vec.MRT <- seq(from = 2, to = 20)
x_vec.PTT <- seq(from = 2, to = 20)
y_vec.MRT <- vector(mode = "numeric", length = 19)
y_vec.PTT <- vector(mode = "numeric", length = 19)
#For every step we take out the feature with the lowest difference between its group means
y_vec.MRT[19] <- Accuracy.LDA.MRT
y_vec.PTT[19] <- Accuracy.LDA.PTT
for(i in (1:18)){
  #Find out the "lowest" feature
  MRT.features <- MRT.features[lda.diff.MRT > min(lda.diff.MRT)]
  PTT.features <- PTT.features[lda.diff.PTT > min(lda.diff.PTT)]
  lda.diff.MRT <- lda.diff.MRT[lda.diff.MRT > min(lda.diff.MRT)]
  lda.diff.PTT <- lda.diff.PTT[lda.diff.PTT > min(lda.diff.PTT)]
  #Use the remaining features
  reduced.Data.MRT <- X[, MRT.features]
  reduced.Data.PTT <- X[, PTT.features]
  #Use leave-one-out approach for validation
  lda.class.MRT.reduced <- lda(formula = formula.MRT, data = as.data.frame(reduced.Data.MRT), CV = T)$class
  lda.class.PTT.reduced <- lda(formula = formula.PTT, data = as.data.frame(reduced.Data.PTT), CV = T)$class
  #Store the Accuracy results in vectors
  table.LDA.MRT.reduced <- table(lda.class.MRT.reduced, MRT)
  table.LDA.PTT.reduced <- table(lda.class.PTT.reduced, PTT)
  y_vec.MRT[19-i] <- (table.LDA.MRT.reduced[1] + table.LDA.MRT.reduced[4])/length(MRT)
  y_vec.PTT[19-i] <- (table.LDA.PTT.reduced[1] + table.LDA.PTT.reduced[4])/length(PTT)}
#Plot the reasults
qplot(x_vec.MRT[1:19], y_vec.MRT[1:19], main = "LDA score by features: MRT", xlab = "Number of features", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(MRT)[2]/length(MRT), col = "steelblue", lwd = 1.5)
qplot(x_vec.PTT[1:19], y_vec.PTT[1:19], main = "LDA score by features: PTT", xlab = "Number of features", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(PTT)[1]/length(PTT), col = "steelblue", lwd = 1.5)
#Get the accuracy of a model with the best k features
lda.score.K <- function(X, y, lda.means.diff, k, formula){
  lda.diff <- lda.means.diff
  features <- seq(1:20)
  for (i in 1:(20-k)){
    features <- features[lda.diff > min(lda.diff)]
    lda.diff <- lda.diff[lda.diff > min(lda.diff)]}
  reduced.Data <- X[, features]
  lda.class.reduced <- lda(formula = formula, data = as.data.frame(reduced.Data), CV = T)$class
  table.LDA.reduced <- table(lda.class.reduced, y) 
  accuracy <- (table.LDA.reduced[1] + table.LDA.reduced[4])/length(y)
  return(c(names(reduced.Data), accuracy, c(lda.class.reduced)))}
#Eg: best #features for PTT = 4, best #features for MRT = 6
res.PTT <- lda.score.K(X, PTT, lda.means.diff = Init.diff.PTT.lda, formula = formula.PTT, 4)
res.MRT <- lda.score.K(X, MRT, lda.means.diff = Init.diff.MRT.lda, formula = formula.MRT, 6)

#MRT
res.MRT[1:6]
round(as.numeric(res.MRT[7]), 3)
Predicted = res.MRT[8:length(res.MRT)]
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: 6-features LDA MRT")
#ROC
options(warn=-1)
pROC_MRT <- roc(as.numeric(res.MRT[8:length(res.MRT)]),as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 6-features LDA MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

#PTT
res.PTT[1:4]
round(as.numeric(res.PTT[5]), 3)
#Confusion-Matrix
Predicted = res.PTT[6:length(res.PTT)]
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: 4-features LDA PTT")
pROC_PTT <- roc(as.numeric(res.PTT[6:length(res.PTT)]),as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 4-features LDA PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
options(warn=0)

##########################################
##6. QAUDRATIC DISCRIMANAT ANALYSIS (QDA)
##########################################
optionS(warn = -1)
par(mfrow=c(2,2))
formula = MRT~.
fit <- qda(formula, X)
class <- predict(fit, X)
Predicted <- class$class
Truth <- MRT
table <- table(class$class, MRT)
fourfoldplot(table(Predicted, Truth), main = paste("QDA-MRT Accuracy:", round(confusionMatrix(table)$overall[1], 3)))

formula = PTT~.
fit <- qda(formula, X)
class <- predict(fit, X)
Predicted <- class$class
Truth <- PTT
table <- table(class$class, PTT)
fourfoldplot(table(Predicted, Truth), main = paste("QDA-PTT Accuracy:", round(confusionMatrix(table)$overall[1], 3)))
par(mfrow=c(1,1))

#6.1 Full model: Training vs LOOCV
par(mfrow=c(2,2))
formula = MRT~.
fit <- qda(formula, X)
class <- predict(fit, X)
Predicted <- class$class
Truth <- MRT
table <- table(class$class, MRT)
fourfoldplot(table(Predicted, Truth), main = paste("QDA-MRT Training Accuracy:", round(confusionMatrix(table)$overall[1], 3)))

formula = MRT~.
qda.class.MRT <- qda(formula, X, CV = T)$class
table.QDA.MRT <- table(qda.class.MRT,MRT)
p.value = confusionMatrix(table.QDA.MRT)$overall[6]
p.value
table.QDA.MRT
Accuracy.QDA.MRT <- (table.QDA.MRT[1] + table.QDA.MRT[4])/length(MRT)
Predicted = qda.class.MRT
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = paste("QDA-MRT LOOCV Accuracy:", round(Accuracy.QDA.MRT,3)))

formula = PTT~.
fit <- qda(formula, X)
class <- predict(fit, X)
Predicted <- class$class
Truth <- PTT
table <- table(class$class, PTT)
fourfoldplot(table(Predicted, Truth), main = paste("QDA-PTT Training Accuracy:", round(confusionMatrix(table)$overall[1], 3)))

formula = PTT~.
qda.class.PTT <- qda(formula, X, CV = T)$class
table.QDA.PTT <- table(qda.class.PTT,PTT)
p.value = confusionMatrix(table.QDA.PTT)$overall[6]
p.value
Accuracy.QDA.PTT <- (table.QDA.PTT[1] + table.QDA.PTT[4])/length(PTT)
Predicted = qda.class.PTT
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = paste("QDA-PTT LOOCV Accuracy:", round(Accuracy.QDA.PTT,3)))
par(mfrow=c(1,1))
#ROC
options(warn=-1)
pROC_MRT <- roc(as.numeric(Predicted),as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-features QDA MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

pROC_PTT <- roc(as.numeric(Predicted),as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-features QDA PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
options(warn=0)

#6.2 Features Selection
qda.fit.MRT <- qda(formula.MRT, data = as.data.frame(X))
qda.fit.PTT <- qda(formula.PTT, data = as.data.frame(X))
qda.means.MRT <- qda.fit.MRT$means
qda.means.PTT <- qda.fit.PTT$means
#Now we want to see how big is the distance bewtween the means
qda.diff.MRT <- vector(mode = "numeric", length = 20)
qda.diff.PTT <- vector(mode = "numeric", length = 20)
for (i in 1:19){
  qda.diff.MRT[i] <- abs((qda.means.MRT[, i][1] - (qda.means.MRT[, i][2])))/(qda.means.MRT[, i][1] + (qda.means.MRT[, i][2]))
  qda.diff.PTT[i] <- abs((qda.means.PTT[, i][1] - (qda.means.PTT[, i][2])))/(qda.means.PTT[, i][1] + (qda.means.PTT[, i][2]))}
round(qda.diff.MRT, 3)
round(qda.diff.PTT, 3)
Init.diff.MRT.qda <- qda.diff.MRT
Init.diff.PTT.qda <- qda.diff.PTT

#6.3 Use a step-back feature selection
qda.diff.MRT <- Init.diff.MRT.qda
qda.diff.PTT <- Init.diff.PTT.qda
MRT.features <- seq(1:20)
PTT.features <- seq(1:20)
x_vec.MRT <- seq(from = 2, to = 20)
x_vec.PTT <- seq(from = 2, to = 20)
y_vec.MRT <- vector(mode = "numeric", length = 19)
y_vec.PTT <- vector(mode = "numeric", length = 19)
#For every step we take out the feature with the lowest difference between its group means
y_vec.MRT[19] <- Accuracy.QDA.MRT
y_vec.PTT[19] <- Accuracy.QDA.PTT
for(i in (1:18)){
  #Find out the "lowest" feature
  MRT.features <- MRT.features[qda.diff.MRT > min(qda.diff.MRT)]
  PTT.features <- PTT.features[qda.diff.PTT > min(qda.diff.PTT)]
  qda.diff.MRT <- qda.diff.MRT[qda.diff.MRT > min(qda.diff.MRT)]
  qda.diff.PTT <- qda.diff.PTT[qda.diff.PTT > min(qda.diff.PTT)]
  #Use the remaining features
  reduced.Data.MRT <- X[, MRT.features]
  reduced.Data.PTT <- X[, PTT.features]
  #Use leave-one-out approach for validation
  qda.class.MRT.reduced <- qda(formula = formula.MRT, data = as.data.frame(reduced.Data.MRT), CV = T)$class
  qda.class.PTT.reduced <- qda(formula = formula.PTT, data = as.data.frame(reduced.Data.PTT), CV = T)$class
  #Store the Accuracy results in vectors
  table.QDA.MRT.reduced <- table(qda.class.MRT.reduced, MRT)
  table.QDA.PTT.reduced <- table(qda.class.PTT.reduced, PTT)
  y_vec.MRT[19-i] <- (table.QDA.MRT.reduced[1] + table.QDA.MRT.reduced[4])/length(MRT)
  y_vec.PTT[19-i] <- (table.QDA.PTT.reduced[1] + table.QDA.PTT.reduced[4])/length(PTT)}
#Plot the reasults
qplot(x_vec.MRT[1:19], y_vec.MRT[1:19], main = "QDA Score by #features: MRT", xlab = "Number of features", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(MRT)[2]/length(MRT), col = "steelblue", lwd = 1.5)
qplot(x_vec.PTT[1:19], y_vec.PTT[1:19], main = "QDA Score by #features: PTT", xlab = "Number of features", ylab = "Accuracy with LOOCV") + geom_hline(yintercept=1-summary(PTT)[1]/length(PTT), col = "steelblue", lwd = 1.5)

#6.4 Get result and features of QDA model with k features
qda.score.K <- function(X, y, qda.means.diff, k, formula){
  qda.diff <- qda.means.diff
  features <- seq(1:20)
  for (i in 1:(20-k)){
    features <- features[qda.diff > min(qda.diff)]
    qda.diff <- qda.diff[qda.diff > min(qda.diff)]}
  reduced.Data <- X[, features]
  qda.class.reduced <- qda(formula = formula, data = as.data.frame(reduced.Data), CV = T)$class
  table.QDA.reduced <- table(qda.class.reduced, y) 
  accuracy <- (table.QDA.reduced[1] + table.QDA.reduced[4])/length(y)
  return(c(names(reduced.Data), accuracy, qda.class.reduced))}
#Eg: best #features for PTT = 4, best #features for MRT = 2
#RES
par(mfrow=c(1,2))
#MRT
res.MRT <- qda.score.K(X, MRT, qda.means.diff = Init.diff.MRT.qda, formula = formula.MRT, 2)
paste("Best features for QDA-MRT")
res.MRT[1:2]
paste("Accuracy of QDA-MRT reduced model")
round(as.numeric(res.MRT[3]), 6)
Predicted = as.numeric(res.MRT[4:length(res.MRT)]) - 1
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: two-features QDA MRT")
#ROC
options(warn=-1)
pROC_MRT <- roc(as.numeric(Predicted),as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: two-features QDA MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

#PTT
res.PTT <- qda.score.K(X, PTT, qda.means.diff = Init.diff.PTT.qda, formula = formula.PTT, 4)
paste("Best features for QDA-PTT:")
res.PTT[1:4]
paste("Accuracy of QDA-PTT reduced model")
round(as.numeric(res.PTT[5]), 3)
#Confusion-Matrix
Predicted = as.numeric(res.PTT[6:length(res.PTT)]) - 1
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: four-features QDA PTT")
pROC_PTT <- roc(as.numeric(Predicted),as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: four-features QDA PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
par(mfrow=c(1,1))


##############################
##7. LOGISTIC CLASSIFIER (GLM)
##############################
#7.1 Full-GLM with LOOCV
LOOCV.glm <- function(formula, data, y, family = "binomial"){
  probs <- vector("numeric", length(y))
  for (i in 1:length(y)){
    glm.fit <- glm(formula, data = as.data.frame(data), family = family, subset = seq(1:222)[-i])
    probs[i] <- predict(glm.fit, data[i, 1:length(data)], type = "response")}
  return(probs)}
#Getting probabilities
X <- Xf

#Full Model
set.seed(1)
MRT.m <- data.matrix(MRT)
PTT.m <- data.matrix(PTT)
Xm   <- data.matrix(X)
probs.MRT <- predict(cv.glmnet(y = MRT.m, x = Xm, family="binomial"), Xm, s = "lambda.min", type = "response")
probs.PTT <- predict(cv.glmnet(y = PTT.m, x = Xm, family="binomial"), Xm, s = "lambda.min", type = "response")
probs.MRT[probs.MRT > summary(MRT)[2]/length(MRT)] <- 1
probs.MRT[probs.MRT <= summary(MRT)[1]/length(MRT)] <- 0
probs.PTT[probs.PTT > summary(PTT)[1]/length(PTT)] <- 1
probs.PTT[probs.PTT <= summary(PTT)[2]/length(PTT)] <- 0
cf.GLM.MRT <- confusionMatrix(as.factor(probs.MRT), MRT)
cf.GLM.PTT <- confusionMatrix(as.factor(probs.PTT), PTT)
par(mfrow=c(2,2))
#Result
Accuracy.GLM.MRT <-cf.GLM.MRT$overall[1]
pvalue.GLM.MRT <-cf.GLM.MRT$overall[6]
paste("Accuracyon MRT with a full GLM model and a LOOCV procedure", round(Accuracy.GLM.MRT, 3))
paste("Pvalue on MRT with a full GLM model and a LOOCV procedure", round(pvalue.GLM.MRT, 3))
#Getting ROC and Confusion Matrix
Predicted = probs.MRT
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: full-GLM MRT")
options(warn=-1)
pROC_MRT <- roc(probs.MRT,as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-GLM MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

#PTT
Accuracy.GLM.PTT <-cf.GLM.PTT$overall[1]
pvalue.GLM.PTT <-cf.GLM.PTT$overall[6]
paste("Accuracyon PTT with a full GLM model and a LOOCV procedure", round(Accuracy.GLM.PTT, 2))
paste("Pvalue on PTT with a full GLM model and a LOOCV procedure", round(pvalue.GLM.PTT, 2))
Predicted = probs.PTT
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: full-GLM PTT")
pROC_PTT <- roc(probs.PTT,as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-GLM PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
cf.GLM.MRT
cf.GLM.PTT

#latex table
ptt_table <- summary(glm(formula = formula.PTT, data = X, family = "binomial"))
mrt_table <- summary(glm(formula = formula.MRT, data = X, family = "binomial"))
mrt_anova <- anova(glm(formula = MRT ~ 1, data = X, family = binomial), 
                   glm(formula = formula.MRT, data = X, family = binomial),
                   test = "Chisq")
ptt_anova <- anova(glm(formula = PTT ~ 1, data = X, family = binomial), 
                   glm(formula = formula.PTT, data = X, family = binomial), 
                   test = "Chisq")
library(xtable)
print(xtable(mrt_table))
print(xtable(mrt_anova))
print(xtable(ptt_table))
print(xtable(ptt_anova))

#7.2 Feature Selection
full.MRT<- glm(MRT~.,  data=X, family=binomial) 
empty.MRT<- glm(MRT~1, data=X, family=binomial)
full.PTT<- glm(PTT~.,  data=X, family=binomial) 
empty.PTT<- glm(PTT~1, data=X, family=binomial)
step.mod.MRT <- step(empty.MRT, steps=10, trace=2, scope=list(lower=formula(empty.MRT), upper=formula(full.MRT)), direction="forward")
step.mod.PTT <- step(empty.PTT, steps=10, trace=2, scope=list(lower=formula(empty.PTT), upper=formula(full.PTT)), direction="forward")
summary(step.mod.MRT)
summary(step.mod.PTT)

#Displaying AIC and order of features
namesX.PTT <- names(as.data.frame(X))
namesX.MRT <- names(as.data.frame(X))
final.names.PTT <- c()
final.names.MRT <- c()
#Data: It will increase of one features every search is completed
PTT.X <- 0
MRT.X <- 0
#Vector of AIC
AIC.PTT <- vector(mode = "numeric", length = 20)
AIC.MRT <- vector(mode = "numeric", length = 20)
for (k in 1:20){
  #Here we will keep every aic
  best_aic.ptt <- vector(mode = "numeric", length = 21-k)
  best_aic.mrt <- vector(mode = "numeric", length = 21-k)
  for(i in 1:(21-k)){
    #Temporary dataset
    if (k==1){
      temp.PTT.X <- as.data.frame(cbind(PTT.X, X[, namesX.PTT[i]])[,-1])
      temp.MRT.X <- as.data.frame(cbind(MRT.X, X[, namesX.MRT[i]])[,-1])}
    else{
      temp.PTT.X <- as.data.frame(cbind(PTT.X, X[, namesX.PTT[i]]))
      temp.MRT.X <- as.data.frame(cbind(MRT.X, X[, namesX.MRT[i]]))
    }
    #Compute the accuracy
    glm.mrt <- glm(formula = MRT~., data = as.data.frame(temp.MRT.X), family = "binomial")
    glm.ptt <- glm(formula = PTT~., data = as.data.frame(temp.PTT.X), family = "binomial")
    #Compute AIC
    best_aic.mrt[i] <- glm.mrt$aic
    best_aic.ptt[i] <- glm.ptt$aic
  }
  #Compute where is the best aic
  mrt.i <- which.min(best_aic.mrt)
  ptt.i <- which.min(best_aic.ptt)
  #update dataset, we take the prevoius and we add the column of X which name is the same of the name of the best feat
  PTT.X <- cbind(PTT.X, X[, namesX.PTT[ptt.i]])
  MRT.X <- cbind(MRT.X, X[, namesX.MRT[mrt.i]])
  final.names.PTT <- c(final.names.PTT, namesX.PTT[ptt.i])
  final.names.MRT <- c(final.names.MRT, namesX.MRT[mrt.i])
  #Take out the features name from the list name 
  namesX.PTT <- namesX.PTT[-ptt.i]
  namesX.MRT <- namesX.MRT[-mrt.i]
  #Update AIC and ACC vectors
  AIC.PTT[k] <- best_aic.ptt[ptt.i]
  AIC.MRT[k] <- best_aic.mrt[mrt.i]
}

paste("Order of importance of features for MRT:")
final.names.MRT
qplot(x=seq(1:20), y=AIC.MRT, ylab = "AIC", xlab = "Numbers of features", main = "AIC for MRT in relation of the number of features")
paste("Order of importance of features for PTT:")
final.names.PTT
qplot(x=seq(1:20), y=AIC.PTT, ylab = "AIC", xlab = "Numbers of features", main = "AIC for PTT in relation of the number of features")

set.seed(1)
#7.3 Testing the selected features
new.Data.MRT <- data.matrix(X[, c(1, 8)])
new.Data.PTT <- data.matrix(X[, c(7, 8, 10, 11 ,18)])
#Getting the posterior probabilities
probs.MRT.red <- predict(cv.glmnet(y = MRT.m, x = new.Data.MRT, family="binomial"), new.Data.MRT, s = "lambda.min", type = "response")
probs.PTT.red <- predict(cv.glmnet(y = PTT.m, x = new.Data.PTT, family="binomial"), new.Data.PTT, s = "lambda.min", type = "response")
probs.MRT.red[probs.MRT.red > 0.5] <- 1
probs.MRT.red[probs.MRT.red <= 0.5] <- 0
probs.PTT.red[probs.PTT.red > 0.5] <- 1
probs.PTT.red[probs.PTT.red <= 0.5] <- 0
#Displaying Confusion Matrix
cf.GLM.MRT.red <- confusionMatrix(as.factor(probs.MRT.red), MRT)
cf.GLM.PTT.red <- confusionMatrix(as.factor(probs.PTT.red), PTT)
#Result
Accuracy.GLM.MRT.red <-cf.GLM.MRT.red$overall[1]
pvalue.GLM.MRT.red <-cf.GLM.MRT.red$overall[6]
options(warn=-1)
paste("Accuracyon MRT with a two features GLM model and a LOOCV procedure", round(Accuracy.GLM.MRT.red, 2))
paste("Pvalue on MRT with a two features GLM model and a LOOCV procedure", round(pvalue.GLM.MRT.red, 2))
cf.GLM.MRT.red
par(mfrow = c(1, 2))
Predicted = probs.MRT.red
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: two-features GLM MRT")
pROC_MRT <- roc(probs.MRT.red,as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 2-features MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

Accuracy.GLM.PTT.red <-cf.GLM.PTT.red$overall[1]
pvalue.GLM.PTT.red <-cf.GLM.PTT.red$overall[6]

paste("Accuracyon PTT with a five features GLM model and a LOOCV procedure", round(Accuracy.GLM.PTT.red, 2))
paste("Pvalue on PTT with a five features GLM model and a LOOCV procedure", round(pvalue.GLM.PTT.red, 2))
Predicted = probs.PTT.red
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: two-features GLM PTT")
pROC_PTT <- roc(probs.PTT.red,as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 5-features GLM PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")


#######################################
##8. QUADRATIC GLM
#######################################
#8.1 Polynomial GLM with LOOCV
X.copy = X
for (i in 2:20){
  X[, i+19] <- I(X[, i]**2)
  names(X)[i+19] <- paste("elevated", names(X)[i])}
View(X)
#Getting probabilities
set.seed(1)
Xm   <- data.matrix(X)
probs.MRT <- predict(cv.glmnet(y = MRT.m, x = Xm, family="binomial"), Xm, s = "lambda.min", type = "response")
probs.PTT <- predict(cv.glmnet(y = PTT.m, x = Xm, family="binomial"), Xm, s = "lambda.min", type = "response")
probs.MRT[probs.MRT > summary(MRT)[2]/length(MRT)] <- 1
probs.MRT[probs.MRT <= summary(MRT)[1]/length(MRT)] <- 0
probs.PTT[probs.PTT > summary(PTT)[1]/length(PTT)] <- 1
probs.PTT[probs.PTT <= summary(PTT)[2]/length(PTT)] <- 0
cf.GLM.MRT <- confusionMatrix(as.factor(probs.MRT), MRT)
cf.GLM.PTT <- confusionMatrix(as.factor(probs.PTT), PTT)
par(mfrow = c(2,2))
#Result
Accuracy.GLM.MRT <-cf.GLM.MRT$overall[1]
pvalue.GLM.MRT <-cf.GLM.MRT$overall[6]
paste("Accuracyon MRT with a full polynomial GLM model and a LOOCV procedure", round(Accuracy.GLM.MRT, 3))
paste("Pvalue on MRT with a full polynomial GLM model and a LOOCV procedure", round(pvalue.GLM.MRT, 3))
#Getting ROC and Confusion Matrix
Predicted = probs.MRT
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = paste("MRT accuracy full polynomial of degree two:", round(Accuracy.GLM.MRT, 3)))
options(warn=-1)
pROC_MRT <- roc(probs.MRT,as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-polynomial-GLM MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

#PTT
Accuracy.GLM.PTT <-cf.GLM.PTT$overall[1]
pvalue.GLM.PTT <-cf.GLM.PTT$overall[6]
paste("Accuracyon PTT with a full-polynomial-GLM model and a LOOCV procedure", round(Accuracy.GLM.PTT, 2))
paste("Pvalue on PTT with a full-polynomial-GLM model and a LOOCV procedure", round(pvalue.GLM.PTT, 2))
Predicted = probs.PTT
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = paste("PTT accuracy full polynomial of degree two:", round(Accuracy.GLM.PTT, 3)))
pROC_PTT <- roc(probs.PTT,as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: full-polynomial-GLM PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
par(mfrow = c(1, 1))
#8.2 Feature Selection
full.MRT<- glm(MRT~.,  data=X, family=binomial) 
empty.MRT<- glm(MRT~1, data=X, family=binomial)
full.PTT<- glm(PTT~.,  data=X, family=binomial) 
empty.PTT<- glm(PTT~1, data=X, family=binomial)
step.mod.MRT <- step(empty.MRT, steps=1000, trace=2, scope=list(lower=formula(empty.MRT), upper=formula(full.MRT)), direction="forward")
step.mod.PTT <- step(empty.PTT, steps=1000, trace=2, scope=list(lower=formula(empty.PTT), upper=formula(full.PTT)), direction="forward")
summary(step.mod.MRT)
summary(step.mod.PTT)

#Displaying AIC and order of features
namesX.PTT <- names(as.data.frame(X))
namesX.MRT <- names(as.data.frame(X))
final.names.PTT <- c()
final.names.MRT <- c()
#Data: It will increase of one features every search is completed
PTT.X <- 0
MRT.X <- 0
#Vector of AIC
AIC.PTT <- vector(mode = "numeric", length = 39)
AIC.MRT <- vector(mode = "numeric", length = 39)
#Vector of accuracy
ACC.PTT <- vector(mode = "numeric", length = 39)
ACC.MRT <- vector(mode = "numeric", length = 39)
for (k in 1:39){
  #Here we will keep every aic
  best_aic.ptt <- vector(mode = "numeric", length = 39-k)
  best_aic.mrt <- vector(mode = "numeric", length = 39-k)
  for(i in 1:(39-k)){
    #Temporary dataset
    if (k==1){
      temp.PTT.X <- as.data.frame(cbind(PTT.X, X[, namesX.PTT[i]])[,-1])
      temp.MRT.X <- as.data.frame(cbind(MRT.X, X[, namesX.MRT[i]])[,-1])}
    else{
      temp.PTT.X <- as.data.frame(cbind(PTT.X, X[, namesX.PTT[i]]))
      temp.MRT.X <- as.data.frame(cbind(MRT.X, X[, namesX.MRT[i]]))
    }
    #Compute the accuracy
    glm.mrt <- glm(formula = MRT~., data = as.data.frame(temp.MRT.X), family = "binomial")
    glm.ptt <- glm(formula = PTT~., data = as.data.frame(temp.PTT.X), family = "binomial")
    
    #Compute AIC
    best_aic.mrt[i] <- glm.mrt$aic
    best_aic.ptt[i] <- glm.ptt$aic
  }
  #Compute where is the best aic
  mrt.i <- which.min(best_aic.mrt)
  ptt.i <- which.min(best_aic.ptt)
  #update dataset, we take the previous and we add the column of X which name is the same of the name of the best feat
  PTT.X <- cbind(PTT.X, X[, namesX.PTT[ptt.i]])
  MRT.X <- cbind(MRT.X, X[, namesX.MRT[mrt.i]])
  final.names.PTT <- c(final.names.PTT, namesX.PTT[ptt.i])
  final.names.MRT <- c(final.names.MRT, namesX.MRT[mrt.i])
  #Take out the features name from the list name 
  namesX.PTT <- namesX.PTT[-ptt.i]
  namesX.MRT <- namesX.MRT[-mrt.i]
  #Update AIC and ACC vectors
  AIC.PTT[k] <- best_aic.ptt[ptt.i]
  AIC.MRT[k] <- best_aic.mrt[mrt.i]
}

paste("Order of importance of features for MRT:")
final.names.MRT
qplot(x=seq(1:39), y=AIC.MRT, ylab = "AIC", xlab = "Numbers of features", main = "AIC for MRT in relation of the number of features")
paste("Order of importance of features for PTT:")
final.names.PTT
qplot(x=seq(1:39), y=AIC.PTT, ylab = "AIC", xlab = "Numbers of features", main = "AIC for PTT in relation of the number of features")

#8.3 Testing the selected features
new.Data.MRT <- data.matrix(X[, c(1, 27, 5, 23, 4, 29, 38, 19, 11, 30)])
new.Data.PTT <- data.matrix(X[, c(37, 27, 8, 11, 7, 31, 18, 12, 10 ,29, 24, 23, 30,  4)])
#Getting the posterior probabilities
set.seed(1)
probs.MRT.red <- predict(cv.glmnet(y = MRT.m, x = new.Data.MRT, family="binomial"), new.Data.MRT, s = "lambda.min", type = "response")
probs.PTT.red <- predict(cv.glmnet(y = PTT.m, x = new.Data.PTT, family="binomial"), new.Data.PTT, s = "lambda.min", type = "response")
probs.MRT.red[probs.MRT.red > 0.5] <- 1
probs.MRT.red[probs.MRT.red <= 0.5] <- 0
probs.PTT.red[probs.PTT.red > 0.5] <- 1
probs.PTT.red[probs.PTT.red <= 0.5] <- 0
#Displaying Confusion Matrix
cf.GLM.MRT.red <- confusionMatrix(as.factor(probs.MRT.red), MRT)
cf.GLM.PTT.red <- confusionMatrix(as.factor(probs.PTT.red), PTT)
#Result
Accuracy.GLM.MRT.red <-cf.GLM.MRT.red$overall[1]
pvalue.GLM.MRT.red <-cf.GLM.MRT.red$overall[6]
par(mfrow = c(1, 2))
options(warn=-1)
paste("Accuracyon MRT with a 10 features GLM model and a LOOCV procedure", round(Accuracy.GLM.MRT.red, 2))
paste("Pvalue on MRT with a 10 features GLM model and a LOOCV procedure", round(pvalue.GLM.MRT.red, 2))
Predicted = probs.MRT.red
Truth = MRT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: 10-features GLM MRT")
pROC_MRT <- roc(probs.MRT.red,as.numeric(MRT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 10-features GLM MRT")
sens.ci.MRT <- ci.se(pROC_MRT)
plot(sens.ci.MRT, type="shape", col="steelblue")
plot(sens.ci.MRT, type="bars")

Accuracy.GLM.PTT.red <-cf.GLM.PTT.red$overall[1]
pvalue.GLM.PTT.red <-cf.GLM.PTT.red$overall[6]
paste("Accuracyon PTT with a 14 features GLM model and a LOOCV procedure", round(Accuracy.GLM.PTT.red, 2))
paste("Pvalue on PTT with a 14 features GLM model and a LOOCV procedure", round(pvalue.GLM.PTT.red, 2))
Predicted = probs.PTT.red
Truth = PTT
fourfoldplot(table(Predicted, Truth), main = "Confusion Matrix: 14-features quadratic GLM PTT")
pROC_PTT <- roc(probs.PTT.red,as.numeric(PTT),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC: 14-features quadratic GLM PTT")
sens.ci.PTT <- ci.se(pROC_PTT)
plot(sens.ci.PTT, type="shape", col="steelblue")
plot(sens.ci.PTT, type="bars")
cf.GLM.MRT.red
cf.GLM.PTT.red

X <- toNumeric(Xf)

###############################
#DISCUSSION
###############################

#relation of gender-mrt
X$GENDER <- as.factor(X$GENDER)
par(mfrow=c(1,2))
levels(X$GENDER) <- c("Male", "Female")
levels(MRT) <- c("Low", "High")
plot(MRT, X$GENDER, col = c("steelblue", "lightblue"), ylab = "Gender", xlab = "MRT", main = "MRT - Gender")
plot(MRT, X$X4.EMOTION.CONTROL, col = "steelblue", main = "MRT - Emotion Control", xlab = "MRT")

par(mfrow = c(1, 2))
levels(PTT) <- c("Low", "High")
plot(PTT, X$GENDER, col = c("steelblue", "lightblue"), ylab = "Gender", xlab = "PTT", main = "PTT - Gender")
plot(PTT, X$CardinalPoints, col = "steelblue", main = "PTT - Cardinal Points", xlab = "PTT")


#plot(PTT, X$X5.EXPERIENCE.OPENING, col = "steelblue", main = "PTT - Experience Opening")
#plot(PTT, X$X4.EMOTION.CONTROL, col = "steelblue", main = "PTT - Emotion Control")


