library(tidyverse)
library(mlbench)
library(caret)
library(ggplot2)
require(caTools)
library(rpart)
library(kernlab)
library(klaR)


data<- read.csv("survey_lung_cancer.csv", sep = ",", header = TRUE)
#Change the levels of the target variable to "1" and "0" which stand for YES and NO respectively
data$LUNG_CANCER=ifelse(data$LUNG_CANCER=="YES",1,0)
data$LUNG_CANCER=as.factor(data$LUNG_CANCER)
table(data$LUNG_CANCER)
intrain <- createDataPartition(y = data$LUNG_CANCER, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

#To train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


seed = 101
metric <- "Accuracy"
preProcess=c("center", "scale")

#SVM
set.seed(seed)
fit.svm <- train(LUNG_CANCER ~., data = training, method = "svmLinear",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# kNN
set.seed(seed)
fit.knn <- train(LUNG_CANCER~., data=training, method="knn", metric=metric, preProc=c("center", "scale"), trControl=trctrl)

save(fit.svm, fit.knn,file = "models.RData")

results <- resamples(list(svm=fit.svm, knn=fit.knn))

#bagging=fit.treebag

save(results, file = "models_accuracy.RData")
load("models_accuracy.RData")
load("models.RData")
test_results <- data.frame(testing$LUNG_CANCER)

test_results$predicted_svm <- predict(fit.svm, testing)
test_results$predicted_knn <- predict(fit.knn, testing)


confusionMatrix(test_results$testing.LUNG_CANCER, test_results$predicted_svm)
confusionMatrix(test_results$testing.LUNG_CANCER, test_results$predicted_knn)



# Table comparison
summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

