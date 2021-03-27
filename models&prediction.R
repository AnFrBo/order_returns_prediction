setwd("/Users/Bisa/Documents/Studium/Masterstudium/3. Semester/BADS/Challenge")

##################################################################################################################
################################################### APPLY MODELS #################################################
##################################################################################################################

library(caret)
library(xgboost)
library(parallelMap)
library(hmeasure)
library(mlr)
library(randomForest) 
library(caret)
library(parallelMap)
library(parallel)

known_clean <- read.csv("known_clean.csv", stringsAsFactors = F)

known_clean <- known_clean[,c(1:4,6:22)] 
known_clean$age <- ifelse(is.na(known_clean$age),-99,known_clean$age)

id <- known_clean$order_item_id
known_clean$order_item_id <- NULL

idx.train <- caret::createDataPartition(y = known_clean$return, p = 0.8, list = FALSE) 

train_data <- known_clean[idx.train, ]
test_data <-  known_clean[-idx.train, ] 

#### Random Forest ####
rf_train <- train_data
rf_train$return <- as.factor(rf_train$return)

task_rf <- makeClassifTask(data = rf_train, target = "return", positive = "1")
task_rf

#rf
rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob",
                  par.vals = list("replace" = TRUE, "importance" = FALSE))


#Tuning
rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 6, upper = 8), 
  makeDiscreteParam("sampsize", values = c(400,650)), 
  makeIntegerParam("ntree", lower = 800, upper = 2500)
) 
#result with: 2-6,200-300,300-1000
#$mtry:       6         6   
#$sampsize:   300       300
#$ntree:      1000      650
#AUC:         0.7072    0.7070

#result with: 4-8,300-400,650-1500
#$mtry:       8         8   
#$sampsize:   400       400
#$ntree:      1075      1500
#AUC:         0.7094    0.7095

#result with: 6-8,400-450,800-1500
#$mtry:       8         8   
#$sampsize:   450       400
#$ntree:      1050     1500
#AUC:         0.7111   0.7112

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE) # !!!!resolution!
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
parallelStartSocket(8, level = "mlr.tuneParams")

timing <- list()
timing[["parallel_param"]] <- system.time(
  
  tuning <- tuneParams(rf, task = task_rf, resampling = rdesc, #rdesc = crossvalidation (s. var above)
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
  
)

tuning$x

parallelStop()

#rf2
rf_2 <- setHyperPars(rf, par.vals = tuning$x)

#Tuning
rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 6, upper = 8), 
  makeDiscreteParam("sampsize", values = c(200,650)), 
  makeIntegerParam("ntree", lower = 800, upper = 1700) 
) 

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)

parallelStartSocket(8, level = "mlr.tuneParams")

timing <- list()
timing[["parallel_param"]] <- system.time(
  
  tuning <- tuneParams(rf, task = task_rf, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
  
)
tuning$x

parallelStop()

#rf3
rf_3 <- setHyperPars(rf, par.vals = tuning$x)


rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 7, upper = 10),
  makeDiscreteParam("sampsize", values = c(200,850)), 
  makeIntegerParam("ntree", lower = 800, upper = 1000) 
) 

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)

parallelStartSocket(8, level = "mlr.tuneParams")

timing <- list()
timing[["parallel_param"]] <- system.time(
  
  tuning <- tuneParams(rf, task = task_rf, resampling = rdesc, 
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
  
)
tuning$x

parallelStop()

#rf4
rf_4 <- setHyperPars(rf, par.vals = tuning$x)

#UNTUNED RF MODEL
modelLib[["rf"]] <- mlr::train(rf, task = task_rf)
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = test_data)
mlr::performance(yhat[["rf"]], measures = mlr::auc)

#TUNED RF2-4 MODEL
modelLib[["rf_2"]] <- mlr::train(rf_2, task = task_rf)
yhat[["rf_2"]] <- predict(modelLib[["rf_2"]], newdata = test_data)
mlr::performance(yhat[["rf_2"]], measures = mlr::auc)

modelLib[["rf_3"]] <- mlr::train(rf_3, task = task_rf)
yhat[["rf_3"]] <- predict(modelLib[["rf_3"]], newdata = test_data)
mlr::performance(yhat[["rf_3"]], measures = mlr::auc)

modelLib[["rf_4"]] <- mlr::train(rf_4, task = task_rf)
yhat[["rf_4"]] <- predict(modelLib[["rf_4"]], newdata = test_data)
mlr::performance(yhat[["rf_4"]], measures = mlr::auc)

#XGB 
modelLib[["xgb"]] <- mlr::train(xgb.learner, task = task_xgb)
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = test_data)
mlr::performance(yhat[["xgb"]], measures = mlr::auc)

#XGB 2-4
modelLib[["xgb2"]] <- mlr::train(xgb.learner2, task = task_xgb)
yhat[["xgb2"]] <- predict(modelLib[["xgb2"]], newdata = test_data)
mlr::performance(yhat[["xgb2"]], measures = mlr::auc)

modelLib[["xgb3"]] <- mlr::train(xgb.learner3, task = task_xgb)
yhat[["xgb3"]] <- predict(modelLib[["xgb3"]], newdata = test_data)
mlr::performance(yhat[["xgb3"]], measures = mlr::auc)

modelLib[["xgb4"]] <- mlr::train(xgb.learner4, task = task_xgb)
yhat[["xgb4"]] <- predict(modelLib[["xgb4"]], newdata = test_data)
mlr::performance(yhat[["xgb4"]], measures = mlr::auc)

#prediction on test data
pred <- sapply(modelLib, predict, newdata = test_data, simplify=FALSE)
#AUC
auc <- sapply(pred, mlr::performance, measures = mlr::auc)
auc

#build ensemble
pred_matrix <- sapply(pred, function(x) x$data$prob.1)
head(pred_matrix)
pred_ensemble <- rowMeans(pred_matrix[, c("rf", "rf_2", "rf_3", "rf_4", "xgb", "xgb2", "xgb3", "xgb4")])
cbind(head(pred_matrix), head(pred_ensemble))
ModelMetrics::auc(actual=test_data$return, predicted=pred_ensemble)

#write result file
unknown_clean <- read.csv("unknown_clean.csv", stringsAsFactors = FALSE)

unknown_clean <- unknown_clean[,c(1:4,6:21)]
unknown_clean$age <- ifelse(is.na(unknown_clean$age),-99,unknown_clean$age)

id <- unknown_clean$order_item_id
unknown_clean$order_item_id <- NULL

pred_unknown <- sapply(modelLib, predict, newdata = unknown_clean, simplify=FALSE)
pred_matrix <- sapply(pred_unknown, function(x) x$data$prob.1)
pred_ensemble <- rowMeans(pred_matrix[, c("rf", "rf_2", "rf_3", "rf_4", "xgb", "xgb2", "xgb3", "xgb4")])

prediction <- data.frame("order_item_id" = id, "return" = pred_ensemble)
colnames(prediction) <- c('order_item_id','return')

write.csv(prediction, file = "prediction_final.csv", row.names = FALSE)

head(prediction)