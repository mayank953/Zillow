
#----------------- XG booost Algorithm -----------------------------------#


target<- main_train$logerror

main_train<- main_train[, !(colnames(main_train) %in% c('logerror', 'parcelid'))]


# making numeric as Xgboost is not able to handle large categories  of data.

main_train$propertycountylandusecode <- as.numeric(as.factor(main_train$propertycountylandusecode))
main_train$propertyzoningdesc <- as.numeric(as.factor(main_train$propertyzoningdesc))

tmean<- mean(target)

# parameter initialisation 

param <- list(objective = "reg:linear", eval_metric = "mae", max_depth = 7, eta = 0.036,
              subsample = 0.75,colsample_bytree = 0.69,alpha = 3.11, base_score = tmean)

#loading the library 

library(xgboost)

cv_res = xgb.cv(param, data.matrix(main_train), label = target, nfold=5,nrounds =350,early_stopping_rounds=50)

# removing duplicate entries from test if any 

test = test[!duplicated(test[,1]),]

#training the model
xgb = xgboost(data=data.matrix(main_train), label =target,params=param, nrounds=183,verbose=1)

# -------prediction of the value -------------------------------#


pred_target<- round(predict(xgb,data.matrix(test)),5)

results <- data.table(parcelid=test$parcelid,  
                      '201610'=pred_target,  
                      '201611'=pred_target, 
                      '201612'=pred_target, 
                      '201710'=pred_target, 
                      '201711'=pred_target,
                      '201712'=pred_target
)

#submission file
write.csv(results, file = "submission_xg.csv", row.names = FALSE)
