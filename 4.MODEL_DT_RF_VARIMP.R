
# ------------------- Decision Trees & Random FOREST ------------#


# -------------------------DT -----------------------------------#
library(rpart)
library(Metrics)

fit = rpart(logerror ~ ., data = main_train, method = "anova")

predictions = predict(fit, properties)

train = main_train[!duplicated(main_train[,1]),]

properties = properties[!(properties$parcelid %in% train$parcelid),]
library(gtools)





#-------------------- random forest ---------------------#

# ----------- changing large category variable to numeric -------#
main_train$propertycountylandusecode = as.numeric(main_train$propertycountylandusecode)
main_train$propertyzoningdesc = as.numeric(main_train$propertyzoningdesc)
main_train$regionidzip = as.numeric(main_train$regionidzip)
main_train$regionidcity = as.numeric(main_train$regionidcity)


library("randomForest")


# test[is.na(test)] = 0


fit_regrex = randomForest(logerror ~ ., main_train, ntree = 150, importance = TRUE,n_jobs = -1) # n-trees = 50,100,150

# ----------------- variable importance -------------#
imp = importance(fit_regrex, type = 1)


pred_target = predict(fit_regrex, test)
imp = as.data.frame(imp)

# sum(is.na(pred_target))    # majority of missing value are there


# ----------- changing large category variable to numeric -------#
test$propertycountylandusecode = as.numeric(test$propertycountylandusecode)
test$propertyzoningdesc = as.numeric(test$propertyzoningdesc)
test$regionidzip = as.numeric(test$regionidzip)
test$regionidcity = as.numeric(test$regionidcity)


# -----------------------------------------------------------------#

# pred_target<- round(predict(xgb,data.matrix(dtest)),5)

results <- data.table(parcelid=train$parcelid,  '201610'=pred_target,  
                      '201611'=pred_target, 
                      '201612'=pred_target, 
                      '201710'=pred_target, 
                      '201711'=pred_target,
                      '201712'=pred_target
)


results[is.na(results)] = 0

write.csv(results, file = "submissionRF.csv", row.names = FALSE)
