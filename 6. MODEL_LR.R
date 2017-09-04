
# --------------- linear regression model --------------------------#

# first Way

#------- A combination of different features and variables can be taken. ------#


lm_model = lm(logerror ~ parcelid + bathcateg + bedcateg + latitude + longitude + yearbuilt + New_ValueRatio + New_ValueProp, data = main_train)

predictions <- data.frame(predict(lm_model, test))

submit$Predictions = predictions$predict.lm_model..test.

print('Creating submission file')
submit <- data.frame(test[,c("parcelid", "predictions")])
submit$"201610" <- round(predictions$predict.lm_model..test2.,5)
submit$"201611" <- round(predictions$predict.lm_model..test2.,5)
submit$"201612" <- round(predictions$predict.lm_model..test2.,5)
submit$"201710" <- round(predictions$predict.lm_model..test2.,5)
submit$"201711" <- round(predictions$predict.lm_model..test2.,5)
submit$"201712" <- round(predictions$predict.lm_model..test2.,5)
submit$predictions = NULL


# submit[is.na(submit)] = 0               
#replace missing value with 0 in main file

write.csv(submit, file = "submit file.csv", row.names = FALSE, na="")

#make sure that no missing value is present in the file as solution wont be accepted then




#-----------------------------------------------------------------------------------------#



# ------------------ WAY 2 -----------------------------------------------------#


# making LR learn on whole dataset 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(sqldf)



properties2 = read_csv("properties_2016.csv")
train = read_csv("train_2016_v2.csv")
adata <- merge(x = properties2, y = train, by = "parcelid", all.x = TRUE)

# properties2 = properties2[-(properties2$parcelid %in% train$parcelid),]

lr1 <- lm(logerror ~ fullbathcnt + calculatedfinishedsquarefeet + parcelid, data=adata);
predictions <- data.frame(predict(lr1, adata))

adata$p_lr1 <- predictions$predict.lr1..adata.
adata$p_lr1[is.na(adata$p_lr1)] <- mean(adata$logerror, na.rm = TRUE)  # Replace missing with average
print('Average prediction value is ...')
mean(adata$p_lr1)

print('Creating submission file')
submit <- data.frame(adata[,c("parcelid", "p_lr1")])
submit$"201610" <- round(submit$p_lr1,4)
submit$"201611" <- round(submit$p_lr1,4)
submit$"201612" <- round(submit$p_lr1,4)
submit$"201710" <- 0
submit$"201711" <- 0
submit$"201712" <- 0
submit$p_lr1<- NULL
 # remove the original prediction from the submit file

# submit[is.na(submit)] = 0               
#replace missing value with 0 in main file


write.csv(submit, file = "submit_1.csv", row.names = FALSE, na="")
