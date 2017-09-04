
# ----------------------------------------------------------------------#
#  loading , pre processing already done              #
# 1 . missing category data removed
# 2. change of class of variable 
# 3. changing to numeric data with higher levels
# main_train$propertycountylandusecode = as.numeric(main_train$propertycountylandusecode)
# main_train$propertyzoningdesc = as.numeric(main_train$propertyzoningdesc)
# main_train$regionidzip = as.numeric(main_train$regionidzip)
# main_train$regionidcity = as.numeric(main_train$regionidcity)
# 4. Imputation 
#

# All these steps have been already done before





train = main_train %>% filter(logerror <= 0.4 & logerror >= -0.39)

#---------------------------- Start modeling -----------------------#


number_trees = 200


# applying the gbm model. 

gbmModel <- gbm(logerror ~ ., 
                distribution="gaussian", 
                # var.monotone=c(0,0,0,0,0,0),  # -1: monotone decrease,
                # +1: monotone increase,
                #  0: no monotone restrictions
                interaction.depth=5,          ### 1 means additive model, 2 is two-way interaction 
                # n.minobsinnode = 10,         ### minimum number of observations in the trees terminal nodes.
                n.cores=detectCores()/2,      ### Number of cores for parallasation
                bag.fraction = 0.8,          ### Every time only x% of the sample are selected to make the tree
                n.trees = number_trees,              ### Number of trees 
                shrinkage = 0.033,           ### There is a shrinkage parameter to avoid over-fitting 
                data = train)

#---------------- Make predictions ---------------------------#



test$propertyzoningdesc = NULL
test$regionidzip = NULL
test$propertycountylandusecode = NULL
test$regionidcounty = NULL
test$regionidcity = NULL
print("Start prediction...")
prop = test
prop$month = factor("10", levels = levels(train$month))

submission <- prop %>%
  mutate("201610"=predict.gbm(object=gbmModel, newdata=prop, n.trees=number_trees, type="response"), 
         month=factor("11", levels = levels(train$month)),
         "201611"=predict.gbm(object=gbmModel, newdata=prop, n.trees=number_trees, type="response"), 
         month=factor("12", levels = levels(train$month)),
         "201612"=predict.gbm(object=gbmModel, newdata=prop, n.trees=number_trees, type="response"), 
         #month=factor("10", levels = levels(train$month)),
         "201710"=0, 
         #month=factor("11", levels = levels(train$month)),
         "201711"=0, 
         #month=factor("12", levels = levels(train$month)),
         "201712"=0) %>%
  select(parcelid, `201610`, `201611`, `201612`, `201710`, `201711`, `201712`)




options(scipen = 999) ## DO not use scientific notation 
write.csv(submission, "submission_gbm.csv", row.names = FALSE)