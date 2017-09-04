# function to determing the uncommon column between two df

my_func <- function(m,n) {
  for (i in names(m)) {
    if (!(i %in% names(n))) {
      print (i)
      print('Warning: Names are not the same')
      
    }  
    else if(i==tail(names(n),n=1)) {
      print('Names are identical')
    }
  }
}
# my_func(prop_train,main_train)



# finding the main data set with all properties using left merge

prop_train = merge(transaction,properties,how = left,by = "parcelid")

#------------------------ dividing data into numeric & categorical ----------------#

#---------------------------- numeric ------------------------------------#
numeric_index = sapply(prop_train,is.numeric) #selecting only numeric
num = prop_train[,numeric_index]
# numeric = numeric[, -c(1,9,10,12,20,17)]
var = c("parcelid","rawcensustractandblock")
num2 = num[,which( names(num) %in% var)]
num = num[,-which( names(num) %in% var)]

rm(numeric_index,var)

# ------------------------ categorical ------------------------------------#

factor_index = sapply(prop_train,is.factor)
factor_data=prop_train[,factor_index]
rm(factor_index)

#-------------------------------------------------------------------------------------------#

#-----------------------  Missing Value Imputation ----------------------------#

library(DMwR)
start.time <- Sys.time()
num = knnImputation(num,k=5)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # 43 mins

start.time <- Sys.time()
factor_data = knnImputation(factor_data,k=5)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #50 mins

# #-------------------------------------------------------------------------------------------#
# analysis upto this point can be directly loaded from k is 5.Rdata
#-------------------------------------------------------------------------------------------#


#-----------------------  Correlation analysis --------------------------------#

#--- corr plot ---#
corrgram(num, order= T, upper.panel=panel.pie, text.panel=panel.txt, prop_train="Correlation Plot")

#---- cor matrix ----#

cormat = cor(num)

# highly corelated variables can be easily deleted with the help of these two.


rm(cormat)


#------------------------ Outlier analysis -------------------------------------#

#-------------------------------------------------------------------------------------------#
# numeric_index = sapply(prop_train,is.numeric) #selecting only numeric
# numeric_data = prop_train[,numeric_index]  
# to see if outliers are there without imouting values.. similar results were obtained
# hence imputation is the right step 
# cnames = colnames(numeric_data)
#-------------------------------------------------------------------------------------------#

# ---------------- making box plots on numeric data --------------------#

cnames = colnames(num)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (cnames[i]), x = "logerror"), data = subset(num))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="logerror")+
           ggtitle(paste("Box plot of log error for",cnames[i])))
          
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn17,ncol=5)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,gn18,ncol=5)
gridExtra::grid.arrange(gn9,gn10,gn11,gn12,gn19,ncol=5)
gridExtra::grid.arrange(gn13,gn14,gn15,gn16,gn20,ncol=5)

# gridExtra::grid.arrange(gn17,gn18,gn19,gn20,ncol=4)

#removing all the plots from the environment 

rm(list = ls(pattern = "gn"),cnames) 

#-------------------------------------------------------------------------------------------#



#_------------------ joining datasets ----------------------------------------------------#



# vec = c("transactiondate","propertyzoningdesc")
#transaction date can be added but month has been extracted while year is the same so 
# no new insight will as such be obtained
# temp = prop_train[,which( names(prop_train) %in% vec)]
main_train = cbind(num2,num,factor_data)
rm(full,num2,prop_train,temp,transaction)
rm(end.time,start.time,time.taken)

#---------------               Creating Train &  test                   ------------------#
test = properties
test[,"month"] = NA
test$month = as.factor(test$month)
train = main_train[!duplicated(main_train[,1]),]
#---------------- deleting rows from test present in main_train- ---------#

test[!(test$parcelid %in% main_train$parcelid),]
test = test[!(test$parcelid %in% train$parcelid),]
train = main_train[,-3]
test = rbind(test,train)

rm(train,factor_data,num,properties,my_func)

#---------------------- filling missing value in test data --------------#
#  to fill in the missing value in the test data if needed

# missing_values <- test %>% summarize_each(funs(sum(is.na(.))/n()))

# test [ is.na(test)] = 0          #filling in all missing value with 0

# -----------------------------------------------------------------------#
#          analysis upto this point in train + test.Rdata                #
#------------------------------------------------------------------------#