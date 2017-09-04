# ----------------  Transaction dataset --------------------------------#
#setting working directory
setwd("~/Zillow")

#checking the working directory
getwd()

require("zoo")

#load the data
transaction = read.csv("train_2016_v2.csv",header = T,na.strings = c(" ", "", "NA","N/A","null"))

#---------------------------------------------------------------------#

t(head(transaction,3))
summary(transaction)

# --------------- finding missing values -------------------------------# 

missing_values <- transaction %>% summarize_each(funs(sum(is.na(.))/n()))
#so our data contains no missing value.
rm(missing_values)


#------------------------------------ UVA ----------------------------#

#--------------- parcel id ---------------------------------------------#
length(unique(transaction$parcelid))
class(transaction$parcelid)

#-------------------  transaction date -------------------------#

transaction$transactiondate = as.Date(transaction$transactiondate)

# Extracting month from transaction dates

transaction$month =format(transaction$transactiondate,"%m")
transaction$month = as.factor(transaction$month)

# transactions vs month 

transaction %>%
  ggplot(aes(x=month)) +
  geom_bar(fill="Red")+
  ylab("Count")


transaction %>%
  ggplot(aes(x=transactiondate)) +
  geom_bar(fill="Blue")+
  ylab("Count")

#---------------------------- log error --------------------------------#


# class(transaction$logerror) #num
#visualizing the target variable i.e. logerror

require(scales)
transaction %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

boxplot(transaction$logerror~transaction$month,main="logerror distribution per months",
        xlab="Month in which transaction was made", ylab="Log-error",col="red")


# for removing the outlier &selecting the subset of the data
# errorrem = transaction[quantile(transaction$logerror,0.01) < transaction$logerror & transaction$logerror < quantile(transaction$logerror,0.99),]

# transaction = write.csv("transaction.csv")