#cleaning the environment
rm(list=ls(all=T))

#function to install library if not installed else load libraries
instant_pkgs <- function(pkgs) { 
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}

#loading the required libraries 
instant_pkgs(c("data.table","dplyr","ggplot2","stringr","DT","tidyr","corrplot","leaflet","lubridate","ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
               
              "MASS","knitr", "rpart", "gbm", "ROSE"))



#setting working directory
setwd("~/Zillow")

#checking the working directory
getwd()

#lod data
#making sure that empty values are not taken as string.

properties = read.csv("properties_2016.csv",na.strings = c(" ", "", "NA","N/A","null") )
datadict = read.csv("zillow_data_dictionary.csv")

t(head(properties,3))

# -----------   PROPERTIES DATASETS -------------------------#


#  missing value analyis as major part of the data was missing 

missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")

# plotting the missing value wrt each variable 

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill = "DarkSlateBlue")+
  coord_flip()+theme_bw()

#deleting all the variable with missing value greater than 60%

# missingpct = properties[, which(colMeans(is.na(properties)) > 0.6 )]
properties = properties[,-which(colMeans(is.na(properties)) > 0.6 )]
rm(missing_values,instant_pkgs)

# save missing value of remaining variable. 
missing_values_remaining <- properties %>% summarize_each(funs(sum(is.na(.))/n()))



#-------------------------  EXPLORATORY DATA ANALYSIS -----------------------------------# 

str(properties)
t(head(properties,3))

#--------------------  UNIVARIATE ANALYSIS -----------------------------------------------#


#--------------------------------- 1. bathroomcnt ---------------------------------------#

#distribution of variable
# table(properties$bathroomcnt)
# class(properties$bathroomcnt)
# sum(is.na(properties$bathroomcnt)) / nrow(properties)

properties$bathcateg[properties$bathroomcnt >= 0 & properties$bathroomcnt <= 5] = "small"
properties$bathcateg[properties$bathroomcnt > 5 & properties$bathroomcnt <= 10] = "medium"
properties$bathcateg[properties$bathroomcnt > 10 & properties$bathroomcnt <= 15] = "large"
properties$bathcateg[properties$bathroomcnt > 15 & properties$bathroomcnt <= 20] = "XL"
table(properties$bathcateg)
properties$bathcateg = as.factor(properties$bathcateg)

#-------------------------------- 2. bedroom cnt ----------------------------------------#

# table(properties$bedroomcnt)
# class(properties$bedroomcnt)
properties$bedcateg[properties$bedroomcnt >= 0 & properties$bedroomcnt <= 5] = "small"
properties$bedcateg[properties$bedroomcnt > 5 & properties$bedroomcnt <= 10] = "medium"
properties$bedcateg[properties$bedroomcnt > 10 & properties$bedroomcnt <= 15] = "large"
properties$bedcateg[properties$bedroomcnt > 15 & properties$bedroomcnt <= 20] = "XL"

# table(properties$bedcateg)
properties$bedcateg = as.factor(properties$bedcateg)


#-----------------------------------3. buildingquality type ID -----------------------------#

#  Overall assessment of condition of the building from best (lowest) to worst (highest) #

# table(properties$buildingqualitytypeid)
# sum(is.na(properties$buildingqualitytypeid)) / nrow(properties)
properties$buildingqualitytypeid = as.factor(properties$buildingqualitytypeid)

properties$buildingqualitytypeid[ properties$buildingqualitytypeid %in% c("2","3")] = "1"
properties$buildingqualitytypeid[ properties$buildingqualitytypeid %in% c("5","6")] = "4"
properties$buildingqualitytypeid[ properties$buildingqualitytypeid %in% c("8","9")] = "7"
properties$buildingqualitytypeid[ properties$buildingqualitytypeid %in% c("11","12")] = "10"
properties$buildingqualitytypeid = as.factor(as.character(properties$buildingqualitytypeid))
# class(properties$buildingqualitytypeid)
# table(properties$buildingqualitytypeid)


#----------------------4. calculatedbathnbr ----------------------------------#

# table(properties$calculatedbathnbr)
cor.test(properties$calculatedbathnbr,properties$bathroomcnt)
sum(is.na(properties$calculatedbathnbr)) / nrow(properties)
properties = properties[,-5]
#as the correlation between this and bathroomcnt is 1 so it will be dropped

#-------------------- 5. calculated finish square feet ---------------------$
# Calculated total finished living area of the home 

# table(properties$calculatedfinishedsquarefeet)
# sum(is.na(properties$calculatedfinishedsquarefeet)) / nrow(properties)
# class(properties$calculatedfinishedsquarefeet)

#-------------------- 6. finished square feet 12 ---------------------------$
# Finished living area

# table(properties$finishedsquarefeet12)
# sum(is.na(properties$finishedsquarefeet12)) / nrow(properties)
properties$finishedsquarefeet12 = as.numeric(properties$finishedsquarefeet12)
cor.test(properties$finishedsquarefeet12,properties$calculatedfinishedsquarefeet)

# both the variables are highly correlated
properties = properties[,-6]

#---------------------7. fips -------------------------------#
# Federal Information Processing Standard code -  see https://en.wikipedia.org/wiki/FIPS_county_code for more details

# table(properties$fips)
# class(properties$fips)
properties$fips = as.factor(properties$fips)


#-------------------- 8. full bath cnt ----------------------#

# Number of full bathrooms (sink, shower + bathtub, and toilet) present in home
# table(properties$fullbathcnt)
# sum(is.na(properties$fullbathcnt)) / nrow(properties)
class(properties$fullbathcnt)
cor.test(properties$bathroomcnt,properties$fullbathcnt)
properties = properties[,-7]

#------------------- 9. heating or system type id

# Type of home heating system
# table(properties$heatingorsystemtypeid)
# class(properties$heatingorsystemtypeid)

properties$heatingorsystemtypeid[properties$heatingorsystemtypeid %in% c("19","21")]="24"
properties$heatingorsystemtypeid = as.factor(as.character(properties$heatingorsystemtypeid))

# sum(is.na(properties$heatingorsystemtypeid)) / nrow(properties) #39.48

#------------------ 10.lattitude --------------------#

# Latitude of the middle of the parcel multiplied by 10e6
# class(properties$latitude)
# length(unique(properties$latitude))

properties$latitude = properties$latitude / 1000000

#-----------------  11. longitude --------------------$


properties$longitude = properties$longitude / 1000000

#----------------- 12. lot size square feet -----------#

# Area of the lot in square feet

# length(unique(properties$lotsizesquarefeet))
# sum(is.na(properties$lotsizesquarefeet)) / nrow(properties) #.0924
# class(properties$lotsizesquarefeet)


#----------------- 13. property country land use ---------------#

# County land use code i.e. it's zoning at the county level
# length(unique(properties$propertycountylandusecode))
properties$propertycountylandusecode = as.factor(properties$propertycountylandusecode)


#------------------16. property zoning desc -------------------#

# length(unique(properties$propertyzoningdesc))
class(properties$propertyzoningdesc)
properties$propertyzoningdesc = as.factor(properties$propertyzoningdesc)

#-------------------- property land use type id----------------#

properties$propertylandusetypeid = as.factor(properties$propertylandusetypeid)
# table(properties$propertylandusetypeid)

#------------------  rawcensustract&block-------------------------$

# class(properties$rawcensustractandblock)

#-----------------  region id city ---------------------------------#

# length(unique(properties$regionidcity)) #187
properties$regionidcity = as.factor(properties$regionidcity)


#-----------------region id countrty ------------------------------#

# table(properties$regionidcounty)
properties$regionidcounty = as.factor(properties$regionidcounty)
# class(properties$regionidcounty)

#---------------------- region id zip =======================#

# table(properties$regionidzip)
# length(unique(properties$regionidzip)) #406
properties$regionidzip = as.factor(properties$regionidzip)

#--------------------- 020. rooom cnt-----------------------------#

# Total number of rooms in the principal residence
# table(properties$roomcnt)
class(properties$roomcnt)


#-------------------  21. unitcnt ----------------------------#

# Number of units the structure is built into (i.e. 2 = duplex, 3 = triplex, etc...)
# table(properties$unitcnt)
# length(unique(properties$unitcnt))
# class(properties$yearbuilt)

#--------------------- 22. year built ---------------------#

# The Year the principal residence was built 
# length(unique(properties$yearbuilt))
properties$yearbuilt = as.integer(properties$yearbuilt)
#-------------------assesement year ----------------------#

properties$assessmentyear = as.factor(properties$assessmentyear)


# ------------------- census tract and block ----------------#

cor.test(properties$rawcensustractandblock,properties$censustractandblock)

properties['censustractandblock'] = NULL

#------------------------------------------------------------------------------------------------#
#                                 Variable addition                                                  #
#------------------------------------------------------------------------------------------------#

# new features are added on the basis of domain understanding


#proportion of living area
properties['New_LivingAreaProp'] = properties['calculatedfinishedsquarefeet']/properties['lotsizesquarefeet']
#Ratio of the built structure value to land area
properties['New_ValueProp'] = properties['structuretaxvaluedollarcnt']/properties['landtaxvaluedollarcnt']
#Ratio of tax of property over parcel
properties['New_ValueRatio'] = properties['taxvaluedollarcnt']/properties['taxamount']
properties["New_location"] = properties["latitude"] + properties["longitude"]
#Amount of Reference Space
properties['New_ExtraSpace'] = properties['lotsizesquarefeet'] - properties['calculatedfinishedsquarefeet'] 
#TotalTaxScore
properties['New_TaxScore'] = properties['taxvaluedollarcnt']*properties['taxamount']
# properties['new_propertyage'] = 2017 - properties['yearbuilt']

#-----------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------#

data_type = as.data.frame(sapply(properties,class))
missing_values_remaining <- properties %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values_remaining = t(missing_values_remaining)
full = cbind(data_type,missing_values_remaining)
rm(data_type,missing_values_remaining,datadict)