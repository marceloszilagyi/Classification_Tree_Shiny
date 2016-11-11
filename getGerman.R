library("caret")
library('plyr')
library('data.table')
library('ggplot2')
library("magrittr")
library('DT')
library('randomForest')
library('e1071')
library(tree)
library("shinyjs")
  

data("GermanCredit")
credit = GermanCredit

# convert to factor data that has less than 10 unique values
fac = which((as.data.frame(lapply(sapply(credit,unique),length))<10)==T)
credit2 = cbind((as.data.frame(lapply(credit[,fac],as.factor))),credit[,-fac])
credit2 = credit2[c(names(credit))]
factors = which(lapply (credit2, class) == "factor")

supportTable = data.frame(name = names(credit2),class = sapply(credit2,class),count=sapply(credit2,function (x) length(unique(x))))

# proper names and levels
longNames = c("Duration of credit in months",                              
              "Credit amount in marks",                                
              "Installment rate as percentage of income",             
              "Residence Duration (years)",                   
              "Age in years",                                   
              "Number of existing credits at this bank ",               
              "Liable to provide maintenance for others",               
              "Telephone registered under the customers name",                              "Foreign worker",                        
              "Customer Credit Classificaton",                                 
              "Checking Account below 0",            
              "Checking Account between 0 and 200 Marks",       
              "Checking Account above 200 Marks",          
              "No checking account with bank ",            
              "No credits taken/ all credits paid back duly",        
              "Credits at this bank paid back duly",        
              "Existing credits paid back duly till now",                
              "Delay in paying off in the past",                   
              "Critical account/ other credits existing (not at this bank)",                "Purpose New Car",                        
              "Purpose Used Car",                       
              "Purpose Furniture/Equipment",           
              "Purpose Radio/Television",              
              "Purpose.Domestic appliances",             
              "Purpose Repairs",                      
              "Purpose Education",                     
              "Purpose Vacation",                      
              "Purpose Retraining",                    
              "Purpose Business",                      
              "Purpose Others",                         
              "Savings account/bonds smaller than 100 Marks",            
              "Savings account/bonds above or equal 100 and below 500 Marks",        
              "Savings account/bonds above or equal 500 and below 1000 Marks",       
              "Savings account/bonds above 1000 Marks",           
              "No Savings account/bonds or Unknown",           
              "Present employment since less than 1 year",               
              "Present employment above or equal to 1 and below 4 years",             
              "Present employment above or equal to 4 and below 7 years",             
              "Present employment above 7 years",               
              "Unemployed",         
              "Personal status and gender: Male, divorced or separated",      
              "Personal status and gender: Female, divorced/separated/married",             "Personal status and gender: Male, single",                  
              "Personal status and gender: Male, married or widowed",         
              "Personal status and gender: Female, single",                
              "None other debtors / guarantors",           
              "CoApplicant",    
              "Guarantor",      
              "Has Property Real Estate",                   
              "No real estate, has property insurance",                    
              "No real estate, has car or other",                     
              "Property Unknown / no proprerty",                      
              "Other installment plans in Banks",            
              "Other installment plans in Stores",          
              "No other installment plans",            
              "Housing: Rent",                          
              "Housing: Own",                           
              "Housing: Use for free",                       
              "Job: unemployed/ unskilled - non-resident",               
              "Job: unskilled - resident",                 
              "Job: skilled employee / official",                   
              "Job:  management..officer")

supportTable = cbind(supportTable,longNames)
rownames(supportTable) <- c()

# define in support table the cases which are binary categorical
twoLevels = supportTable[which(supportTable$count==2 & supportTable$name !="Class"),c("name")]

#define x and y variable in the support table
supportTable$var = "x"
supportTable$var[(which(supportTable$name == "Class"))]  = "y"

# setup levels to be more meaningful 
yesno = match(twoLevels,names(credit2))
for(i in yesno) {levels(credit2[,i])  <- c("No","Yes")}
levels(credit2$ResidenceDuration) <- c("<1","1-2","3-4",">4")

#convert 2 data to numeric
credit2$NumberExistingCredits= as.numeric(credit2$NumberExistingCredits)
credit2$InstallmentRatePercentage= as.numeric(credit2$InstallmentRatePercentage)

# remove strange or unreliable data (all data equal)
supportTable = supportTable[!(as.character(supportTable$name)=="Purpose.Vacation"),]
supportTable = supportTable[!(as.character(supportTable$name)=="Personal.Female.Single"),]

credit2$Purpose.Vacation <- NULL
credit2$Personal.Female.Single <- NULL

# create x and y variables
supportTable$var = "x"
supportTable$var[(which(supportTable$name == "Class"))]  = "y"

# identify categorical bin, categorical, continous var
supportTable$typevar = ""
supportTable$typevar[(which(supportTable$count == 2 & supportTable$var=="x"))]  = "bin"
supportTable$typevar[(which(supportTable$count  > 2 & supportTable$var=="x"))]  = "cat"
supportTable$typevar[(which(supportTable$count  > 4 & supportTable$var=="x"))]  = "con"

# create trainign and testing sets
trainingRows<-createDataPartition(credit2$Class, p=0.70, list=FALSE) # creating training set
mytrain <- credit2[trainingRows,]  
mytestd <- credit2[-trainingRows,] # the rest is 

#create the proper subsets
binaryvars = subset(supportTable, typevar == "bin" & var == "x")
categovars = subset(supportTable, typevar == "cat" & var == "x")
continvars = subset(supportTable, typevar == "con" & var == "x")

#create the percentage tables based on the **training** data for binary (binarypertable)

binarypertable = as.data.frame(t(sapply (mytrain[,which(colnames(mytrain) %in% supportTable$name[supportTable$typevar=="bin"])], function(x) table(mytrain$Class, x))))
binarypertable$varname <- rownames(binarypertable) 
rownames(binarypertable) = NULL
names(binarypertable)[1:4] = c("No_Bad","No_Good","Yes_Bad","Yes_Good" )
binarypertable$PerBadYesCredit = binarypertable$Yes_Bad/(binarypertable$Yes_Bad+binarypertable$No_Bad)
binarypertable$PerGoodYesCredit = binarypertable$Yes_Good/(binarypertable$Yes_Good+binarypertable$No_Good)
binarypertable$delta_yes = binarypertable$PerBadYesCredit-binarypertable$PerGoodYesCredit

# create the percentage tables based on the **training** data for categorical data
categoricalpertable = lapply (mytrain[,which(colnames(mytrain) %in% supportTable$name[supportTable$typevar=="cat"])], function(x) table(mytrain$Class, x))

# create datatable object for future reuse
dts = list()
for (i in (1:length(categoricalpertable))) {
  title = supportTable$longNames[match(names(categoricalpertable)[i],supportTable$name)]
  tablerawdata = t(categoricalpertable[[i]])
  per_good = tablerawdata[,1]/(tablerawdata[,1]+tablerawdata[,2])
  tablerawdata = cbind(colnames(categoricalpertable[[i]]),tablerawdata,per_good)
  dts[[i]] = datatable(tablerawdata,
                       options = list(searching=FALSE, filtering='none'),
                       caption = paste0('Table ',i,': Survey question -  ',title),           
                       rownames = FALSE,
                       colnames = c('Bad Credit' = which(colnames(tablerawdata)=="Bad"),
                                    'Good Credit' = which(colnames(tablerawdata)=="Good"),
                                    'Percentage for Good Credit'= which(colnames(tablerawdata)=="per_good"),
                                    'Range'= 1)
  ) %>% formatPercentage(4)
}



# create the summarized info for continous data 

tempdf = mytrain[,which(colnames(mytrain) %in% supportTable$name[supportTable$typevar=="con"])]
tempdf$Class = mytrain$Class

graphduration= ggplot(tempdf, aes(x=Class, y=Duration, fill=Class)) + geom_boxplot() +
  guides(fill=FALSE) +ggtitle("Duration of credit in months") +labs(x="Credit Classification",y="Duration (monhts)")

graphamount= ggplot(tempdf, aes(x=Class, y=Amount, fill=Class)) + geom_boxplot() +  guides(fill=FALSE) +ggtitle("Amount of credit in Marks") +labs(x="Credit Classification",y="Marks")

graphage= ggplot(tempdf, aes(x=Class, y=(Age), fill=Class)) + geom_boxplot() +
  guides(fill=FALSE) +ggtitle("Age of applicants") +labs(x="Credit Classification",y="Years") 

