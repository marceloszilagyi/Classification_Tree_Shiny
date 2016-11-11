

playnames = sample(as.character(supportTable$longNames),25)
realnames <- function (x) {as.character(supportTable[match(x, supportTable$longNames),1])}
a = realnames(playnames)
a = a[a!="Class"] 


subsettrain  = mytrain[,a]
subsetval = mytestd[,a]
control <- trainControl(method = "cv",number = 3 ,allowParallel = TRUE) 
# for fast selection of potential models, set cross validation for 3 folds
preProcValues <- preProcess(subsettrain, method = c("knnImpute"))
trainTransformed <- predict(preProcValues, subsettrain)


#mod01 = train(y = mytrain$Class, x=trainTransformed, trControl=control, method = "rf")
mod01 = train(y = mytrain$Class, x=trainTransformed, trControl=control, method = "rpart")

predictMyTest = predict(mod01,trainTransformed)

CMMyTest = confusionMatrix (predictMyTest,mytrain$Class)
Accuracytest = round(CMMyTest$overall[1],5)
mod01testdacc =CMMyTest$table

valTransformed <- predict(preProcValues, subsetval)
predictMyValidation = predict(mod01,valTransformed)
CMMyValidation = confusionMatrix (predictMyValidation,mytestd$Class)
Accuracyvald = round(CMMyValidation$overall[1],5)
mod01valdacc =CMMyValidation$table

plottree = prp(mod01$finalModel, type = 3, extra=104, box.palette = "BuBn",shadow.col = "gray", branch.lty = 1, nn=TRUE)


# variables in the model
#Marcelo, remember to replace the a for the realnames()
numbervarmodel = as.data.frame(table(supportTable$typevar[match(a, as.character(supportTable$name))]))
numbervarmodel$nicenames  = ifelse(numbervarmodel[,1]=="bin","Binary",ifelse(numbervarmodel[,1]=="cat","Categorical","Continuous"))


nicetext = paste0("This model has ", numbervarmodel$Freq[numbervarmodel$nicenames=="Binary"]," binary variable(s), ", numbervarmodel$Freq[numbervarmodel$nicenames=="Categorical"],
                  " categorical variable(s) and ", numbervarmodel$Freq[numbervarmodel$nicenames=="Continuous"],"  continuous variable(s)" )


# Results of the model
descriptionofresult = paste0("The model delivered an accuracy of ",format(Accuracyvald*100,digits = 3),"% for the validation set (30% of the data) and an accuracy of ", format(Accuracytest*100,digits = 3),"% for the test set (70% of the data).")
moredescription = paste0 ("For the validation set -  a total of ",sum(mod01valdacc)," cases -  the model assigned a good credit incorrectly for ", mod01valdacc[2]," case(s), and incorreclty assigned a bad credit for ", mod01valdacc[3]," case(s).")


#Additonal information
additionalinfo = mod01$finalModel

#Example of a decision tree
(getTree(mod01$finalModel,k = 3,labelVar = T))


