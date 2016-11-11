rfmodel <- reactive ({
  subsettrain  = mytrain[,realnames()]
  control <- trainControl(method = "cv",number = 1 ,allowParallel = TRUE) # for fast selection of           potential models, set cross validation for 1 folds
  
  preProcValues <- preProcess(subsettrain, method = c("center", "scale", "knnImpute"))
  trainTransformed <- predict(preProcValues, subsettrain)
  # testTransformed <- predict(preProcValues, test)
  mod01 = train(y = mytrain$Class, x=trainTransformed, trControl=control, method = "rf")
  predictMyTest = predict(mod01,trainTransformed)
  CMMyTest = confusionMatrix (predictMyTest,mytrain$Class)
  Accuracytest = round(CMMyTest$overall[1],5)
  mod01testdacc =CMMyTest$table
  
  subsetval = mytestd[,realnames(playnames)]
  valTransformed <- predict(preProcValues, subsetval)
  predictMyValidation = predict(mod01,valTransformed)
  CMMyValidation = confusionMatrix (predictMyValidation,mytestd$Class)
  Accuracyvald = round(CMMyValidation$overall[1],5)
  mod01valdacc =CMMyValidation$table
  
})