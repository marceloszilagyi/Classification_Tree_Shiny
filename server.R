# Load libraries 
library(shiny)
library(DT)
library(magrittr)
library(shinyjs)
library(ggplot2)
library(plotROC)
#
  
# this is just a handler to make the table look nice - add column names, and double column on the top
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Survey Question'),
      th(colspan = 2, 'Bad Credit'),
      th(colspan = 2, 'Good Credit'),
      th(colspan = 3, 'Percentage of Yes responses')
    ),
    tr(
      th('No'),
      th('Yes'),
      th('No'),
      th('Yes'),
      th('Bad Credit [1]'),
      th('Good Credit [2]'),
      th('Delta [2]-[1]')
    )
  )
))

#######################
# the shiny server code starts here
########################

shinyServer(function(input, output) {
  
  ### some functions to determine things that be reused
  # thatlongname - gets the table selection in the binary table and gets the variable name
  thatlongname <- reactive({
    match(binarypertable[input$mytable1_rows_selected,5],supportTable$name)
  })
  
  # aggregate all variables that are selected by the user
  allvariables <- reactive({c(supportTable[thatlongname(),4],
                              input$in22,
                              input$in32)
  })
  
  #graphsforcategorical - prepare the tables for printing 
  graphsforcategorical <- reactive({
    
    # this get the right names and pass to the contigency table
    i = match(supportTable$name[match(input$in21,supportTable$longNames)],names(categoricalpertable))
    title = supportTable$longNames[match(names(categoricalpertable)[i],supportTable$name)]
    tablerawdata = t(categoricalpertable[[i]])
    per_good = tablerawdata[,1]/(tablerawdata[,1]+tablerawdata[,2])
    tablerawdata = cbind(colnames(categoricalpertable[[i]]),tablerawdata,per_good)
    
    # this generates the table
    datatable(tablerawdata,
              options = list(searching=FALSE, filtering='none'),
              caption = paste0('Survey question -  ',title),           
              rownames = FALSE,
              colnames = c('Bad Credit' = which(colnames(tablerawdata)=="Bad"),
                           'Good Credit' = which(colnames(tablerawdata)=="Good"),
                           'Percentage for Good Credit'= which(colnames(tablerawdata)=="per_good"),
                           'Range'= 1)
    ) %>% formatPercentage(4)
  })
  
  # reactive function to translate real variable names to long names
  allvarsmodel <- reactive ({ c(as.character(supportTable[thatlongname(),4]),input$in22,input$in32)})
  realnames <- reactive ({ as.character(supportTable[match(allvarsmodel(), supportTable$longNames),1])})
  
  # some initial outups - the name of the variables, the long name of the variables and the total number of variables
  output$model1 <- renderText(allvarsmodel())
  output$model2 <- renderText(realnames())
  output$model6 <- renderText(length(allvariables())>3)
  
  
  # button to be enable after 3 variables are selected 
  observe({
    toggleState("submit_loc", condition = length(allvariables())>2)
  })
  
  # lock the button while calculation is running

  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      disable("submit_loc")
      
      #######################
      # model starts here
      #######################
      
      # subset the database for the names selected
      subsettrain  = mytrain[,realnames()]
      subsetval = mytestd[,realnames()]
      
      # for best user experience, set cross validation for 5 folds only
      control <- trainControl(method = "cv",number = 3 ,allowParallel = TRUE) 
      
      # preprocess the data and apply transformations for the test data 
      preProcValues <- preProcess(subsettrain, method = c("knnImpute"))
      trainTransformed <- predict(preProcValues, subsettrain)
      
      # run the model using CARET
      mod01 = train(y = mytrain$Class, x=trainTransformed, trControl=control, method = "rpart")
      predictMyTest = predict(mod01,trainTransformed)
      
      # determine stats - confusion matrix, etc
      CMMyTest = confusionMatrix (predictMyTest,mytrain$Class)
      Accuracytest = round(CMMyTest$overall[1],5)
      mod01testdacc =CMMyTest$table
      
      # transform validation set
      valTransformed <- predict(preProcValues, subsetval)
      
      # determine stats - confusion matrix, etc
      predictMyValidation = predict(mod01,valTransformed)
      CMMyValidation = confusionMatrix (predictMyValidation,mytestd$Class)
      Accuracyvald = round(CMMyValidation$overall[1],5)
      mod01valdacc =CMMyValidation$table
      
      #######################
      # data prep for output
      ######################
      
      # number of variables in the model
      numbervarmodel = as.data.frame(
        table(supportTable$typevar[match(realnames(), as.character(supportTable$name))]))
      # apply long name for the variable type
      numbervarmodel$nicenames  = ifelse(numbervarmodel[,1]=="bin","Binary",
                                         ifelse(numbervarmodel[,1]=="cat","Categorical","Continuous"))
      
      # Create the number of variables text 
      nicetext = paste0("This model has ", numbervarmodel$Freq[numbervarmodel$nicenames=="Binary"],
                        " binary variable(s), ", numbervarmodel$Freq[numbervarmodel$nicenames=="Categorical"],
                        " categorical variable(s) and ", numbervarmodel$Freq[numbervarmodel$nicenames=="Continuous"],
                        "  continuous variable(s)." )
      
      
      # Results of the model - accuracy
      descriptionofresult = paste0("The model delivered an accuracy of "
                                   ,format(Accuracyvald*100,digits = 3),
                                   "% for the validation set (30% of the data) and an accuracy of ", 
                                   format(Accuracytest*100,digits = 3),"% for the test set (70% of the data).")
      
      # Results of the model - some numbers
      moredescription = paste0 ("For the validation set -  a total of ",sum(mod01valdacc),
                                " cases -  the model assigned a good credit incorrectly for ", 
                                mod01valdacc[2]," case(s), and incorreclty assigned a bad credit for ", 
                                mod01valdacc[3]," case(s).")
      
      
      #######################
      # Outputs for the UI.R
      ######################
    
      # all the outputs from the model 
      output$model3 <-  renderText(realnames())
      output$model4 <-  renderTable(subsettrain[(1:10),])
      output$model5 <-  renderTable(mytrain$Class)
      output$model8 <-  renderText(nicetext)
      output$model9 <-  renderText(descriptionofresult)
      output$model10 <- renderText(moredescription)
      output$model11 <- renderPrint(mod01)
      output$model13 <- renderPlot(prp(mod01$finalModel, type = 3, extra=104, box.palette = "BuBn",shadow.col = "gray", branch.lty = 1, nn=TRUE)) # tree 
      output$model15 <- renderPrint(CMMyValidation)
      
      
      # after all calculations, enable submit again
      enable("submit_loc")
      
      
      
    }
  )
  
  #######################
  # Other Outputs for the UI.R
  ######################
  
  
  #graphsforcont - prepare the graphs for printing for cont. variables
  # this reactive functions runs a graph based on the selection box
  graphsforgraphs <- reactive({
    prepy = match(supportTable$name[match(input$in31,supportTable$longNames)],colnames(tempdf))
    tempdf2 = tempdf[,c(prepy,4)]
    ggplot(tempdf2, aes_string(y=colnames(tempdf2)[1], x=colnames(tempdf2)[2], fill="Class")) + geom_boxplot() +
    guides(fill=FALSE) +ggtitle(input$in31) +labs(x="Credit Classification",y="") 
  })
  
  output$plot1 <- renderPlot(graphsforgraphs())

  # this is the side panel   
  output$out7 <- renderPrint(c(
    as.character(supportTable[thatlongname(),4]),
    input$in22,
    input$in32))
  
  # this is the contgency table
  output$mytable5 <-  DT::renderDataTable(graphsforcategorical())
  
  # Table for binary variables
  output$mytable1 <- DT::renderDataTable(
    datatable(
      cbind(supportTable[match(binarypertable$varname, 
                               supportTable$name),"longNames"]
            ,binarypertable[,c(1,3,2,4,6:8)]), 
      container = sketch, 
      rownames = FALSE,
      caption = "Binary variables - Summary"
    ) %>% formatPercentage(6:8)
  )
  
  
  
})








