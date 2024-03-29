shinyServer(function(input, output){
  #load all packages 
  library(ROCR)
  library(caret)
  library(glmnet)
  library(randomForest)
  library(e1071)
  library(adabag)
  library(obliqueRF)
  library(plotly)
  
  #input dataset 
  datasetInput1 <- reactive({
    req(input$file1) 
    inFile <- input$file1 
    #form dataframe
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',',
                   quote = '"')
    return(df)
  })
  
  
  #choose dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Choose dependent variable'), choices = names(datasetInput1()))
  })
  
  #and return new dataset with dependent variable defined as a factor
  datasetInput<-reactive({
    df<-datasetInput1()
    df[,c(input$dv)]<-as.factor(df[,c(input$dv)])
    df
  })
  
  #Assign K-fold variable
  kfold<-reactive({
    as.integer(input$k_fold)
  })
  #Create folds 
  cvIndex <- reactive({
    set.seed(222)
    df<-datasetInput1()
    createFolds(factor(df[,c(input$dv)]), k = kfold() , returnTrain = T)
  })
  
  #Create empty vectors to store indices
  
  #check trainset output
  
  
  #loop for no. of folds
  #create test, train, and answers
  trainset<- reactive({
    df<- datasetInput()
    cv<-cvIndex()
    trainset<-NULL
    for(i in 1:kfold()){
      trainset[[i]]<-df[c(cv[[i]]), ]
    }
    return(trainset)})
  
  #test 
  testset<- reactive({
    df<- datasetInput()
    cv<-cvIndex()
    testset<-NULL
    for(i in 1:kfold()){
      testset[[i]]<-df[-c(cv[[i]]), ]
    }
    return(testset)
  })
  
  #response variable for test sets
  answers<- reactive({
    test_df<-testset()
    answers<-NULL
    for(i in 1:kfold()){
      answers[[i]]<-test_df[[i]][,c(input$dv)]
    }
    return(answers)
  })
  
  
  #Matrices for our obrf model
  #some models cannot accept formulae as arguments
  trainset_x<- reactive({
    trainset_x<-NULL
    train_df<- trainset()
    drops<- c(input$dv)
    for (i in 1:kfold()){
      trainset_x[[i]]<-as.matrix(train_df[[i]][ , !(names(datasetInput()) %in% drops)])
    }
    return(trainset_x)
  })
  
  #response variable of the training set
  trainset_y<- reactive({
    trainset_y<-NULL
    train_df<- trainset()
    for (i in 1:kfold()){
      trainset_y[[i]]<-as.factor(train_df[[i]][ ,c(input$dv)]==c(levelset()[1]))
    }
    return(trainset_y)
  })
  #test set predictor variables
  testset_x<- reactive({
    testset_x<-NULL
    drops<- c(input$dv)
    for (i in 1:kfold()){
      testset_x[[i]]<-as.matrix(testset()[[i]][ , !(names(datasetInput()) %in% drops)])
    }
    testset_x
  })
  #The class lables of the dependant variable
  levelset<-reactive({
    levels(datasetInput()[,c(input$dv)])
  })
  
  #names for dependent variable choice
  output$sumvar = renderUI({
    selectInput('sumvar', h3('Variable Summary'), choices = names(datasetInput()))
  })
  
  #summary for chosen variable
  output$summary <- renderPrint({
    summary(datasetInput()[input$sumvar])
  })
  
  #boxplot for chosen variable
  output$boxPlot<- renderPlotly({
    df_input<- datasetInput()
    p<-plot_ly(df_input,y = df_input[,c(input$sumvar)] ,color = df_input[,c(input$dv)], type = 'box')%>%
      layout(showlegend = FALSE,
             xaxis = list(title = input$dv), 
             yaxis = list(title = input$sumvar))
    p
  })
  
  
  #Wilcoxon statistic
  output$wilcoxon<- renderPrint({
    A<- subset(datasetInput(), datasetInput()[,c(input$dv)] == c(levelset()[1]), select = c(input$sumvar))[,1]
    B<- subset(datasetInput(), datasetInput()[,c(input$dv)] == c(levelset()[2]), select = c(input$sumvar))[,1]
    wilcox.test(A,B, paired = TRUE)
  })
  
  #Regression formulae
  regFormula <- reactive({
    as.formula(paste(input$dv, '~', '.'))
  })
  
  #make single reactive models for our VarImpPlots
  rf_model<-reactive({
    randomForest(regFormula(), data = datasetInput(), ntree = input$rf_tree)
  })
  
  lasso_model<-reactive({
    drops<-c(input$dv)
    dataset_x<-as.matrix(datasetInput()[ , !(names(datasetInput()) %in% drops)])
    cv.glmnet(x = dataset_x, y = datasetInput()[,c(input$dv)], family='binomial', alpha = 1)
  })
  
  ada_model<-reactive({
    boosting(regFormula(),data= datasetInput() , mfinal = input$ada_tree)
  })
  
  obrf_model<- reactive({
    drops<- c(input$dv)
    df<-datasetInput()
    dataset_x<- as.matrix(df[ , !(names(df) %in% drops)])
    dataset_y<- as.factor(as.character(df[,c(input$dv)]))
    #dataset_x
    mod<-obliqueRF(x = dataset_x, y = dataset_y, 
                   ntree = input$obrf_tree, bImportance = TRUE, training_method = 'log')
    mod
  })
  
  
  
  #Probabilities---------------------------------------
  #Begin creating models for each fold in k-folds.
  #Here we store all the predictive probabilities of our models
  lasso_pred_prob<-reactive({
    lasso_pred_prob<-NULL
    for (i in 1:kfold()){
      lasso_model<-cv.glmnet(trainset_x()[[i]], y = trainset_y()[[i]], family='binomial', alpha = 1, nfolds = 5)
      lasso_pred_prob1<-predict(lasso_model, newx= testset_x()[[i]], type='response',s=c(input$t_value))
      lasso_pred_prob[[i]]<-lasso_pred_prob1
    }
    return(lasso_pred_prob)
  })
  
  #Store predictive probabilities of our Random Forest
  rf_pred_prob<-reactive({
    rf_pred_prob<-NULL
    for (i in 1:kfold()){
      rf_model<-randomForest(regFormula(), data = trainset()[[i]], ntree = input$rf_tree)
      rf_pred_prob[[i]]<-predict(rf_model, newdata = testset()[[i]], type ='prob')[,c(levelset()[2])]
    }
    return(rf_pred_prob)
  })
  
  
  #Again for our SVM 
  svm_pred_prob<-reactive({
    svm_pred_prob<-NULL
    for (i in 1:kfold()){
      svm_model<-svm(regFormula(), data = trainset()[[i]], probability = TRUE, cost = input$svm_cost)
      svm_pred_prob1<- predict(svm_model,newdata = testset()[[i]] ,decision.values =TRUE,
                               probability = TRUE)
      svm_pred_prob[[i]]<-attr(svm_pred_prob1,"probabilities")[,c(levelset()[2])]
    }
    return(svm_pred_prob)
  })
  
  #Again for our AdaBoost model
  ada_pred_prob<-reactive({
    ada_pred_prob<-NULL
    for (i in 1:kfold()){
      ada_model<-boosting(regFormula(),data=trainset()[[i]], mfinal = input$ada_tree)
      ada_pred_prob1<- predict(ada_model ,newdata = testset()[[i]],type='prob')$prob
      ada_pred_prob[[i]]<- ada_pred_prob1[,2]
    }
    return(ada_pred_prob)
  })
  
  #Oblique random forest - logistic node split
  obrf_pred_prob<-reactive({
    obrf_pred_prob<-NULL
    for (i in 1:kfold()){
      obrf_model<-obliqueRF(trainset_x()[[i]], trainset_y()[[i]], ntree = input$obrf_tree, training_method = 'log')
      obrf_pred_prob1<-predict(obrf_model,newdata = testset_x()[[i]], type='prob')
      colnames(obrf_pred_prob1)<-c(levelset()[1], levelset()[2])
      obrf_pred_prob[[i]]<- obrf_pred_prob1[,c(levelset()[1])]
    }
    return(obrf_pred_prob)
  })
  

  
  
  #Classes----------------------------------------------
  
  #Round the probabilities of the models up to their respective lables.
  #This will denote the class prediction (instead of having to re-run models)
  lasso_pred_class<-reactive({
    lasso_pred_class<-NULL
    lasso_pred_prob1<-lasso_pred_prob()
    for (i in 1:kfold()){
      lasso_pred_class1<-ifelse(lasso_pred_prob1[[i]] > 0.5 ,levelset()[1],levelset()[2])
      lasso_pred_class[[i]]<-as.data.frame(lasso_pred_class1)[,1]
    }
    return(lasso_pred_class)
  })
  
  
  
  rf_pred_class<-reactive({
    rf_pred_class<-NULL
    rf_pred_prob1<-rf_pred_prob()
    for (i in 1:kfold()){
      rf_pred_class1<-ifelse(rf_pred_prob1[[i]] > 0.5 ,levelset()[2],levelset()[1])
      rf_pred_class[[i]]<-as.data.frame(rf_pred_class1)[,1]
    }
    return(rf_pred_class)
  })
  
  svm_pred_class<-reactive({
    svm_pred_class<-NULL
    svm_pred_prob1<-svm_pred_prob()
    for (i in 1:kfold()){
      svm_pred_class1<-ifelse(svm_pred_prob1[[i]] > 0.5 ,levelset()[2],levelset()[1])
      svm_pred_class[[i]]<-as.data.frame(svm_pred_class1)[,1]
    }
    return(svm_pred_class)
  })
  
  
  ada_pred_class<-reactive({
    ada_pred_class<-NULL
    ada_pred_prob1<-ada_pred_prob()
    for (i in 1:kfold()){
      ada_pred_class1<-ifelse(ada_pred_prob1[[i]] > 0.5 ,levelset()[2],levelset()[1])
      ada_pred_class[[i]]<-as.data.frame(ada_pred_class1)[,1]
    }
    return(ada_pred_class)
  })
  
  obrf_pred_class<-reactive({
    obrf_pred_class<-NULL
    obrf_pred_prob1<-obrf_pred_prob()
    for (i in 1:kfold()){
      obrf_pred_class1<-ifelse(obrf_pred_prob1[[i]] > 0.5 ,levelset()[2],levelset()[1])
      obrf_pred_class[[i]]<-as.data.frame(obrf_pred_class1)[,1]
    }
    return(obrf_pred_class)
  })
  
  
  #Accuracies------------------------------------
  
  #Calculate accuracy for each model.
  #Unlist the class predictions of each model and 
  #compare them to their true lables
  lasso_acc<-reactive({
    mean(unlist(answers()) == unlist(lasso_pred_class()))
  })
  
  rf_acc<-reactive({
    mean(unlist(answers()) == unlist(rf_pred_class()))
  })
  
  svm_acc<-reactive({
    mean(unlist(answers()) == unlist(svm_pred_class()))
  })
  
  ada_acc<-reactive({
    mean(unlist(answers()) == unlist(ada_pred_class()))
  })
  
  obrf_acc<-reactive({
    mean(unlist(answers()) == unlist(obrf_pred_class()))
  })
  
  
  #all in a table---- 
  #Construct a table for the accuracies to be output in the User Interface
  output$table<- renderTable({
    table1<-c(lasso_acc(), svm_acc(), ada_acc(), rf_acc(), obrf_acc())
    table2<-as.data.frame(t(table1))
    colnames(table2)<- c('Lasso', 'SVM', 'AdaBoostM1','RF', 'ObliqueRF')
    table2
  }, bordered = TRUE, striped = TRUE, spacing = 'l', width = '100%')
  
  #Confusion Matrices----------------
  #Calculate confusion matrices to be used by our four fold plot arguments
  lasso_cm<- reactive({
    confusionMatrix(data=as.factor(unlist(lasso_pred_class())), reference= as.factor(unlist(answers())))
  })
  
  rf_cm<- reactive({
    confusionMatrix(data=as.factor(unlist(rf_pred_class())), reference= as.factor(unlist(answers())))
  })
  
  svm_cm<- reactive({
    confusionMatrix(data=as.factor(unlist(svm_pred_class())), reference= as.factor(unlist(answers())))
  })
  
  ada_cm<- reactive({
    confusionMatrix(data=as.factor(unlist(ada_pred_class())), reference= as.factor(unlist(answers())))
  })
  
  obrf_cm<- reactive({
    confusionMatrix(data=as.factor(unlist(obrf_pred_class())), reference= as.factor(unlist(answers())))
  })
  
  #Performance ---------------------------------
  #Calculate the performance of each model. 
  #This is to be used in constructing the ROC curve of each model
  lasso_perf<-reactive({
    lasso_pred <- prediction((1-unlist(lasso_pred_prob())), unlist(answers()))
    performance(lasso_pred, 'tpr', 'fpr')
  })
  
  rf_perf<-reactive({
    rf_pred <- prediction(unlist(rf_pred_prob()), unlist(answers()))
    performance(rf_pred, 'tpr', 'fpr')
  })
  
  svm_perf<-reactive({
    svm_pred <- prediction(unlist(svm_pred_prob()), unlist(answers()))
    performance(svm_pred, 'tpr', 'fpr')
  })
  
  ada_perf<-reactive({
    ada_pred <- prediction(unlist(ada_pred_prob()), unlist(answers()))
    performance(ada_pred, 'tpr', 'fpr')
  })
  
  obrf_perf<-reactive({
    obrf_pred <- prediction(unlist(obrf_pred_prob()), unlist(answers()))
    performance(obrf_pred, 'tpr', 'fpr')
  })
  
  
  
  
  #AUC----------------------------------------
  #Calculate the AUC values for each of the ROC curves
  #rounded to 4 places
  lasso_perf_auc<-reactive({
    lasso_pred <- prediction(1-unlist(lasso_pred_prob()), unlist(answers()))
    lasso_perf_auc1<-performance(lasso_pred, 'auc')
    round(lasso_perf_auc1@y.values[[1]],4)
  })
  
  rf_perf_auc<-reactive({
    rf_pred <- prediction(unlist(rf_pred_prob()), unlist(answers()))
    rf_perf_auc1<-performance(rf_pred, 'auc')
    round(rf_perf_auc1@y.values[[1]],4)
  })
  
  svm_perf_auc<-reactive({
    svm_pred <- prediction(unlist(svm_pred_prob()), unlist(answers()))
    svm_perf_auc1<-performance(svm_pred, 'auc')
    round(svm_perf_auc1@y.values[[1]],4)
  })
  
  ada_perf_auc<-reactive({
    ada_pred <- prediction(unlist(ada_pred_prob()), unlist(answers()))
    ada_perf_auc1<-performance(ada_pred, 'auc')
    round(ada_perf_auc1@y.values[[1]],4)
  })
  
  obrf_perf_auc<-reactive({
    obrf_pred <- prediction(unlist(obrf_pred_prob()), unlist(answers()))
    obrf_perf_auc1<-performance(obrf_pred, 'auc')
    round(obrf_perf_auc1@y.values[[1]],4)
  })
  
  
  # create graphics 
  # data view 
  output$contents <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  #As we are using plotly, a different approach had to be made into constructing the 
  #ROC plot. 
  #Here we disect the values of the performance() of each model, 
  #and plot the TPR against the FPR
  
  lasso_data<-reactive({
    y_lasso<-lasso_perf()@y.values[[1]]
    x_lasso<-lasso_perf()@x.values[[1]]
    as.data.frame(cbind(x_lasso,y_lasso))
  })
  
  rf_data<-reactive({
    y_rf<-rf_perf()@y.values[[1]]
    x_rf<-rf_perf()@x.values[[1]]
    as.data.frame(cbind(x_rf,y_rf))
  })
  
  svm_data<-reactive({
    y_svm<-svm_perf()@y.values[[1]]
    x_svm<-svm_perf()@x.values[[1]]
    as.data.frame(cbind(x_svm,y_svm))
  })
  
  ada_data<-reactive({
    y_ada<-ada_perf()@y.values[[1]]
    x_ada<-ada_perf()@x.values[[1]]
    as.data.frame(cbind(x_ada,y_ada))
  })
  
  obrf_data<-reactive({
    y_obrf<-obrf_perf()@y.values[[1]]
    x_obrf<-obrf_perf()@x.values[[1]]
    as.data.frame(cbind(x_obrf,y_obrf))
  })
  
  #Along with plotting the TPR and FPR of each model, 
  #we include the AUC in the interactive legend of our plot.
  output$roc <- renderPlotly({
    p<-plot_ly(lasso_data(), x = lasso_data()[,1], y = lasso_data()[,2], name = sprintf("Lasso -  %s", lasso_perf_auc()), type = 'scatter', mode = 'lines', line = list(color = 'green', width = 1))%>%
      add_trace(svm_data(), x = svm_data()[,1], y = svm_data()[,2], name = sprintf("SVM -  %s", svm_perf_auc()), mode = 'lines', line = list(color = 'blue', width = 1))%>%
      add_trace(ada_data(), x = ada_data()[,1], y = ada_data()[,2], name = sprintf("AdaBoostM1 -  %s", ada_perf_auc()), mode = 'lines', line = list(color = 'orange', width = 1))%>%
      add_trace(rf_data(), x = rf_data()[,1], y = rf_data()[,2], name = sprintf("Random Forest -  %s", rf_perf_auc()),  mode = 'lines', line = list(color = 'red', width = 1))%>%
      add_trace(obrf_data(), x = obrf_data()[,1], y = obrf_data()[,2], name = sprintf("oblique RF -  %s", obrf_perf_auc()), mode = 'lines', line = list(color = 'pink', width = 1))%>%
      add_trace(x = c(0,1),y =  c(0,1), mode = "lines", showlegend = FALSE ,line = list(color = 'black', width = 2, dash = 4))%>%
      layout(yaxis = list(title = 'True Positive Rate'), 
             xaxis = list(title = 'False Positive Rate'))
    p
  })
  
  
  
  #FourFoldPlots-----------------------------------
  
  output$lasso_4fold<- renderPlot({
    fourfoldplot(lasso_cm()$table, color = c('#ec2f0d', '#1fd00f'), 
                 main = 'Lasso')
  })
  
  output$svm_4fold<- renderPlot({
    fourfoldplot(svm_cm()$table, color = c('#ec2f0d', '#1fd00f'), 
                 main = 'Support Vector Machine')
  })
  
  output$ada_4fold<- renderPlot({
    fourfoldplot(ada_cm()$table, color = c('#ec2f0d', '#1fd00f'), 
                 main = 'AdaBoostM1')
  })
  
  output$rf_4fold<- renderPlot({
    fourfoldplot(rf_cm()$table, color = c('#ec2f0d', '#1fd00f'), 
                 main = ' Random Forest')
  })
  
  output$obrf_4fold<- renderPlot({
    fourfoldplot(obrf_cm()$table, color = c('#ec2f0d', '#1fd00f'), 
                 main = 'Oblique Random Forest')
  })
  
  #Varible Importance plot for Random Forest
  output$rf_VarImpPlot<- renderPlot({
    #sort the importance values
    imp_vec1<-sort.int(rf_model()$importance,index.return = TRUE)
    #extract the names of the importance values in order
    names1<-names(rf_model()$importance[,1])[imp_vec1$ix]
    #Allow big margin for lengthly protein names
    par(mar=c(6.1, 12.2 ,4.1 ,2.1))
    barplot(tail(imp_vec1$x,20), horiz = TRUE, 
            names.arg =rev(rev(names(rf_model()$importance[,1])[imp_vec1$ix])[1:20]), las = 1, col = 'red',
            main = 'Random Forest Variable Importance', xlim = c(0, max(imp_vec1$x) + 1))
  })
  
  
  # AdaBoost Variable importance plot 
  
  output$ada_VarImpPlot<- renderPlot({
    imp_vec1<-sort.int(ada_model()$importance,index.return = TRUE)
    names1<-names(ada_model()$importance)[imp_vec1$ix]
    par(mar=c(6.1, 12.2 ,4.1 ,2.1))
    barplot(tail(imp_vec1$x,20), horiz = TRUE, 
            names.arg = rev(rev(names(ada_model()$importance)[imp_vec1$ix])[1:20]), las = 1, col = 'red',
            main = 'AdaBoost Variable Importance', xlim = c(0, max(imp_vec2$x) + 2))
  })
  
  
  
  
  #include VarImpPlot of obliqueRF
  output$obrf_VarImpPlot<- renderPlot({
    obrf_imp<-as.vector(obrf_model()$importance)
    imp_vec1<-sort.int(obrf_imp,index.return = TRUE)
    drops<- c(input$dv)
    names.arg1 <- colnames(datasetInput()[,!(names(datasetInput()) %in% drops)])[unlist(imp_vec1$ix)]
    par(mar=c(6.1, 12.2 ,4.1 ,2.1))
    barplot(rev(head(imp_vec1$x,20)), horiz = TRUE, 
            names.arg = rev(names.arg1[1:20]), las = 1, col = 'red',
            main = 'OBRF Variable Importance',xlim = c(0, min(imp_vec1$x -16)))
  })
  
  
  #include summary of coefficients for lasso/logistic regression
  output$lasso_summary <- renderTable({
    drops<- c(input$dv)
    #non-zero variables in our laaao model, given the saelected shrinkage parameter
    Variables<-c(colnames(datasetInput()[,!(names(datasetInput()) %in% drops)])[which(coef(lasso_model(), s = input$t_value) != 0)])
    #Coefficients of our variables
    Coefficients<-c(coef(lasso_cv_model, s = input$t_value)[which(coef(lasso_model(), s = input$t_value) != 0)])
    tab<-cbind(Variables, Coefficients)
    tab
    #sig<-coef(lasso_model(), s = "lambda.min")
    #names(sig[sig!=0.0])
  })
  
  
  
})



