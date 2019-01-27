#The following is Java Script code for 
#the logrithim scale of the Cost function for the SVM 

JScode <-
  "$(function() {
setTimeout(function(){
var vals = [1e-5];
var powStart = -4;
var powStop = 5;
for (i = powStart; i <= powStop; i++) {
var val = Math.pow(10, i);
val = parseFloat(val.toFixed(8));
vals.push(val);
}
$('#svm_cost').data('ionRangeSlider').update({'values':vals})
}, 10)})"

shinyUI(fluidPage(#theme = 'bootstrap1.css',
  # Application title
  #CSS script for the font of the title 
  includeCSS("lobster.css"),
  
  headerPanel("Classification of Proteomic Data"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      #file upload section 
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      uiOutput('dv'),
      
      # added interface for uploading data from
      tags$br(), 
      
      #select the number of folds we wish to perform
      selectInput('k_fold', label = "Number of folds - Cross Validation", 
                             choices = list('2'=2,'3'=3,'4'=4,'5'=5, 
                                            '6'=6, '7'=7, '8'=8, '9'=9, '10'=10, 
                                            n = 'nrows'), selected = '5'),
      
      #Check which classification techniques we would like to perform
      #Currently redundant. Runs for all methods.
      checkboxGroupInput('check', h3('Methods of Classification:'), 
                         choices = list( 'Lasso Regression' = 1, 'Support Vector Machine' = 2, 
                                         'AdaBoost' = 3, 'Random Forest' = 4, 
                                         'Oblique Random Forest' = 5), selected = c(1,2,3,4,5)), 
      HTML('</br>'),
      
      #Select a value for the lasso shrinkage parameter
      selectInput('t_value', label = "Lasso shrinkage parameter", 
                  choices = list("One S.E." = 'lambda.1se', "Minimum" = 'lambda.min'), selected = 'lambda.1se'),
      
      #Cost function scale for the SVM
      tags$head(tags$script(HTML(JScode))),
      
      sliderInput('svm_cost',
                  "svm_cost:",
                  min = 0.0001,
                  max = 10000,
                  value = 5), 
      
      #Select no. of trees for AdaBoost to run
      sliderInput("ada_tree", "AdaBoost Trees",
                  min = 0, max = 100, value = 10, step = 2),
      #Select no. of trees for the random forest to run
      sliderInput("rf_tree", label = "RF Trees",
                  min = 0, max = 1000, value = 600, step = 50),
      #Select no. of trees for the Oblique Random Forest
      sliderInput("obrf_tree", "OBliqueRF Trees",
                  min = 0, max = 100, value = 10, step = 2)),
    
    #Main panel 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  #Data table tab
                  tabPanel(title = "Data",
                           numericInput("obs", label = h5("Number of observations to view"), 10),
                           tableOutput("contents")),
                  
                  #Various Univariate statistics
                  tabPanel("Summary Statistics",
                           uiOutput('sumvar'),
                           verbatimTextOutput('summary'), 
                           h3(' Wilcoxon signed-rank test'),
                           verbatimTextOutput('wilcoxon'),
                           h2('BoxPlot'),
                           plotlyOutput('boxPlot')),
                  
                  #Construct accuracy tab
                  tabPanel(title = 'Accuracy',
                           helpText("Below is the classification accuracy of each individual model."
                                    ,br(),br()),
                           #Output table for accuracies
                           tableOutput('table'),
                           tabsetPanel( id = 'subtab',
                                        #All corresponding four fold plots 
                                        tabPanel( title = 'Lasso',
                                                  plotOutput('lasso_4fold')),
                                        tabPanel( title = 'SVM',
                                                  plotOutput('svm_4fold')),
                                        tabPanel( title = 'Ada', 
                                                  plotOutput('ada_4fold')),
                                        tabPanel( title = 'RF',
                                                  plotOutput('rf_4fold')),
                                        tabPanel( title = 'Oblique RF', 
                                                  plotOutput('obrf_4fold')))),
                  
                  tabPanel(title = "ROC Curve", 
                           #Output ROC Curve
                           plotlyOutput("roc")), 
                  
                  tabPanel(title = 'Variable Importance', 
                           helpText("Here we look at the variable importance scores which are available with certain methods."
                                    ,br(),br()),
                           tabsetPanel( id = 'subtab', 
                                        tabPanel('Lasso',
                                                 #Output the table of non-zero coefficients 
                                                 helpText('Variables output have non-zero coefficients.'),
                                                 helpText('0. may appear where coefficients very small'),
                                                 tableOutput('lasso_summary')),
                                        tabPanel('Random Forest',
                                                 #Variable importance plot for Random Forest
                                                 plotOutput('rf_VarImpPlot')), 
                                        tabPanel('AdaBoost', 
                                                 #Variable importance plot for Ada Boost
                                                 plotOutput('ada_VarImpPlot')), 
                                        tabPanel('ObliqueRF',
                                                 #Variable importance plot for Oblique Random Forest
                                                 plotOutput('obrf_VarImpPlot')))
                  )
      )
    )                         
  )))
