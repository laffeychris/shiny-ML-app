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
  includeCSS("lobster.css"),
  
  headerPanel("Classification of Proteomic Data"),
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      uiOutput('dv'),
      
      # added interface for uploading data from
      tags$br(),
      
      checkboxGroupInput('check', h3('Methods of Classification:'), 
                         choices = list( 'Lasso Regression' = 1, 'Support Vector Machine' = 2, 
                                         'AdaBoost' = 3, 'Random Forest' = 4, 
                                         'Oblique Random Forest' = 5), selected = c(1,2,3,4,5)), 
      HTML('</br>'),
      
      selectInput('t_value', label = "Lasso shrinkage parameter", 
                  choices = list("One S.E." = 'lambda.1se', "Minimum" = 'lambda.min'), selected = 'lambda.1se'),
      
      tags$head(tags$script(HTML(JScode))),
      
      sliderInput('svm_cost',
                  "svm_cost:",
                  min = 0.0001,
                  max = 10000,
                  value = 5), 
      
      sliderInput("ada_tree", "AdaBoost Trees",
                  min = 0, max = 60, value = 10, step = 2), 
      sliderInput("rf_tree", label = "RF Trees",
                  min = 0, max = 1000, value = 600, step = 50),
      sliderInput("obrf_tree", "OBliqueRF Trees",
                  min = 0, max = 60, value = 10, step = 2)),
    
    #Main panel 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel(title = "Data",
                           numericInput("obs", label = h5("Number of observations to view"), 10),
                           tableOutput("contents")),
                  
                  tabPanel("Summary Statistics",
                           uiOutput('sumvar'),
                           verbatimTextOutput('summary'), 
                           h2('BoxPlot'),
                           plotlyOutput('boxPlot')),
                  
                  tabPanel(title = 'Accuracy',
                           helpText("Below is the classification accuracy of each individual model."
                                    ,br(),br()),
                           tableOutput('table'),
                           tabsetPanel( id = 'subtab',
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
                           plotlyOutput("roc")), 
                  
                  tabPanel(title = 'Variable Importance', 
                           helpText("Here we look at the variable importance scores which are available with certain methods."
                                    ,br(),br()),
                           tabsetPanel( id = 'subtab', 
                                        tabPanel('Lasso',
                                                 #helpText("Variables denoted with '.' are deemed insignificant at a 0.05 level of confidence"),
                                                 #They arent?
                                                 #Maybe we should just list all non-zero variables???
                                                 verbatimTextOutput('lasso_summary')),
                                        tabPanel('Random Forest',
                                                 plotOutput('rf_VarImpPlot')), 
                                        tabPanel('AdaBoost', 
                                                 plotOutput('ada_VarImpPlot')), 
                                        tabPanel('ObliqueRF',
                                                 plotOutput('obrf_VarImpPlot')))
                  )
      )
    )                         
  )))
