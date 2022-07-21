#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(naivebayes)

# Define UI for application that draws a histogram
library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(plotly)


dashboardPage(
  dashboardHeader(title = h3("Fake News Analyser"), dropdownMenuOutput("msgOutput")),
  dashboardSidebar(
    sliderInput(
      "Slider1",
      label = h3("Train/Test Split %"),
      min = 0,
      max = 100,
      value = 70
    ),
    
    textOutput("cntTrain"),
    textOutput("cntTest"),
    br(),
    sliderInput(
      "Slider2",
      label = h3("Ntree In Random Forest"),
      min = 0,
      max = 100,
      value = 53
    ),
    
    br(),
    
    sliderInput(
      "Slider3",
      label = h3("Mtry In Random Forest"),
      min = 0,
      max = 10,
      value = 3
    ),
    
    
    br(),
    
    sliderInput(
      "Slider4",
      label = h3("Cluster Size"),
      min = 0,
      max = 10,
      value = 4
    )
    
    #
    # menuItem(
    #   "Generate Report",
    #   tabName = "sectors",
    #   icon = icon("download"),
    #   radioButtons(
    #     'format',
    #     'Document format',
    #     c('HTML', 'Word'),
    #     inline = FALSE,
    #     selected = 1
    #   ),
    #   downloadButton("report", "Download Report", class = "butt"),
    #   tags$head(tags$style(".butt{color: blue !important;}"))
    # )
    
  ),
  dashboardBody(
    fluidPage(
      box(
        selectInput(
          "SelectX",
          label = "Select variables:",
          choices = c("Source","Title","Description","Links","DatePosted","Label"),
          multiple = TRUE,
          selected = c("Source","Title","Description","Links","DatePosted","Label")
        ),
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "X variable"
      ),
      box(
        selectInput("SelectY", label = "Select variable to predict:", choices = c("Label")),
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "Y variable",
        selected = c("Label")
      )
      
      
      
    ),
    
    fluidPage(  
      
      tabBox(
        id = "tabset1",
        height = "2000px",
        width = 12,
        
        tabPanel("Data",
                 box(withSpinner(DTOutput(
                   "Data"
                 )), width = 12)),
        tabPanel(
          "Data Summary",
          box(withSpinner(verbatimTextOutput("Summ")), width = 6),
          box(withSpinner(verbatimTextOutput("Summ_old")), width = 6),
          box(
            withSpinner(verbatimTextOutput("structure")), width = "6"
          )
        ),
 
        tabPanel("Plots",
                 box(withSpinner(plotOutput(
                   "Corr"
                 )), width = 6),
        box(withSpinner(plotOutput("CorrMatrix")), width = 6),
        box(withSpinner(plotOutput("CorrMatrix1")), width = 6)
        
        ),
 
 
        tabPanel(
          "Random Forest Model",
          box(
            withSpinner(verbatimTextOutput("Model")),
            width = 6,
            title = "Model Summary"
          ),
          
          box(
            withSpinner(verbatimTextOutput("ImpVar")),
            width = 6,
            title = "Variable Importance"
          )
        ),
        #textOutput("correlation_accuracy"),
        tabPanel(
          "Random Forest Prediction",
          box(withSpinner(verbatimTextOutput("Prediction")), width = 6, title = "RF - Test - Prediction"),
          box(withSpinner(verbatimTextOutput("Prediction_train")), width = 6, title = "RF - Train - Prediction"),
          box(withSpinner(plotOutput("residualPlots")),width=3, title = "RF - Actual Test data"),
          box(withSpinner(plotOutput("residualPlots1")), width=3, title = "RF - Predicted Test data"),
          box(withSpinner(plotOutput("residualPlots_train")), width=3, title = "RF - Actual Train data"),
          box(withSpinner(plotOutput("residualPlots1_train")), width=3, title = "RF - Predicted Train data")
        ),
 
 tabPanel(
   "Naive Bayes Model",
   box(
     withSpinner(verbatimTextOutput("Model2")),
     width = 6,
     title = "NB - Model Apriori[Total of rows w.r.t Levels]"
   ),
   box(
     withSpinner(verbatimTextOutput("Model_sum")),
     width = 6,
     title = "Model Summary"
   ),
   
   box(
     withSpinner(verbatimTextOutput("ImpVar2")),
     width = 6,
     title = "NB - Variable Importance"
   )
 ),
 tabPanel(
   "Naive Bayes Prediction",
   box(withSpinner(verbatimTextOutput("Prediction2")), width = 6, title = "NB - Test Prediction"),
   box(withSpinner(verbatimTextOutput("Prediction3")), width = 6, title = "NB - Train Prediction"),
   box(withSpinner(plotOutput("residualPlots2")), width=3, title = "NB - Actual Test data"),
   box(withSpinner(plotOutput("residualPlots3")),width=3, title = "NB - Predicted Test data"),
   box(withSpinner(plotOutput("residualPlots4")), width=3, title = "NB - Actual Train data"),
   box(withSpinner(plotOutput("residualPlots5")), width=3, title = "NB - Predicted Train data")
 ),
 tabPanel(
   "K-Means",
   box(withSpinner(plotOutput("residualPlots_kmeans")), width = 6, title = "K-Means - Clusters"),
   box(withSpinner(verbatimTextOutput("residualPlots_kmeans_summ")), width = 6, title = "K - Means Summary")
 ),
 
 tabPanel(
   "Sentimental Analysis",
   box(withSpinner(verbatimTextOutput("sentiment")), width = 12, title = "Sentimental Analysis w.r.t Label"),
   box(withSpinner(verbatimTextOutput("sentiment_title")), width = 12, title = "Sentimental Analysis w.r.t Description")
 )
 
 
 
      )
    )
  )
)


