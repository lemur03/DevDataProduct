
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)

dataset <- diamonds

shinyUI(pageWithSidebar(
  
  headerPanel("Coronary Heart Disease analysis"),
    
  sidebarPanel(
    textInput(inputId="seed", label = "Seed",value = 125),
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=dim(SAheart)[1],
                    value=dim(SAheart)[1]/2, step=5, round=0),
    
    
    selectInput('y', 'outcome', names(SAheart),"chd"),
    tags$span(style="color:red", "Do not change the outcome!"),    
    
    # Dynamic list of predictors
    uiOutput("predictorsName")
    
  ),
  
  mainPanel(
    
    tags$h3("Training Set"),
    tags$p("Miss Classification for Training set:"),
    verbatimTextOutput('missClassTrain')
    ,  plotOutput('plotTrain'), 
  
    tags$h3("Test Set"),
    tags$p("Miss Classification for Test set:"),
    verbatimTextOutput('missClassTest'),
    plotOutput('plotTest')
    
   
  )
))
