
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)
library(caret)
library(ElemStatLearn)
data(SAheart)

shinyServer(function(input, output) {
  library(caret)
  data <- reactive({SAheart})
  #train <- reactive({ })
  trainSA <- reactive({data()[sample(1:dim(data())[1],size=input$sampleSize,replace=F),]})
  testSA <- reactive({data()[-sample(1:dim(data())[1],size=input$sampleSize,replace=F),]})
  
    
  output$predictorsName <- renderUI({
    if (is.null(input$y))
      return()
    # Depending on input$x, we'll update the possible precitors.
    otherNames <- names(data())[!(names(data()) %in% c(input$y))]
    selectInput("x", "Predictors",
                choices =  otherNames,
                selected = otherNames[length(otherNames)-1],
                multiple = TRUE)  
  })
    
  glmFit <- reactive({
    if (is.null(input$y) || is.null(input$x))
      return()
   set.seed(input$seed)    
   formula <- as.formula(paste(input$y,paste(input$x,collapse=" + "),sep=" ~ "))
   train(formula,method="glm",family="binomial",data=trainSA())  
  })
  
  output$missClassTest = reactive({    
    if (is.null(input$y) || is.null(input$x))
      return()
    missClassification <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
    missClassification(trainSA()$chd,predict(glmFit(),testSA()))  
  })
  
  output$missClassTrain = reactive({  
    if (is.null(input$y) || is.null(input$x))
      return()
    missClassification <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
    missClassification(trainSA()$chd,predict(glmFit(),trainSA()))  
  })
  
  
  
  output$plotTrain <- renderPlot({
    if (is.null(input$y) || is.null(input$x))
      return()
    prediction <- (predict(glmFit(),trainSA()) >0.5)*1 == trainSA()$chd
    tmpData <- cbind(trainSA(),prediction)
    names(tmpData)<- c(names(trainSA()),'predicted')
    
    p<- ggplot(tmpData, aes_string(x=paste(input$x,collapse=" + "), y=input$y)) + 
        geom_point(aes(colour=predicted),shape=1, position=position_jitter(width=.05,height=.05)) + 
       
        stat_smooth(method="glm", family="binomial", se=T)
    print(p)
    }, height=300)  
  output$plotTest <- renderPlot({
    if (is.null(input$y) || is.null(input$x))
      return()
    
    prediction <- (predict(glmFit(),testSA()) >0.5)*1 == testSA()$chd
    tmpData <- cbind(testSA(),prediction)
    names(tmpData)<- c(names(testSA()),'predicted')

    p<- ggplot(tmpData, aes_string(x=paste(input$x,collapse=" + "), y=input$y), title=) + 
       geom_point(aes(colour=predicted),shape=1, position=position_jitter(width=.05,height=.05)) + 
       stat_smooth(method="glm", family="binomial", se=T)
    print(p)
  }, height=300)  

})
