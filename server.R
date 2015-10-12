#closeAllConnections()
#rm(list=ls())
library(shiny)
library(limma)
library(png)
library(vsn)
library(ggplot2)
source("arrayQCRunner.r")
source("helper.R")
source("global.R", local = FALSE)
source("data_shinyLimma/ggplotBoxPlotForArrays.r")
source("data_shinyLimma/HeatmapRunner.r")

#rm(list=ls(all=TRUE))
#Define server logic required to print whether dataset was uploaded
# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)



shinyServer(function(input, output) {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
  
  #Define a global variable for holding limma obj
  x <- NULL 
  probePath <- NULL
  
  getProbePath <- function(){
    return (probePath)
  }
  
  
  changeProbePath <- function(path){
    probePath <<- path
  }
  
  getX <- function(){
    return(x)
  }
  
  changeX<- function(y){
    x <<- y
  }
  
  y <- NULL
  
  getY <- function(){
    return(y)
  }
  
  changeY <- function(x){
    y <<- x
  }
  
  normalization <- function(data, style){
    if (style == 1){
      return (data)
    }
    else if (style == 2){
      newData = normalizeVSN(data)
      return (newData)
    }
    else if (style == 3){
      newData = neqc(data)
      return (newData)
    }
    else if (style == 4){
      newData = normalizeBetweenArrays(data, method = "cyclicloess", cyclic.method = "fast")
      return (newData)
    }else{
      return(-1)
    }
  }

  observeEvent(input$fileSubmitter, {
    #Capture control probe information from input
    probeFile = input$probeFile
    controlFile = input$controlProbeFile
    targetFile = input$targets
    #Save path names for manipulation
    probePath = probeFile$datapath
    changeProbePath(probePath)
    controlPath = controlFile$datapath
    targetPath = targetFile$datapath
    #Prep path names that will be used by Limma.
    probePath = substr(probePath, 1, nchar(probePath)-2)
    controlPath = substr(controlPath, 1, nchar(controlPath)-2)
    targetPath = substr(targetPath, 1, nchar(targetPath)-2)
    #Read ilmn just like mom used to do.
    target <- readTargets(file = "0", path = targetPath)
    x <- read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
    changeX(x)
    changeTargets(target)
    
    output$rawPlot <- renderPlot({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data... this may take a while...", value = 0.3)
      plot <- densityPlotter(x)
      progress$set(message = "Almost done!", value = 1.0)
      plot
    
  })
    
    output$exploratoryText <- renderText({
      
      if (input$exploreSelection == 1){
        paste("Choose the exploratory analysis technique")
      }
      
      else if (input$exploreSelection == 2){
        paste("Displaying Heatmap of Raw Data")
      }
      
      else if(input$exploreSelection == 3){
        paste("Displaying boxplot of Raw Data")
      }
      
    })
    
    output$exploratoryPlot <- renderPlot({
      
      plot <- getExploratoryPlot(input$exploreSelection, getX())
      if (isnt.null(plot)){
        plot
      }
      })
    
    
    observeEvent(input$QCGenerator, {
      QC_Reporter(probePath)
      changeDownload()
      cat(download)
    })
    
    output$rawPlot2 <- renderPlot({
      boxplot(log2(x$E),range=0,ylab="log2 intensity")
      #toPlot <- x$e
      #d <- ggplot(data= toPlot)
      #d <- d + geom_bar(stat = "identity", width = .5)
      #d
      
    })
  })
  
  observeEvent(input$preprocessingSubmitter, {
    x <- getX()
    if(input$backgroundCheckbox){
      x <-backgroundCorrect(x)
      cat("\nBackground Correct Happened\n")
    }
    x <- normalization(x, input$normalizationSelection)
    if(input$filteringSelection == 4){
    expressed <- rowSums(x$other$Detection < input$filterSlider) >= round(ncol(x)/4)
      
    }else{
    expressed <- rowSums(x$other$Detection < input$filteringSelection) >= round(ncol(x)/4)
    }
    x<- x[expressed,]
    changeY(x)
    y <- getY()
    
    output$preprocessingPlot <- renderPlot({
      boxplot(log2(y$E),range=0,ylab="log2 intensity")
      cat("\nPlot updated!\n")
    + geom_bar(stat = "identity", width = .5)
      
    })

  })
  


  
})