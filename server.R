library(shiny)
library(limma)
library(png)
library(vsn)
library(ggplot2)
#Define server logic required to print whether dataset was uploaded
# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

x <- NULL 

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
    #Shouldn't ever reach this case. 
    return(-1)
  }
}

shinyServer(function(input, output) {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
  
  #Define a global variable for holding limma obj
 
  

  observeEvent(input$fileSubmitter, {
    
    
    #Capture control probe information from input
    probeFile = input$probeFile
    controlFile = input$controlProbeFile
    targetFile = input$targets
    #Save path names for manipulation
    probePath = probeFile$datapath
    controlPath = controlFile$datapath
    targetPath = targetFile$datapath
    #Prep path names that will be used by Limma.
    probePath = substr(probePath, 1, nchar(probePath)-2)
    controlPath = substr(controlPath, 1, nchar(controlPath)-2)
    targetPath = substr(targetPath, 1, nchar(targetPath)-2)
    #Read ilmn just like mom used to do.
    readTargets(file = "0", path = targetPath)
    x <- read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
    changeX(x)
    
    
    output$rawPlot <- renderPlot({
      boxplot(log2(x$E),range=0,ylab="log2 intensity")
      #toPlot <- x$e
      #d <- ggplot(data= toPlot)
      #d <- d + geom_bar(stat = "identity", width = .5)
      #d
    
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
      #toPlot <- x$e
      #d <- ggplot(data= toPlot)
      #d <- d + geom_bar(stat = "identity", width = .5)
      #d
      
    })

  })
  
  

  

})