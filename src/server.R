library(shiny)
library(shinyBS)
library(shinyjs)
library(limma)
library(png)
library(vsn)
library(ggplot2)
library(RUnit)
library(tools)
library(R6)
source("helpers/arrayQCRunner.R")
source("helpers/helper.R")
source("helpers/syntaxChecker.R")
source("helpers/limmaTool_Functions.R")
source("helpers/ggplotBoxPlotForArrays.R")
source("helpers/HeatmapRunner.R")
source("helpers/normalization.R")
source("helpers/script_writer.R")
source("helpers/scaledVennDiagram.R")
source("objects/datasetValidator.R")
source("objects/rawArray.R")
source("objects/normalizedArray.R")
source("objects/inputSL.R")
source("objects/metadata.R")
source("global.R", local = FALSE)

# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

shinyServer(function(input, output) {
  downloadReady <- FALSE
  
  isDownloadReady <- function(){
    return (downloadReady)
  }

  observe({
    #Vapply can force a return to logical
    mandatoryFilledDataset <-
      vapply(fieldsMandatoryDataset,
             function(x){
               !is.null(input[[x]])
             },
             logical(1))
    mandatoryFilledDataset <- all(mandatoryFilledDataset)
    shinyjs::toggleState(id = 'fileSubmitter', condition = mandatoryFilledDataset)
  })
  
  observeEvent(input$fileSubmitter, {    
    userInput = inputSL$new(input$probeFile, input$controlProbeFile, input$targets)
    userInput$loadTargetData(input$targets)
    ####TO-DO#### EDIT SO THIS RETURNS AS VALID AND WE OFF TO A GOOD START.
    output$rawPlot <- renderPlot({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data... this may take a while...", value = 0.3)
      progress$set(message = "Almost done!", value = 1.0)
      userInput$plot()
  })
    ################################################################
    
          #### SERVER-SIDE code for EXPLORE section HERE####
    
    ###############################################################

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
    })
    ################################################################
    
    #### SERVER-SIDE code for PRE-PROCESSING section HERE####
    
    ###############################################################
    output$rawPlot2 <- renderPlot({
      boxplot(log2(x$E),range=0,ylab="log2 intensity")
    })
  })
  
  observeEvent(input$preprocessingSubmitter, {
    USER_CUSTOM <- 4
    x <- getX()
    if(input$backgroundCheckbox){
      x <-backgroundCorrect(x)
    }
    x <- normalization(x, input$normalizationSelection)
    filter_level <- as.numeric(input$filteringSelection)
    ratio  <-  round(ncol(x) * (as.numeric(input$ratioSelection)))
    if(input$filteringSelection == USER_CUSTOM){
      filter_level <- as.numeric(input$filterSlider)/100
    }
    if(input$ratioSelection == USER_CUSTOM){
      ratio  <- round(ncol(x) * (as.numeric(input$ratioSlider)/100))
    }
    expressed <- rowSums(x$other$Detection < filter_level) >= ratio
    x<- x[expressed,]
    changeY(x)
    y <- getY()
    
    output$preprocessingPlot <- renderPlot({
      boxplot(log2(y$E),range=0,ylab="log2 intensity")
    })

  })
  ################################################################
  
  #### SERVER-SIDE code for CONTRAST MATRiX section HERE####
  
  ###############################################################  
  observeEvent(input$contrastSubmitter, {
      group1Syntax <- input$group1Contrast
      group2Syntax <- input$group2Contrast
      goodSyntax <- CMSyntaxChecker(group1Syntax, group2Syntax)
      if (goodSyntax){
        computeMatrix(group1Syntax, group2Syntax, corr = NULL)
        changeGroup1(group1Syntax)
        changeGroup2(group2Syntax)
      }else{
        cat("\nTO-DO:\nSomehow the console should display this is a problem and that the user should try again.")
      }
  })
  
  output$targetsTable <- renderDataTable({
  toDisplay <- getTargets()
  if(isnt.null(toDisplay)){
    toDisplay <- as.data.frame(toDisplay)
    toDisplay
    }
    })  
    ################################################################
    
    #### SERVER-SIDE code for ANALYSIS section HERE####
    
    ###############################################################
  observeEvent(input$analysisSubmitter, {
    design <- getDesign()
    fit <- lmFit(getY(), design)
    toContrast <- getGroup()
    cont.matrix <- makeContrasts(
      contrasts = (toContrast),
      levels = design
    )
    fit2 <- contrasts.fit(fit, cont.matrix)
    EFit <- eBayes(fit2)
    changeEfit(EFit)
    downloadReady <<- TRUE
  })
    
  output$topTable <- renderDataTable({
    
    if (input$analysisSelection == 2){
      topTable(getEfit(), adjust.method = "fdr")
    }else{
      NULL
    }
  })
  
  output$vennDiagram <- renderPlot({
    
    if(input$analysisSelection == 3){
    Efit <- getEfit()
    criteria <- decideTests(Efit)
    up <- vennCounts(criteria, include = "up")[4]
    down <- vennCounts(criteria, include = "down")[4]
    ScaledVennDiagram(up, down)
    }else{
      NULL
    }
  })
  
  output$geneTable <- renderDataTable({
    if(input$analysisSelection == 4){
    NUM_GENES <- nrow(getEfit())
    topTable(getEfit(), number = NUM_GENES)
    }else{
      NULL
    }
  })
  #######TO-DO#########
  #Filter by p.value, adjusted p-value
  #Venn-Diagram 
  #Volcano Plot
  #######TO-DO#########
  observeEvent(input$codeDownloader, {
    if(isDownloadReady()){
    writeScript()
    }
    })
})