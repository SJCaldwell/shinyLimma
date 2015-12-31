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
source("objects/exp_design.R")
source("objects/differentialExpression.R")
source("global.R", local = FALSE)

# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

shinyServer(function(input, output) {
  downloadReady <- FALSE
  
  isDownloadReady <- function(){
    return (downloadReady)
  }
  #Globals that will represent the user input.
  userInput = NULL
  userProcessed = NULL
  userDesign = NULL
  completedAnalysis = NULL

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
    userInput <<- inputSL$new(input$probeFile, input$controlProbeFile, input$targets)
    output$rawPlot <- renderPlot({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading data... this may take a while...", value = 0.3)
      progress$set(message = "Almost done!", value = 1.0)
      userInput$densityPlot()
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
      
      plot <- getExploratoryPlot(input$exploreSelection, userInput$dataManager$rawData)
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
      boxplot(log2(userInput$dataManager$rawData$E),range=0,ylab="log2 intensity")
    })
  })
  
  observeEvent(input$preprocessingSubmitter, {
      USER_CUSTOM = 4
      bgCorrect = input$backgroundCheckbox
      filter_level = as.numeric(input$filteringSelection)
      ratio  =  round(ncol(userInput$dataManager$rawData) * (as.numeric(input$ratioSelection)))
      if(input$filteringSelection == USER_CUSTOM){
          filter_level = as.numeric(input$filterSlider)/100
    }
      if(input$ratioSelection == USER_CUSTOM){
          ratio  = round(ncol(userInput$dataManager$rawData) * (as.numeric(input$ratioSlider)/100))
    }
    userProcessed = normalizedArray$new(userInput$dataManager$rawData, input$normalizationSelection, filter_level, ratio, bgCorrect)
    userProcessed <<- userProcessed
    
    output$preprocessingPlot <- renderPlot({
      userProcessed$boxplot()
    })
    
    output$filteringResults <- renderPlot({
      userProcessed$probeFilterPlot()
    })

  })
  
  observeEvent(input$refilterButton, {
    if(isnt.null(userProcessed)){
        output$filteringResults <- renderPlot({
          USER_CUSTOM = 4
          filter_level = as.numeric(input$filteringSelection)
          ratio  =  round(ncol(userInput$dataManager$rawData) * (as.numeric(input$ratioSelection)))
          if(input$filteringSelection == USER_CUSTOM){
            filter_level = as.numeric(input$filterSlider)/100
          }
          if(input$ratioSelection == USER_CUSTOM){
            ratio  = round(ncol(userInput$dataManager$rawData) * (as.numeric(input$ratioSlider)/100))
          }
          userProcessed$pdetectionFilter(userProcessed$rawData, filter_level, ratio)
          userProcessed$probeFilterPlot()
      })
      
    }else{
      NULL
    }
  })
  ################################################################
  
  #### SERVER-SIDE code for CONTRAST MATRiX section HERE####
  
  ###############################################################  
  observeEvent(input$contrastSubmitter, {
      userDesign <<- exp_design$new(input$group1Contrast, input$group2Contrast, userInput$targetManager$validGroups, userInput$targetManager$targets)
      if (userDesign$validSyntax){
        cat("\nwe str8 contrast design works")
      }else{
        cat("\nTO-DO:\nSomehow the console should display this is a problem and that the user should try again.")
      }
  })
  
  output$targetsTable <- renderDataTable({
        cat("Attempt to render targets table\n")
      if(isnt.null(userInput)){
          toDisplay <- as.data.frame(userInput$targetManager$targets)
          toDisplay 
      }
  })  
    ################################################################
    
    #### SERVER-SIDE code for ANALYSIS section HERE####
    
    ###############################################################
  observeEvent(input$analysisSubmitter, {
    completedAnalysis <<- differentialExpression$new(userDesign$contrastMatrix, 
                                    userProcessed$normalizedData, userDesign$designExpression)
    downloadReady <<- TRUE
  })
    
  output$topTable <- renderDataTable({
    
    if (input$analysisSelection == 2){
      completedAnalysis$topGeneTable()
    }else{
    }
  })
  
  output$vennDiagram <- renderPlot({
    
    if(input$analysisSelection == 3){
    completedAnalysis$scaledDiagram()
    }else{
    }
  })
  
  output$geneTable <- renderDataTable({
    if(input$analysisSelection == 4){
      completedAnalysis$allGeneTable()
    }else{
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