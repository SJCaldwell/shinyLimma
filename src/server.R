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
source("helpers/scaledVennDiagram.R")
source("objects/datasetValidator.R")
source("objects/rawArray.R")
source("objects/normalizedArray.R")
source("objects/inputSL.R")
source("objects/metadata.R")
source("objects/exp_design.R")
source("objects/differentialExpression.R")
source("objects/script_writer.R")
source("objects/report_generator.R")
source("global.R", local = FALSE)

# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

shinyServer(function(input, output, session) {
  downloadReady <- FALSE
  
  isDownloadReady <- function(){
    return (downloadReady)
  }

showModal <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('show')"
                                             ,sep="")))
}

hideModal <- function(id, session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('hide')"
                                             ,sep="")))
}

  #Globals that will represent the user input.
  userInput = NULL
  userProcessed = NULL
  userDesign = NULL
  completedAnalysis = NULL
  scripter = NULL
  reporter = NULL

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
    showModal("loading", session)
    userInput <<- inputSL$new(input$probeFile, input$controlProbeFile, input$targets)
    output$rawPlot <- renderPlot({
      toShow <- userInput$densityPlot()
      hideModal("loading", session)
      toShow
      
  })
    ################################################################
    
          #### SERVER-SIDE code for EXPLORE section HERE####
    
    ###############################################################

    output$exploratoryText <- renderText({
      if (input$exploreSelection == 1){
        hide("exploratoryPlot")

      }
      
      else if (input$exploreSelection == 2){
        paste("Displaying Heatmap of Raw Data")
        hide("exploratoryPlot")
        toggle("exploratoryPlot")
      }
      
      else if(input$exploreSelection == 3){
        paste("Displaying boxplot of Raw Data")
        hide("exploratoryPlot")
        toggle("exploratoryPlot")
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
        showModal("matrixDone", session)
      }else{
        showModal("matrixFailed", session)
        
      }
  })
  
  output$targetsTable <- renderDataTable({
    cat("attempting to display targets table")
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
    showModal("analysisDone", session)
    
  })
    
  output$topTable <- renderDataTable({

    if (input$analysisSelection == 1){
        hide("vennDiagram")
        hide("geneTable")
        hide("topTable")
    }
    
    if (input$analysisSelection == 2){
      hide("vennDiagram")
      hide("geneTable")
      toggle("topTable")
      completedAnalysis$topGeneTable()
    }else{
    }
  })
  
  output$vennDiagram <- renderPlot({
    
    if(input$analysisSelection == 3){
      completedAnalysis$scaledDiagram()
      hide("topTable")
      hide("geneTable")
      toggle("vennDiagram")
    }else{
    }
  })
  
  output$geneTable <- renderDataTable({
    if(input$analysisSelection == 4){
      hide("topTable")
      hide("vennDiagram")
      toggle("geneTable")
      completedAnalysis$allGeneTable()

    }else{
    }
  })
 
  output$codeDownloader <- downloadHandler(
      filename = function(){
        paste('script-', Sys.Date(), '.R', sep ='')
      },
      content = function(con){
        scripter <<- script_writer$new(userInput, userProcessed, userDesign, completedAnalysis)
        code <- scripter$output_script()
        writeLines(code, con)
      }
    )

  output$reportDownloader <- downloadHandler(
    filename = function(){
      paste('report-', Sys.Date(), '.Rmd', sep = '')
      },
      content = function(con){
        title <- input$report_title
        author <- input$report_author
        output_type <- input$report_output
        reporter <<- report_generator$new(userInput, userProcessed, userDesign, completedAnalysis, title, author, output_type)
        report <- reporter$output_RMD()
        writeLines(report, con)
        })
})