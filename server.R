library(shiny)
library(limma)
library(png)
#Define server logic required to print whether dataset was uploaded
# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

shinyServer(function(input, output) {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
  

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
    
    output$uploadResult <- renderImage({
      return(list(
        src = "images/Go_check.png",
        contentType = "image/png",
        alt = "Your file was uploaded. Move on to preprocessing!"
      ))
    }, deleteFile = FALSE)
    
  })
  
  
  
  

  

})