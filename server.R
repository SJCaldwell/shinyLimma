library(shiny)
library(limma)
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
    probeFile = input$probeFile
    controlFile = input$controlProbeFile
    
    probePath = probeFile$datapath
    controlPath = controlFile$datapath
    
    cat("Original names are\n\n ", probePath, "\n", controlPath, "\n")
    
    probePath = substr(probePath, 1, nchar(probePath)-2)
    controlPath = substr(controlPath, 1, nchar(controlPath)-2)
    file = file(probePath)
    cat(probeFile$name, " is the name ", probePath, " PATH HERE \n\n")
    cat(controlPath, " PATH HERE \n\n")
    x <- read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
    cat("YOU DID IT YOU BEAUTIFUL PRICK")
  })
  

  

})