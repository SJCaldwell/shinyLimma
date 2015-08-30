library(shiny)
library(limma)
#Define server logic required to print whether dataset was uploaded

# setting this option. Here we'll raise limit to 130MB.
options(shiny.maxRequestSize = 130*1024^2)

shinyServer(function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$probeFile
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  output$contents <- renderTable({
    
    inFile2 <- input$controlProbeFile
    
    if (is.null(inFile2))
      return(NULL)
    
    read.csv(inFile2$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
      
  })
})