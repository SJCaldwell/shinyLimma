library(shiny)

#Define UI for Application Designed as a Limma workflow

shinyUI(fluidPage(
  titlePanel("Choose Dataset"),
  sidebarLayout(
    sidebarPanel(
      fileInput("probeFile", "Select Affymetrix Probe File",
                accept = c(
                  'text/csv',
                  'text/comma-seperated-values',
                  'text/tab-seperated-values',
                  'text/plain',
                  '.csv'
                )
     ),
     tags$hr(),

     
     fileInput("controlProbeFile", "Select Affymetrix Control Probe File",
               accept = c(
                 'text/csv',
                 'text/comma-seperated-values',
                 'text/tab-seperated-values',
                 'text/plain',
                 '.csv'
               )
      ),
     tags$hr(),
     
     fileInput("targets", "Upload your targets.txt",
               accept = c(
                 'text/csv',
                 'text/comma-seperated-values',
                 'text/tab-seperated-values',
                 'text/plain',
                 '.csv'
               )
     ),
     tags$hr(),
     p("Version 0.1 of shiny-Limma accepts only Illumina Probe Data")
    ),
    
    
  mainPanel(
    tableOutput('contents'),
    #Button that reads in files and attempts to run ilmn
    actionButton("fileSubmitter", "Submit files!")
    
  )
     
)
))