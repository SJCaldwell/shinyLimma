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
     checkboxInput("header", "Header", TRUE),
     radioButtons('sep', 'Seperator',
                  c(Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"),
                  ','),
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
     checkboxInput("header", "Header", TRUE),
     radioButtons('sep', 'Seperator',
                  c(Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"),
                  ','),
     tags$hr(),
     
     fileInput("targets", "Upload your targets text",
               accept = c(
                 'text/csv',
                 'text/comma-seperated-values',
                 'text/tab-seperated-values',
                 'text/plain',
                 '.csv'
               )
     ),
     tags$hr(),
     checkboxInput("header", "Header", TRUE),
     radioButtons('sep', 'Seperator',
                  c(Comma = ",",
                    Semicolon = ";",
                    Tab = "\t"),
                  ','),
     p("Version 0.1 of shiny-Limma accepts only Affymetrix Probe Data")
    ),
  mainPanel(
    tableOutput('contents')
  )
     
)
))