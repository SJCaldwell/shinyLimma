library(shiny)

#Define UI for Application Designed as a Limma workflow

shinyUI(navbarPage("Shinny-Limma!",
                   tabPanel("Choose Dataset",
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
                                
                                fileInput("targets", "Select Your Targets File",
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
                                #Button that reads in files and attempts to run ilmn
                                actionButton("fileSubmitter", "Submit files!"),
                                
                                imageOutput("uploadResult")
                                
                              )
                            )
                   ),
                   tabPanel("Preprocessing",
                            sidebarPanel(
                              
                              p("Select your preprocessing options here!"),
                              
                              br(),
                              br(),
                              strong("Background correction?"),
                              
                              checkboxInput("backgroundCheckbox", label = "Yes, I want background correction.", value = FALSE),
                              
                              br(),
                              br(),
                              
                              selectInput("normalizationSelection", label = "Choose desired normalization technique...",
                                          choices = list("No normalization..." = 1, "Variance Stabilization Transform" = 2, "Log Quantile" = 3),
                                          selected = 3),
                              
                              br(),
                              br(),
                              
                              selectInput("filteringSelection", label = "Choose desired filtering level for probes...",
                                          choices = list("No filtering..." = 1, "Exploratory (10%)" = 0.10, "Standard (5%)" = 0.05, "Conservative (1%)" = 0.01),
                                          selected = 3),
                              
                              br(),
                              br(),
                              
                              
                              actionButton("preprocessingSubmitter", "Preprocess my data!")
                              
                            )
                            
                            
                   ),                   
                   tabPanel("Contrast Matrix",
                            verbatimTextOutput("Put contrast stuff here somewhere.")
                            
                   ),
                   
                   tabPanel("Analysis",
                            verbatimTextOutput("Put Analysis stuff here somewhere.")
                            
                            
                            
                            
                   ),
                   
                   tabPanel("Export Reports",
                            verbatimTextOutput("Put reports stuff here somewhere.")
                            
                            
                   )
))