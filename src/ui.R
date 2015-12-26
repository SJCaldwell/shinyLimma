library(shiny)
library(shinyBS)

ui <- (tagList(
  #Necessary call to shinyjs, independent of any tab. DON'T MOVE
  shinyjs::useShinyjs(),
  tags$head(tags$link(rel = 'icon', href = "favicon.ico", type = "image/x-icon")),
  titlePanel(title = "", windowTitle = "Shiny-Limma!"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  navbarPage(title = a(href="http://shanecaldwell.info/2015/02/06/shinyLimma.html", "Shiny-Limma!"),
################################################################

#### CLIENT-SIDE code for CHOOSE DATASET section HERE ####

###############################################################
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
      p("Version 0.1 of shiny-Limma accepts only Illumina Probe Data"),
      tags$hr(),
      actionButton("fileSubmitter", "Submit files!")
                ),

    mainPanel(
      tags$style(type="text/css", "body { overflow-y: scroll; }"),
      #Button that reads in files and attempts to run ilmn
      plotOutput("rawPlot")
             )
                    )
  ),
################################################################

#### CLIENT-SIDE code for EXPLORE section HERE####

###############################################################
tabPanel("Explore",
  sidebarLayout(
    sidebarPanel(
      strong("Explore"),
      br(),
      p("This is your data. Visualizing it will help you understand
        it and make better preprocessing decisions."),
      br(),
      br(),
      selectInput("exploreSelection", label = "Desired Data Visualization",
                  choices = list("Choose your technique..." = 1, "Heatmap" = 2, "Boxplot" = 3),
                  selected = 1),
      br(),
      br(),
      strong("Array Quality Report"),
      br(),
      actionButton("QCGenerator","Generate Report"),
      br(),
      br(),
      p("ArrayQC reports cover a gambit of quality
        control reports for arrays. This will open a new tab upon completion,
        but could take a little while to run."),
      conditionalPanel(
          condition = "output.download == true",
          strong("YOUR DOWNLOAD IS RDY AF NOW")
                      ),
      br()
                ),
    mainPanel(
      textOutput("exploratoryText"),
      plotOutput("exploratoryPlot")
             )
                )
),
################################################################

#### CLIENT-SIDE code for PREPROCESSING section HERE####

###############################################################
tabPanel("Preprocessing",
  sidebarLayout(
    sidebarPanel(
      p("Select your preprocessing options here!"),
      br(),
      br(),
      strong("Background correction?"),
      checkboxInput("backgroundCheckbox",
        label = "Yes, I want background correction.", value = FALSE),
      br(),
      br(),
      selectInput("normalizationSelection", label = "Choose desired normalization technique...",
                  choices = list("No normalization..." = 1, "Variance Stabilization Transform" = 2,
                   "Log Quantile" = 3, "Cyclic Loess" = 4),
                  selected = 1),
      br(),
      br(),

      selectInput("filteringSelection", label = "Choose desired filtering level for probes...",
                 choices = list("No filtering..." = 1, "Exploratory (10%)" = 0.10,
                 "Standard (5%)" = 0.05, "Conservative (1%)" = 0.01, "Choose my own value" = 4),
                  selected = 1),

      conditionalPanel(
                 condition = "input.filteringSelection == 4",
                 sliderInput("filterSlider", "Select your own percentage:",
                min = 0, max = 100, value = 5)
                      ),
      br(),
      br(),

      selectInput("ratioSelection", label = "What percentage of the samples must meet the filtering criteria?",
                 choices = list("None of them" = 1, "Exploratory (25%)" = 0.25, "Standard (15%)" = 0.15,
                                "Conservative (10%)" = 0.10, "Choose my own value" = 4),
                 selected = 1),
      conditionalPanel(
                condition = "input.ratioSelection == 4",
                sliderInput("ratioSlider", "Select your own ratio:",
               min = 0, max = 100, value = 10)
                      ),

      br(),
      br(),

      actionButton("preprocessingSubmitter", "Preprocess my data!")
                 ),

    mainPanel(
      h4("Before Preprocessing"),
      plotOutput("rawPlot2"),
      h4("After Preprocessing"),
      plotOutput("preprocessingPlot")

            )
              )
),

################################################################

#### CLIENT-SIDE code for CONTRAST MATRIX section HERE####

###############################################################
tabPanel("Contrast Matrix",
  sidebarLayout(
    sidebarPanel(
      p("Read through the results of your analysis here!"),
      br(),
      br(),
      textInput("group1Contrast", "Group1"),
      br(),
      strong("VS"),
      br(),
      br(),
      br(),
      textInput("group2Contrast", "Group2"),
      br(),
      br(),
      actionButton("contrastSubmitter", "Contrast matrix is done"),
      br(),
      br(),
      actionLink("contrastInfo","How do I specify a contrast matrix?")
                ),
    mainPanel(
      h4("Specification of Contrast Matrix"),
        dataTableOutput("targetsTable")
              )
                )
         ),
################################################################

#### CLIENT-SIDE code for ANALYSIS section HERE####

###############################################################

tabPanel("Analysis",
  sidebarLayout(
    sidebarPanel(
      p("Read through the results of your analysis here!"),
      br(),
      br(),
      actionButton("analysisSubmitter", "Run my Linear Model!"),
      br(),
      br(),
      br(),
      br(),
      selectInput("analysisSelection", label = "How do you want to look at your results?",
                 choices = list("Take your pick!" = 1, "Top Table" = 2,
                 "Venn Diagram" = 3, "Select a Gene" = 4),
                 selected = 1)
              ),
    mainPanel(
      h3("Analysis Results"),
      dataTableOutput("topTable"),
      dataTableOutput("geneTable"),
      plotOutput("vennDiagram")
            )
              )
),

################################################################

#### CLIENT-SIDE code for EXPORT REPORTS section HERE####

###############################################################
tabPanel("Export Reports",
    sidebarLayout(
      sidebarPanel(
        p("Export your results here!"),
        br(),
        br(),
        actionButton("codeDownloader", "Get my code.")
                  ),
      mainPanel(
        h4("Options")
               )
                  )
),
###This works but why###
  bsModal("modalExample", "What's a contrast matrix?", "contrastInfo", size = "large",
                      HTML(getTutorial()))
        )
))