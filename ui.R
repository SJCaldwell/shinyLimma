library(shiny)
library(shinyBS)

ui <- (tagList(
  shinyjs::useShinyjs(),
  tags$head(tags$link(rel = 'icon', href = "favicon.ico", type = "image/x-icon")),
  titlePanel(title = "", windowTitle = "Shiny-Limma!"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  tags$head(tags$script(src = "js/script.js")),
  HTML("<div id='loading' class='modal fade' role='dialog'>
  <div class='modal-dialog'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h4 class='modal-title', align = 'center'>Loading, One Moment...</h4>
      </div>
    </div>
  </div>
</div>"),
    HTML("<div id='matrixDone' class='modal fade' role='dialog'>
  <div class='modal-dialog'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h4 class='modal-title', align = 'center'>Contrast Matrix Loaded</h4>
      </div>
    </div>
  </div>
</div>"),
  HTML("<div id='analysisDone' class='modal fade' role='dialog'>
  <div class='modal-dialog'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h4 class='modal-title', align = 'center'>Analysis Complete</h4>
      </div>
    </div>
  </div>
</div>"),
  HTML("<div id='matrixFailed' class='modal fade' role='dialog'>
  <div class='modal-dialog'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h4 class='modal-title', align = 'center'>Detected syntax error. Please try again.</h4>
      </div>
    </div>
  </div>
</div>"),
  navbarPage(title = a(href="http://shanecaldwell.info/shinyLimma/", "Shiny-Limma!"),
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
                  choices = list("No normalization" = 1, "Variance Stabilization Transform" = 2,
                   "Log Quantile" = 3, "Cyclic Loess" = 4),
                  selected = 3),
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
      fluidRow(
        column(5, offset = 1,
      
      h4("Before Preprocessing"),
      plotOutput("rawPlot2"),
      h4("After Preprocessing"),
      plotOutput("preprocessingPlot")
        ),
      column(5, offset = 1,
             
             h4("Filtering Results"),
             plotOutput("filteringResults"),
             br(),
             br(),
             actionButton("refilterButton", "Try out new filtering settings")

      )
    )
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
                 "Select a Gene" = 3),
                 selected = 1),
      conditionalPanel(
        condition = "input.analysisSelection == 3",
        selectInput("exportCriteria", label = p("Which genes to export"),
                    choices = list("All Genes" = "All", "Nominally Significant" = "P.Value", "Significant Post Multiple Test" = "adj.P.Val"),
                    selected = "All"),
        conditionalPanel(
          condition = "input.exportCriteria != 'All'",
          sliderInput("cutoff", "Significance Score Cutoff", min = 0, max = 1, value = 0.05, step = 0.01)
        ),
        downloadButton('downloadGenes', 'Download')
      )
              ),
    mainPanel(
      h3("Analysis Results"),
      dataTableOutput("topTable"),
      dataTableOutput("geneTable")
            )
              )
),

################################################################

#### CLIENT-SIDE code for EXPORT REPORTS section HERE####

###############################################################
tabPanel("Export Reports",
    sidebarLayout(
      sidebarPanel(
        h4("Reproducible Script"),
        br(),
        p("Hit the download button to get an R file you can share with your researchers, PI's, and friends!"),
        br(),
        downloadButton("codeDownloader", "Get my code!")
                  ),
      mainPanel(
        div(
        h4("Options For Report"),
          textInput("report_title", label = h5("Title of Report"),
                    value = "Unique title..."),
          textInput("report_author", label = h5("Author of the report"),
                    value = "Your name here!"),
          selectInput("report_output", label = h5("Style of Report to Generate"), 
                      choices = list("PDF" = "PDF", "HTML" = "HTML", "Word" = "Word"), selected = "PDF"),
        br(),
        downloadButton("reportDownloader", "Generate my report!"),
        align = "center")
               )
                  )
),
###This works but why###
  bsModal("modalExample", "What's a contrast matrix?", "contrastInfo", size = "large",
                      HTML(getTutorial()))
        )
))