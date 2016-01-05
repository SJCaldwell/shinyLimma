script_writer <- R6Class("script_writer",
	public = list(

		input = NULL,
		preprocessing = NULL,
		contrast = NULL,
		analysis = NULL,
 
		initialize = function(input, preprocessing, contrast, analysis){
			self$input = input
			self$preprocessing = preprocessing
			self$contrast = contrast
			self$analysis = analysis
		},

		output_script = function(){
			sink("research.R")
			cat("library(limma)\n")
			cat("library(statmod)\n")
			cat("library(fdrtool)\n\n")
			#Prepare Inputs
			cat("filepath <- ", "'", "probe.txt", "'\n", sep = "")
			cat("setwd(filepath)\n")
			cat("probeData <- ", "'", "controlProbe.txt", "'\n", sep = "")
			cat("CtrlProbe <- ", "'", "okay", "'\n", sep = "")
			cat("targets <- readTargets()","\n", sep = "")
			
			#Read in inputs
			cat('rawExpression <- read.ilmn(files = probeData, ctrlfiles = CtrlProbe, probeid="ProbeID", other.columns = "Detection")\n')

			#Quality Control
			cat("rawExpression <- backgroundCorrect(rawExpression, method = 'half')\n")
			cat("passedQC <- rowSums(rawExpression$other$Detection <=", self$preprocessing$filter_level,") >= ", self$preprocessing$ratio, ")\n", sep = "")
			#Wont be normalize VSN everytime
			#Add some logic here that determines which normalization was chosen
			self$normalization_handler()
			cat("normExpression <- normExpression[passedQC,]\n")
			#Get rid of magic numbers
			cat("NAMES <- 1\n")
			cat("geneTable <- normExpression$genes\n")
			cat("geneList <- geneNames[[NAMES]]\n")
			#Get rid of redundant survivors
			cat("normExpression <- avereps(normExpression, ID = geneList\n")

			self$GEC_handler()

			cat('design <- model.matrix(~0+GEC)\n')

			cat('colnames(design) <-levels(as.factor(GEC)\n')
			#Choices become relevant for deciding which code to produce
			#some lines must be altered, some must never be created at all
			if(self$preprocessing$bgCorrect){
				cat('corfit <- duplicateCorrelation(normExpression, design, block = ','targets','[[', 'block', ']]\n', sep="")
			}
			cat("fit <-lmFit(normExpression, design, block =", "targets$Donor")
			if (getCorrelationChoice()){
				cat(", correlation = corfit$consensus.correlation)\n", sep = "")
    		}else{
    			cat(")\n", sep = "")
    		}
			cat("cont.matrix <- makeContrasts(\n")
  			cat("\tcontrast = (", self$contrast$groupAsyntax, '-', self$contrast$groupBsyntax,'),\n', sep = "")
  	  		cat('\tlevels = design)\n', sep = "")
			cat("fit2 <- contrasts.fit(fit, cont.matrix)\n")
			cat("fit2<-eBayes(fit2)\n")
			cat("topTable(fit2, adjust.method = 'fdr')\n")
			cat("results <- decideTests(fit2, method = 'separate', adjust.method = 'fdr')")
			sink()
		},

		normalization_handler = function(){
			style = self$preprocessing$method
			NONE  = 1 
			VSN   = 2
			LOGQ  = 3
			LOESS = 4
			if (style == NONE){
				cat("normExpression <- rawExpression\n")
			}
			else if (style == VSN){
				cat("normExpression <- normalizeVSN(rawExpression)\n")
			}
			else if (style == LOGQ){
				cat("normExpression <- neqc(rawExpression)\n")
			}
			else if (style == LOESS){
				cat("normalizeBetweenArrays(rawExpression, method = cyclicloess, cylic.method = 'fast'\n")
			}
			else{
				return (-1)
			}
		},

		GEC_handler = function(){
			targets = self$input$targetManager$targets
			###get a session going n see whacha gotta do in targets to infer the correct numbers
			cat('GEC <- paste(targets$CSE, targets$Exp_Cont, targets$CSE, sep = "."\n')
		}
))