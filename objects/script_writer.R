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
			code <- ""
			code <- paste0(code, "library(limma)\n")
			code <- paste0(code, "library(statmod)\n")
			code <- paste0(code, "library(fdrtool)\n\n")
			code <- paste0(code, "filepath <- ", "'", "Enter path to probe file, control probe file, and target file here", "'\n")
			code <- paste0(code, "setwd(filepath)\n")
			code <- paste0(code, "probeData <- ", "'", self$input$probe_path, "'\n")
			code <- paste0(code, "CtrlProbe <- ", "'", self$input$control_path, "'\n")
			code <- paste0(code, "targets <- readTargets()","\n")
			code <- paste0(code, 'rawExpression <- read.ilmn(files = probeData, ctrlfiles = CtrlProbe, probeid="ProbeID", other.columns = "Detection")\n')

			# #Quality Control
			code <- self$background_correct_handler(code)
			code <- paste0(code, "passedQC <- rowSums(rawExpression$other$Detection <=", toString(self$preprocessing$filter_level),") >= ", toString(self$preprocessing$ratio), "\n")
			
			# self$normalization_handler()
			code <- paste0(code, ("normExpression <- normExpression[passedQC,]\n"))
			# #Get rid of magic numbers
			code <- paste0(code, ("NAMES <- 1\n"))
			code <- paste0(code, ("geneTable <- normExpression$genes\n"))
			code <- paste0(code, ("geneList <- geneNames[[NAMES]]\n"))
			# #Get rid of redundant survivors
			code <- paste0(code, ("normExpression <- avereps(normExpression, ID = geneList\n"))

			# self$GEC_handler()

			code <- paste0(code, ('design <- model.matrix(~0+GEC)\n'))

			code <- paste0(code, ('colnames(design) <-levels(as.factor(GEC))\n'))
			# #Choices become relevant for deciding which code to produce
			# #some lines must be altered, some must never be created at all
			if(self$preprocessing$bgCorrect){
				code <- paste0(code, 'corfit <- duplicode <<- paste0(code, eCorrelation(normExpression, design, block = ','targets','[[', 'block', ']]\n')
			 }
			code <- paste0(code, "fit <-lmFit(normExpression, design, block = targets$Donor")
			if (getCorrelationChoice()){
			 	code <- paste0(code, ", correlation = corfit$consensus.correlation)\n")
     		}else{
     			code <- paste0(code, ")\n", sep = "")
     		}
			code <- paste0(code, "cont.matrix <- makeContrasts(\n")
  	 	    code <- paste0(code, "\tcontrast = (", self$contrast$groupAsyntax, '-', self$contrast$groupBsyntax,'),\n')
  	        code <- paste0(code, '\tlevels = design)\n')
			code <- paste0(code, "fit2 <- contrasts.fit(fit, cont.matrix)\n")
			code <- paste0(code, "fit2<-eBayes(fit2)\n")
			code <- paste0(code, "topTable(fit2, adjust.method = 'fdr')\n")
			code <- paste0(code, "results <- decideTests(fit2, method = 'separate', adjust.method = 'fdr')")
			return (code)
		},

		normalization_handler = function(){
			style = self$preprocessing$method
			NONE  = 1 
			VSN   = 2
			LOGQ  = 3
			LOESS = 4
			if (style == NONE){
				code <<- paste0(code, ("normExpression <- rawExpression\n"))
			}
			else if (style == VSN){
				code <<- paste0(code, ("normExpression <- normalizeVSN(rawExpression)\n"))
			}
			else if (style == LOGQ){
				code <<- paste0(code, ("normExpression <- neqc(rawExpression)\n"))
			}
			else if (style == LOESS){
				code <<- paste0(code, ("normalizeBetweenArrays(rawExpression, method = cyclicloess, cylic.method = 'fast'\n"))
			}
			else{
				return (-1)
			}
		},

		GEC_handler = function(){
			targets = self$input$targetManager$targets
			###get a session going n see whacha gotta do in targets to infer the correct numbers
			code <<- paste0(code, ('GEC <- paste(targets$CSE, targets$Exp_Cont, targets$CSE, sep = "."\n'))
		},

		background_correct_handler = function(code){
			if (self$preprocessing$bgCorrect){
		    	code <- paste0(code, ('rawExpression <- backgroundCorrect(rawExpression, method = 	"half")\n'))
			}
		  return (code)
}
))
