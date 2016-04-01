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
			code <<- ""
			code <<- c(code, "library(limma)\n")
			code <<- c(code, "library(statmod)\n")
			code <<- c(code, "library(fdrtool)\n\n")
			#Prepare Inputs
			code <<- c(code, "filepath <- ", "'", "probe.txt", "'\n")
			code <<- c(code, "setwd(filepath)\n")
			code <<- c(code, "probeData <- ", "'", "controlProbe.txt", "'\n")
			code <<- c(code, "CtrlProbe <- ", "'", "okay", "'\n")
			code <<- c(code, "targets <- readTargets()","\n")
			
			#Read in inputs
			code <<- c(code, 'rawExpression <- read.ilmn(files = probeData, ctrlfiles = CtrlProbe, probeid="ProbeID", other.columns = "Detection")\n')

			#Quality Control
			code <<- c(code, ("rawExpression <- backgroundCorrect(rawExpression, method = 'half')\n"))
			code <<- c(code, "passedQC <- rowSums(rawExpression$other$Detection <=", toString(self$preprocessing$filter_level),") >= ", toString(self$preprocessing$ratio), ")\n")
			#Wont be normalize VSN everytime
			#Add some logic here that determines which normalization was chosen
			self$normalization_handler()
			code <<- c(code, ("normExpression <- normExpression[passedQC,]\n"))
			#Get rid of magic numbers
			code <<- c(code, ("NAMES <- 1\n"))
			code <<- c(code, ("geneTable <- normExpression$genes\n"))
			code <<- c(code, ("geneList <- geneNames[[NAMES]]\n"))
			#Get rid of redundant survivors
			code <<- c(code, ("normExpression <- avereps(normExpression, ID = geneList\n"))

			self$GEC_handler()

			code <<- c(code, ('design <- model.matrix(~0+GEC)\n'))

			code <<- c(code, ('colnames(design) <-levels(as.factor(GEC)\n'))
			#Choices become relevant for deciding which code to produce
			#some lines must be altered, some must never be created at all
			if(self$preprocessing$bgCorrect){
				code <<- c(code, 'corfit <- duplicode <<- c(code, eCorrelation(normExpression, design, block = ','targets','[[', 'block', ']]\n')
			}
			code <<- c(code, "fit <-lmFit(normExpression, design, block = targets$Donor")
			if (getCorrelationChoice()){
				code <<- c(code, ", correlation = corfit$consensus.correlation)\n")
    		}else{
    			code <<- c(code, ")\n", sep = "")
    		}
			code <<- c(code, "cont.matrix <- makeContrasts(\n")
  			code <<- c(code, "\tcontrast = (", self$contrast$groupAsyntax, '-', self$contrast$groupBsyntax,'),\n')
  	  		code <<- c(code, '\tlevels = design)\n')
			code <<- c(code, "fit2 <- contrasts.fit(fit, cont.matrix)\n")
			code <<- c(code, "fit2<-eBayes(fit2)\n")
			code <<- c(code, "topTable(fit2, adjust.method = 'fdr')\n")
			code <<- c(code, "results <- decideTests(fit2, method = 'separate', adjust.method = 'fdr')")
			cat("TYPE OF code IS")
			cat(typeof(code))
			return(code)
		},

		normalization_handler = function(){
			style = self$preprocessing$method
			NONE  = 1 
			VSN   = 2
			LOGQ  = 3
			LOESS = 4
			if (style == NONE){
				code <<- c(code, ("normExpression <- rawExpression\n"))
			}
			else if (style == VSN){
				code <<- c(code, ("normExpression <- normalizeVSN(rawExpression)\n"))
			}
			else if (style == LOGQ){
				code <<- c(code, ("normExpression <- neqc(rawExpression)\n"))
			}
			else if (style == LOESS){
				code <<- c(code, ("normalizeBetweenArrays(rawExpression, method = cyclicloess, cylic.method = 'fast'\n"))
			}
			else{
				return (-1)
			}
		},

		GEC_handler = function(){
			targets = self$input$targetManager$targets
			###get a session going n see whacha gotta do in targets to infer the correct numbers
			code <<- c(code, ('GEC <- paste(targets$CSE, targets$Exp_Cont, targets$CSE, sep = "."\n'))
		}
))