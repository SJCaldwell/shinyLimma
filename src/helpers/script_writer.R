#Programmer: Shane Caldwell
writeScript <- function(){
sink("research.R")
cat("library(limma)\n")
cat("library(statmod)\n")
cat("library(fdrtool)\n")
#Prepare Inputs
cat("probeData <- ", "'", getProbeFile(), "'\n", sep = "")
cat("CtrlProbe <- ", "'", getControlProbeFile(), "'\n", sep = "")
cat("filepath <- ", "'", "GET PATH", "'\n", sep = "")
cat("setwd(filepath)\n")
cat("targets <- readTargets(", "'", getTargetPath(), "'\n", sep = "")

#Read in inputs
cat('rawExpression <- read.ilmn(files = probeData, ctrlfiles = CtrlProbe, probeid="ProbeID", other.columns = "Detection")\n')

#Quality Control
cat("rawExpression <- backgroundCorrect(rawExpression, method = 'half')\n")
cat("passedQC <- rowSums(rawExpression$other$Detection <=", getFilterLevel(),") >= ", getRatio(), ")\n", sep = "")
cat("normExpression <- normalizeVSN(rawExpression)\n")
cat("normExpression <- normExpression[passedQC,]\n")

#Get rid of magic numbers
cat("NAMES <- 1\n")
cat("geneTable <- normExpression$genes\n")
cat("geneList <- geneNames[[NAMES]]\n")

#Get rid of redundant survivors
cat("normExpression <- avereps(normExpression, ID = geneList\n")

cat('GEC <- paste(targets$Genotype, targets$Exp_Cont, targets$CSE, sep = "."\n')

cat('design <- model.matrix(~0+GEC)\n')

cat('colnames(design) <-levels(as.factor(GEC)\n')
#Choices become relevant for deciding which code to produce
#some lines must be altered, some must never created at all
if(getCorrelationChoice()){
cat('corfit <- duplicateCorrelation(normExpression, design, block = ','targets','[[', 'block', ']]\n', sep="")
}

cat("fit <-lmFit(normExpression, design, block =", "targets$Donor")
	if (getCorrelationChoice()){
		cat(", correlation = corfit$consensus.correlation)\n", sep = "")
    }else{
    	cat(")\n", sep = "")
    }
cat("cont.matrix <- makeContrasts(\n
    contrast = (", getGroup1(), '-', getGroup2(),'),\n',
  'levels = design)\n\n', sep = "")
cat("fit2 <- contrasts.fit(fit, cont.matrix)\n")
cat("fit2<-eBayes(fit2)\n")
cat("topTable(fit2, adjust.method = 'fdr')\n")
cat("results <- decideTests(fit2, method = 'separate', adjust.method = 'fdr')")
sink()
}