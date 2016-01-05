library(limma)
library(statmod)
library(fdrtool)

probeData <- 'lungprobe.txt'
CtrlProbe <- 'controlprobe.txt'
filepath <- 'desktop/shinyLimma'
setwd(filepath)
targets <- readTargets()
rawExpression <- read.ilmn(files = probeData, ctrlfiles = CtrlProbe, 
						probeid="ProbeID", other.columns = "Detection")
rawExpression <- backgroundCorrect(rawExpression, method = 'half')
passedQC <- rowSums(rawExpression$other$Detection <= 15) >= 20)
normExpression <- normalizeVSN(rawExpression)
normExpression <- normExpression[passedQC,]
NAMES <- 1
geneTable <- normExpression$genes
geneList <- geneNames[[NAMES]]
normExpression <- avereps(normExpression, ID = geneList
GEC <- paste(targets$Genotype, targets$Exp_Cont, targets$CSE, sep = "."
design <- model.matrix(~0+GEC)
colnames(design) <-levels(as.factor(GEC)
fit <-lmFit(normExpression, design, block = targets$)
cont.matrix <- makeContrasts(
contrast = (-),
levels = design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2<-eBayes(fit2)
topTable(fit2, adjust.method = 'fdr')
results <- decideTests(fit2, method = 'separate', adjust.method = 'fdr')