#removing stuff > rm(list=setdiff(ls(), "dd"))

library(ggplot2)
library(limma)
library(reshape)


densityPlotter <- function(rawLimma){
  	y <- melt(rawLimma$E)
  	p2 <- ggplot(y, aes(x = value)) + geom_density( aes(group = X2, colour = X2), adjust=10) + xlim(0, 300)
  	p2 <- p2 + labs(title = "Density of Probe Values")
  	p2 <- p2 + labs(x = "Probes", y = "Density")
  	p2 <- p2 + theme(legend.position= "none") + theme(plot.title = element_text(size = rel(1.3)))
  	return (p2)
}

boxplotBuilder <- function(rawLimma){
	toReturn <- boxplot((rawLimma$E),range=0,ylab="Probe Intensity Distribution")
	return (toReturn)
}