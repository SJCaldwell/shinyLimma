library(limma)

rawArray = R6Class("rawArray",
	public = list(
		rawData = NULL,
		initialize= function(probePath, controlPath){
		  self$rawData = read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
		  cat("\n raw data is type ", typeof(self$rawData))
		},

		rawMicroarray.getData = function(){
			return (self$rawData)
		},

		densityPlot = function(){
		    cat("\nplotting from rawArray class")
			  y <- melt(self$rawData$E)
  			p2 <- ggplot(y, aes(x = value)) + geom_density( aes(group = X2, colour = X2), adjust=10) + xlim(0, 300)
  			p2 <- p2 + labs(title = "Density of Probe Values")
  			p2 <- p2 + labs(x = "Probes", y = "Density")
  			p2 <- p2 + theme(legend.position= "none") + theme(plot.title = element_text(size = rel(1.3)))
  			return(p2)
		}
  )     
)