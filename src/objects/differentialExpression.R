library(convert)

differentialExpression = R6Class("differentialExpression",
	public = list(
		model = NULL,
		topGenes = NULL,
		fullGenes = NULL,

		initialize = function(exp_design, normData, contrastExpression){
			cat("\nincorrect number of subscripts on matrix?")
			cat("\n",typeof(normData))
			fit = lmFit(as(normData,"EList"), exp_design)
			cat("\n model is fitted")
			cont.matrix = makeContrasts(
				contrasts = (contrastExpression),
				levels = exp_design
				)
			cat("\ncontrast is made...")
			fit2 = contrasts.fit(fit, cont.matrix)
			cat("\nContrast is fit")
			eFit = eBayes(fit2)
			cat("\nBayesian model completed!")
			self$model = eFit
			},

		topGeneTable = function(){
			return (topTable(self$model, adjust.method = "fdr"))
		}



))