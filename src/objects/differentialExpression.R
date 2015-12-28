library(convert)

differentialExpression = R6Class("differentialExpression",
	public = list(
		model = NULL,
		topGenes = NULL,
		fullGenes = NULL,

		initialize = function(exp_design, normData, contrastExpression){
			fit = lmFit(as(normData,"EList"), exp_design)
			cont.matrix = makeContrasts(
				contrasts = (contrastExpression),
				levels = exp_design
				)
			fit2 = contrasts.fit(fit, cont.matrix)
			eFit = eBayes(fit2)
			self$model = eFit
			},

		topGeneTable = function(){
			return (topTable(self$model, adjust.method = "fdr"))
		},

		scaledDiagram = function(){
			criteria = decideTests(self$model)
			up = vennCounts(criteria, include = "up")[4]
			down = vennCounts(criteria, include = "down")[4]
			return (self$vennDiagramBuilder(up, down))

		},

		allGeneTable = function(){
			NUM_GENES = nrow(self$model)
			return (topTable(self$model, number = NUM_GENES))
		},

		vennDiagramBuilder = function(up, down){
			grid.newpage()
  			venn.plot <- draw.pairwise.venn(
  								  area1        = up,
                                  area2        = down,
                                  cross.area   = 0,
                                  scaled       = T,
                                  category     = c("up", "down"),
                                  fill         = c("red", "green"),
                                  alpha        = 0.3,
                                  lty          = "blank",
                                  cex          = 2,
                                  cat.cex      = 2,
                                  cat.pos      = c(200,150),
                                  cat.dist     = 0.10,
                                  cat.just     = list(c(-1, -1), c(1, 1)),
                                  ext.pos      = 30,
                                  ext.dist     = -0.05,
                                  ext.length   = 0.85,
                                  ext.line.lwd = 2,
                                  ext.line.lty = "dashed")
  			return(grid.draw(venn.plot))

		}
    )
)