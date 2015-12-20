library(vennDiagram)
ScaledVennDiagram <- function(up, down, neutral){
  grid.newpage()
  venn.plot <- draw.pairwise.venn(area1        = up,
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