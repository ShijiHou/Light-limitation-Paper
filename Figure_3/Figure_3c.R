#install.packages("VennDiagram")

library (VennDiagram)
library(limma)

length_a <- 875
length_b <- 932
length_ab <- 227

venn.plot <- draw.pairwise.venn(area1=932,area2 = 875, cross.area=227,
 scaled = TRUE, inverted = TRUE,
  fill = c("green", "gray"),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.pos = c(285, 105),
  cat.dist = 0.09,
  cat.just = list(c(-1, -1), c(1, 1)),
  ext.pos = 30,
  ext.dist = -0.05,
  ext.length = 0.85,
  ext.line.lwd = 2,
  ext.line.lty = "dashed")

pdf("Figure_3c.pdf")
grid.draw(venn.plot)
dev.off()

