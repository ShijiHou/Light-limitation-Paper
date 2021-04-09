library(gplots)
library(ggplot2)
library(dplyr)

chip2019 <- read.table("MYC2_JA_2019.txt", header=F, sep="\t")
chip2019 <- unique(chip2019)
colnames(chip2019) <- c("x")

chip2020 <- read.table("MYC2_JA_2020.txt", header=F, sep="\t")
chip2020 <- unique(chip2020)
colnames(chip2020) <- c("x")

chip2019PNAS <- read.table("MYC2_JA_2019_PNAS.txt", header=F, sep="\t")
chip2019PNAS <- unique(chip2019PNAS)
colnames(chip2019PNAS) <- c("x")

s1 <- read.table("S1.txt", header=T, sep="\t")
s2 <- read.table("S2.txt", header=T, sep="\t")
s3 <- read.table("S3.txt", header=T, sep="\t")
s4 <- read.table("S4.txt", header=T, sep="\t")
s5 <- read.table("S5.txt", header=T, sep="\t")
s6 <- read.table("S6.txt", header=T, sep="\t")
s7 <- read.table("S7.txt", header=T, sep="\t")
s8 <- read.table("S8.txt", header=T, sep="\t")

r1 <- read.table("R1.txt", header=T, sep="\t")
r2 <- read.table("R2.txt", header=T, sep="\t")
r3 <- read.table("R3.txt", header=T, sep="\t")
r4 <- read.table("R4.txt", header=T, sep="\t")
r5 <- read.table("R5.txt", header=T, sep="\t")
r6 <- read.table("R6.txt", header=T, sep="\t")
r7 <- read.table("R7.txt", header=T, sep="\t")
r8 <- read.table("R8.txt", header=T, sep="\t")

shoot <- rbind(s1,s2,s3,s4,s5,s6,s7,s8)
root <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)

shootchip2019PNAS <- inner_join(shoot, chip2019PNAS, by="x")
shootchip2019 <- inner_join(shoot, chip2019, by="x")
shootchip2020 <- inner_join(shoot, chip2020, by="x")
chip2019PNASchip2019 <- inner_join(chip2019PNAS, chip2019, by="x")
chip2019PNASchip2020 <- inner_join(chip2019PNAS, chip2020, by="x")
chip2019chip2020 <- inner_join(chip2019, chip2020, by="x")

shootchip2019PNASchip2019 <- inner_join(shootchip2019PNAS, chip2019, by="x")
shootchip2019PNASchip2020 <- inner_join(shootchip2019PNAS, chip2020, by="x")
chip2019chip2020shoot <- inner_join(chip2019chip2020, shoot, by="x")
chip2019chip2020chip2019PNAS <- inner_join(chip2019chip2020, chip2019PNAS, by="x")

shootchip2019chip2020chip2019PNAS <- inner_join(shoot, chip2019chip2020chip2019PNAS, by="x")


library("VennDiagram") 
venn.plot <- draw.quad.venn(
  area1 = 5038,
  area2 = 1511,
  area3 = 8789,
  area4 = 6178,
  n12 = 426,
  n13 = 2572,
  n14 = 1632,
  n23 = 1020,
  n24 = 998,
  n34 = 4422,
  n123 = 366,
  n124 = 320,
  n134 = 1337,
  n234 = 834,
  n1234 = 292,
  category = c("Shoot", "chip2019PNAS", "chip2019", "chip2020"),
  fill = c("orange", "blue", "turquoise", "maroon1"),
  lty = "dashed",
  cex = 2,
  cat.cex = 2,
  cat.col = c("green", "pink", "orange", "blue")
)

grid.draw(venn.plot)

library(grDevices)

pdf(file="Extended_Data_Figure_9a_Shoot.pdf")
grid.draw(venn.plot)
dev.off()


rootchip2019PNAS <- inner_join(root, chip2019PNAS, by="x")
rootchip2019 <- inner_join(root, chip2019, by="x")
rootchip2020 <- inner_join(root, chip2020, by="x")
chip2019PNASchip2019 <- inner_join(chip2019PNAS, chip2019, by="x")
chip2019PNASchip2020 <- inner_join(chip2019PNAS, chip2020, by="x")
chip2019chip2020 <- inner_join(chip2019, chip2020, by="x")

rootchip2019PNASchip2019 <- inner_join(rootchip2019PNAS, chip2019, by="x")
rootchip2019PNASchip2020 <- inner_join(rootchip2019PNAS, chip2020, by="x")
chip2019chip2020root <- inner_join(chip2019chip2020, root, by="x")
chip2019chip2020chip2019PNAS <- inner_join(chip2019chip2020, chip2019PNAS, by="x")

rootchip2019chip2020chip2019PNAS <- inner_join(root, chip2019chip2020chip2019PNAS, by="x")

library("VennDiagram") 
venn.plot1 <- draw.quad.venn(
  area1 = 5231,
  area2 = 1511,
  area3 = 8789,
  area4 = 6178,
  n12 = 357,
  n13 = 2257,
  n14 = 1659,
  n23 = 1020,
  n24 = 998,
  n34 = 4422,
  n123 = 285,
  n124 = 262,
  n134 = 1259,
  n234 = 834,
  n1234 = 232,
  category = c("Root", "chip2019PNAS", "chip2019", "chip2020"),
  fill = c("orange", "blue", "turquoise", "maroon1"),
  lty = "dashed",
  cex = 2,
  cat.cex = 2,
  cat.col = c("green", "pink", "orange", "blue")
)

grid.draw(venn.plot1)

library(grDevices)

pdf(file="Extended_Data_Figure_9a_Root.pdf")
grid.draw(venn.plot1)
dev.off()

