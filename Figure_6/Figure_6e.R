library(gplots)
library(ggplot2)
library(dplyr)

col0 <- read.table("shoot_geneID_clusterID_col0.txt", header=T, sep="\t")
col0 <- as.matrix(col0)
col0 <- as.data.frame(col0)


s2 <- read.table("S2.txt", header=T, sep="\t")
gene1 <- as.matrix(s2[,1])
S2 <- cbind(gene1, rep("MDE_S2", nrow(gene1)))
colnames(S2) = c("GeneID","ClusterID")
S2 <- as.data.frame(S2)

inter = inner_join(S2,col0, by = "GeneID")
inter1 = as.matrix(inter[,3])

mdeS2 <- as.data.frame(table(inter1))
sa = sum(mdeS2$Freq)
na = c("Not_find",nrow(S2)-sa)
na.row=matrix(na, nrow = 1)
na = as.matrix(na.row)
colnames(na)=c("inter1","Freq")
mdeS2 <- rbind(mdeS2,na)
x = as.numeric(mdeS2$Freq)
piepercenta <- round(100*x/sum(x), 1)
piepercenta <-paste(piepercenta, "%", sep = "")

pdf("Figure_6e_MED_S2.pdf")
pa <- pie(as.numeric(mdeS2$Freq), labels = piepercenta, col = c("darkgreen","blue","yellow","yellow4","gray","orange","black","skyblue","white"))
dev.off()


s7 <- read.table("S7.txt", header=T, sep="\t")
gene1 <- as.matrix(s7[,1])
S7 <- cbind(gene1, rep("MDE_S7", nrow(gene1)))
colnames(S7) = c("GeneID","ClusterID")
S7 <- as.data.frame(S7)

inter = inner_join(S7,col0, by = "GeneID")
inter1 = as.matrix(inter[,3])

mdeS7 <- as.data.frame(table(inter1))
sa = sum(mdeS7$Freq)
na = c("Not_find",nrow(S7)-sa)
na.row=matrix(na, nrow = 1)
na = as.matrix(na.row)
colnames(na)=c("inter1","Freq")
mdeS7 <- rbind(mdeS7,na)
x = as.numeric(mdeS7$Freq)
piepercenta <- round(100*x/sum(x), 1)
piepercenta <-paste(piepercenta, "%", sep = "")

pdf("Figure_6e_MED_S7.pdf")
pa <- pie(as.numeric(mdeS7$Freq), labels = piepercenta, col = c("darkgreen","blue","yellow","yellow4","gray","orange","skyblue","white"))
dev.off()