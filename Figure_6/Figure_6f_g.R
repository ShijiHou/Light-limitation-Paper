library(ggplot2)
library(stringr)
library(clusterProfiler)
library(org.At.tair.db)
library(forcats)
library(DOSE)
library(plyr)
library(dplyr)
library(reshape)
library(tidygraph)
library(ggraph)
library(igraph)
library(scatterpie)
require(enrichplot)
require(viridis)

#data("gcSample")


S2 <- read.table("S2.txt", header=T, sep="\t")
S7 <- read.table("S7.txt", header=T, sep="\t")

S2 <- as.character(S2[,1])
S7 <- as.character(S7[,1])

s2 <- data.frame(S2, stringsAsFactors = FALSE)
s7 <- data.frame(S7, stringsAsFactors = FALSE)


test <- c(s2,s7)

# OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni", minGSSize = 10, maxGSSize = 500, pool = FALSE


xx <- compareCluster(test, fun="enrichGO",
                     OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP")


a=as.data.frame(xx)
ego=xx


p1 <- emapplot(xx, showCategory=12, pie="count", pie_scale=1, layou="kk")
p1
pdf("Figure_6f.pdf")
p1
dev.off()

a = as.data.frame(xx)

write.table(a, "all_go_S2S7.txt", sep="\t", quote=FALSE) 

asub <- a[c(1:12,198:209,483,498),]
write.table(asub,"top12_go_S2S7.txt", sep="\t", quote=FALSE)

g1 <- read.table("top12_go_S2S7.txt", header=T, sep="\t")
gene <- g1$geneID
gene <- as.character(gene)
gene1 <- strsplit(gene,split = "/")

genelist1 <- c(gene1[[1]],gene1[[2]],gene1[[3]],gene1[[4]],gene1[[5]],gene1[[6]],
               gene1[[7]],gene1[[8]],gene1[[9]],gene1[[10]],gene1[[11]],gene1[[12]])

genelist1 <- unique(genelist1)

s2list <- as.data.frame(cbind(genelist1,rep("-100",261)))
colnames(s2list) <- c("gene","cluster")


genelist2 <-c(gene1[[13]],gene1[[14]],gene1[[15]],gene1[[16]],gene1[[17]],gene1[[18]],
       gene1[[19]],gene1[[20]],gene1[[21]],gene1[[22]],gene1[[23]],gene1[[24]],gene1[[25]],gene1[[26]])
genelist2 <-unique(genelist2)
s7list <- as.data.frame(cbind(genelist2,rep("100",230)))
colnames(s7list) <- c("gene","cluster")

fc <- rbind(s2list,s7list)

write.table(fc, "cluster1.txt", sep="\t", quote=FALSE)

fc <- read.table("cluster1.txt", header=T, sep="\t")
genelist2 <- as.character(fc$gene)

edo <- enrichGO(gene = genelist2, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni")
edox <- setReadable(edo, 'org.At.tair.db')

b <- a

a <- as.data.frame(edox)

ego1 <- edox
egoframe <- as.data.frame(ego1)

for (g in unique(g1$Description)){
  x <- which(egoframe$Description == g)
  print(x)
}

rowno <- c(1,8,7,22,63,87,88,68,69,67,10,11,2,4,6,3,16,17,12,20,5,9,13,19)

ego2<-edox
ego2@result <- ego1@result[rowno,]  

cluster <- fc$cluster
names(cluster) <- as.character(fc$gene)

p2 <- cnetplot(ego2,showCategory = 24,  categorySize=1, foldChange = cluster,  vertex.label.font=6, layout="kk")+scale_color_gradient2(name="cluster",low = "darkviolet",high = "lightgreen")

p2
pdf("Figure_6g.pdf",width = 13,height = 10,useDingbats = F)
p2
dev.off()  

