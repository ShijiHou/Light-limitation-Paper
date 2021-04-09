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
library(ggparty)

#data("gcSample")

R7 <- read.table("R7.txt", header=T, sep="\t")
S4 <- read.table("S4.txt", header=T, sep="\t")

R7 <- as.character(R7[,1])
S4 <- as.character(S4[,1])

r7 <- data.frame(R7, stringsAsFactors = FALSE)
s4 <- data.frame(S4, stringsAsFactors = FALSE)

test <- c(r7,s4)


# OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni", minGSSize = 10, maxGSSize = 500, pool = FALSE


xx <- compareCluster(test, fun="enrichGO",
                     OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni")


a=as.data.frame(xx)
ego=xx
ego@compareClusterResult <- ego@compareClusterResult[c(1:12,48),]

p1 <- emapplot(ego, showCategory=13, pie="count", pie_scale=1, layou="kk")
p1
#p1 <- emapplot(xx, showCategory=15, pie="count", pie_scale=1, layout="kk")+geom_scatterpie(aes_(color=c("gray","darkgray","green")))
#pdf("go_cluster_R7S4_12.pdf",width = 13,height = 10,useDingbats = F)
#p1
#dev.off()

b = as.data.frame(ego)

write.table(b, "R7S4_12GO.txt", sep="\t", quote=FALSE) 

g1 <- b
gene <- g1$geneID
gene <- as.character(gene)
gene1 <- strsplit(gene,split = "/")
genelist1 <- c(gene1[[1]],gene1[[2]],gene1[[3]],gene1[[4]],gene1[[5]],gene1[[6]],
               gene1[[7]],gene1[[8]],gene1[[9]],gene1[[10]],gene1[[11]],gene1[[12]],
               gene1[[13]])

ge1 <-c(gene1[[1]],gene1[[2]],gene1[[3]],gene1[[4]],gene1[[5]],gene1[[6]],
        gene1[[7]],gene1[[8]],gene1[[9]],gene1[[10]],gene1[[11]],gene1[[12]])
ge1 <-unique(ge1)
r7list <- as.data.frame(cbind(ge1,rep("-100",46)))
colnames(r7list) <- c("gene","cluster")

ge2 <-c(gene1[[13]])
ge2 <-unique(ge2)
s4list <- as.data.frame(cbind(ge2,rep("100",7)))
colnames(s4list) <- c("gene","cluster")


fc <- rbind(r7list,s4list)
write.table(fc, "clusterR7S4.txt", sep="\t", quote=FALSE)

fc <- read.table("clusterR7S4.txt", header=T, sep="\t")
genelist2 <- as.character(fc$gene)

edo <- enrichGO(gene = genelist2, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni")
edox <- setReadable(edo, 'org.At.tair.db')

a <- as.data.frame(edox)

ego1 <- edox
egoframe <- as.data.frame(ego1)

a1 <- which(egoframe$Description==b$Description[1])
b1 <- which(egoframe$Description==b$Description[2])
c1 <- which(egoframe$Description==b$Description[3])
d1 <- which(egoframe$Description==b$Description[4])
dd1 <- which(egoframe$Description==b$Description[5])
ddd1 <- which(egoframe$Description==b$Description[6])
e1 <- which(egoframe$Description==b$Description[7])
f1 <- which(egoframe$Description==b$Description[8])
g1 <- which(egoframe$Description==b$Description[9])
h1 <- which(egoframe$Description==b$Description[10])
i1 <- which(egoframe$Description==b$Description[11])
j1 <- which(egoframe$Description==b$Description[12])
k1 <- which(egoframe$Description==b$Description[13])


rowno <- c(a1,b1,c1,d1,dd1,ddd1,e1,f1,g1,h1,i1,j1,k1)

ego2<-edox
ego2@result <- ego1@result[rowno,]  

cluster <- fc$cluster
names(cluster) <- as.character(fc$gene)

  
p2 <- cnetplot(ego2,showCategory = 13,  categorySize=0.5, foldChange = cluster,  vertex.label.font=6, layout="nicely")+scale_color_gradient2(name="cluster",low = "tan4",mid="black",high = "green")

p2
pdf("Extended_Data_Figure_5e.pdf",width = 8,height = 6,useDingbats = F)
p2
dev.off()  
  
