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

R1 <- read.table("R1.txt", header=F, sep="\t")
R3 <- read.table("R3.txt", header=F, sep="\t")
S1 <- read.table("S1.txt", header=F, sep="\t")

R1 <- as.character(R1[,1])
R3 <- as.character(R3[,1])
S1 <- as.character(S1[,1])

r1 <- data.frame(R1, stringsAsFactors = FALSE)
r3 <- data.frame(R3, stringsAsFactors = FALSE)
s1 <- data.frame(S1, stringsAsFactors = FALSE)

test <- c(r1,r3,s1)


# OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni", minGSSize = 10, maxGSSize = 500, pool = FALSE


xx <- compareCluster(test, fun="enrichGO",
                     OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni")


a=as.data.frame(xx)
ego=xx
ego@compareClusterResult <- ego@compareClusterResult[c(1:12,33:44,125:136),]

p1 <- emapplot(ego, showCategory=18, pie="count", pie_scale=1, layou="kk")
p1
#p1 <- emapplot(xx, showCategory=15, pie="count", pie_scale=1, layout="kk")+geom_scatterpie(aes_(color=c("gray","darkgray","green")))
pdf("Figure_3d.pdf",width = 13,height = 10,useDingbats = F)
p1
dev.off()

b = as.data.frame(ego)

write.table(b, "S1R1R3_12GO.txt", sep="\t", quote=FALSE) 

g1 <- b
gene <- g1$geneID
gene <- as.character(gene)
gene1 <- strsplit(gene,split = "/")
genelist1 <- c(gene1[[1]],gene1[[2]],gene1[[3]],gene1[[4]],gene1[[5]],gene1[[6]],
               gene1[[7]],gene1[[8]],gene1[[9]],gene1[[10]],gene1[[11]],gene1[[12]],
               gene1[[13]],gene1[[14]],gene1[[15]],gene1[[16]],gene1[[17]],gene1[[18]],
               gene1[[19]],gene1[[20]],gene1[[21]],gene1[[22]],gene1[[23]],gene1[[24]],
               gene1[[25]],gene1[[26]],gene1[[27]],gene1[[28]],gene1[[29]],gene1[[30]],
               gene1[[31]],gene1[[32]],gene1[[33]],gene1[[34]],gene1[[35]],gene1[[36]])

ge1 <-c(gene1[[1]],gene1[[2]],gene1[[3]],gene1[[4]],gene1[[5]],gene1[[6]],
        gene1[[7]],gene1[[8]],gene1[[9]],gene1[[10]],gene1[[11]],gene1[[12]])
ge1 <-unique(ge1)
r1list <- as.data.frame(cbind(ge1,rep("-100",77)))
colnames(r1list) <- c("gene","cluster")

ge2 <-c(gene1[[13]],gene1[[14]],gene1[[15]],gene1[[16]],gene1[[17]],gene1[[18]],
        gene1[[19]],gene1[[20]],gene1[[21]],gene1[[22]],gene1[[23]],gene1[[24]])
ge2 <-unique(ge2)
r3list <- as.data.frame(cbind(ge2,rep("-80",128)))
colnames(r3list) <- c("gene","cluster")

ge3 <-c(gene1[[25]],gene1[[26]],gene1[[27]],gene1[[28]],gene1[[29]],gene1[[30]],
        gene1[[31]],gene1[[32]],gene1[[33]],gene1[[34]],gene1[[35]],gene1[[36]])
ge3 <-unique(ge3)
s1list <- as.data.frame(cbind(ge3,rep("100",286)))
colnames(s1list) <- c("gene","cluster")

gee <- c(ge1,ge2,ge3)

ge13 <- intersect(ge1,ge3)
gee13 <- rep("0",21)
r1s1 <- as.data.frame(cbind(ge13,gee13))
colnames(r1s1) <- c("gene","cluster")

ge23 <- intersect(ge2,ge3)
gee23 <- rep("0",55)
r3s1 <- as.data.frame(cbind(ge23,gee23))
colnames(r3s1) <- c("gene","cluster")

sepr1 <- anti_join(r1list,s1list,by="gene")
sepr3 <- anti_join(r3list,s1list, by="gene")
r13 <- rbind(r1list,r3list)
seps1 <- anti_join(s1list,r13, by="gene")

fc <- rbind(r1s1,r3s1,sepr1,sepr3,seps1)
write.table(fc, "cluster.txt", sep="\t", quote=FALSE)

fc <- read.table("cluster.txt", header=T, sep="\t")
genelist2 <- as.character(fc$gene)

edo <- enrichGO(gene = genelist2, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "bonferroni")
edox <- setReadable(edo, 'org.At.tair.db')

a <- as.data.frame(edox)

ego1 <- edox
egoframe <- as.data.frame(ego1)

a1 <- which(egoframe$Description==b$Description[3])
b1 <- which(egoframe$Description==b$Description[7])
c1 <- which(egoframe$Description==b$Description[15])
d1 <- which(egoframe$Description==b$Description[16])
dd1 <- which(egoframe$Description==b$Description[17])
ddd1 <- which(egoframe$Description==b$Description[12])
e1 <- which(egoframe$Description==b$Description[25])
f1 <- which(egoframe$Description==b$Description[26])
g1 <- which(egoframe$Description==b$Description[27])
h1 <- which(egoframe$Description==b$Description[28])
i1 <- which(egoframe$Description==b$Description[29])
j1 <- which(egoframe$Description==b$Description[30])
k1 <- which(egoframe$Description==b$Description[31])
l1 <- which(egoframe$Description==b$Description[32])
m1 <- which(egoframe$Description==b$Description[33])
n1 <- which(egoframe$Description==b$Description[34])
o1 <- which(egoframe$Description==b$Description[35])
p1 <- which(egoframe$Description==b$Description[36])


rowno <- c(a1,b1,c1,d1,dd1,ddd1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1)

ego2<-edox
ego2@result <- ego1@result[rowno,]  

cluster <- fc$cluster
names(cluster) <- as.character(fc$gene)

  
p2 <- cnetplot(ego2,showCategory = 18,  categorySize=1, foldChange = cluster,  vertex.label.font=6, layout="kk")+scale_color_gradient2(name="cluster",low = "tan4",mid="black",high = "green")

p2
pdf("Figure_3e.pdf",width = 13,height = 10,useDingbats = F)
p2
dev.off()  
  
