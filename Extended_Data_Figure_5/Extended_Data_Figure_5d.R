library(ggplot2)
library(stringr)
library(clusterProfiler)
library(org.At.tair.db)
library(forcats)
library(GOSemSim)
library(DOSE)


s1 <- read.table("S1.txt", header=T, sep="\t")
genes1 <- as.character(s1[,1])
egos1 <- enrichGO(gene = genes1, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s1 <- cbind(as.data.frame(egos1),rep("S1",nrow(egos1)))
colnames(s1)[10] = "SampleID"

s2 <- read.table("S2.txt", header=T, sep="\t")
genes2 <- as.character(s2[,1])
egos2 <- enrichGO(gene = genes2, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s2 <- cbind(as.data.frame(egos2),rep("S2",nrow(egos2)))
colnames(s2)[10] = "SampleID"

s3 <- read.table("S3.txt", header=T, sep="\t")
genes3 <- as.character(s3[,1])
egos3 <- enrichGO(gene = genes3, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s3 <- cbind(as.data.frame(egos3),rep("S3",nrow(egos3)))
colnames(s3)[10] = "SampleID"

s4 <- read.table("S4.txt", header=T, sep="\t")
genes4 <- as.character(s4[,1])
egos4 <- enrichGO(gene = genes4, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s4 <- cbind(as.data.frame(egos4),rep("S4",nrow(egos4)))
colnames(s4)[10] = "SampleID"

s5 <- read.table("S5.txt", header=T, sep="\t")
genes5 <- as.character(s5[,1])
egos5 <- enrichGO(gene = genes5, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s5 <- cbind(as.data.frame(egos5),rep("S5",nrow(egos5)))
colnames(s5)[10] = "SampleID"

s6 <- read.table("S6.txt", header=T, sep="\t")
genes6 <- as.character(s6[,1])
egos6 <- enrichGO(gene = genes6, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s6 <- cbind(as.data.frame(egos6),rep("S6",nrow(egos6)))
colnames(s6)[10] = "SampleID"

s7 <- read.table("S7.txt", header=T, sep="\t")
genes7 <- as.character(s7[,1])
egos7 <- enrichGO(gene = genes7, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s7 <- cbind(as.data.frame(egos7),rep("S7",nrow(egos7)))
colnames(s7)[10] = "SampleID"

s8 <- read.table("S8.txt", header=T, sep="\t")
genes8 <- as.character(s8[,1])
egos8 <- enrichGO(gene = genes8, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
s8 <- cbind(as.data.frame(egos8),rep("S8",nrow(egos8)))
colnames(s8)[10] = "SampleID"


S1 <- s1[1:12,]
S2 <- s2[1:12,]
S3 <- s3[1:12,]
S5 <- s5[1:12,]
S6 <- s6[1:12,]


ego <- rbind(S1,S2,S3,s4,S5,S6,s7,s8)

##plot microbes vs nonmicrobes

p <- ggplot(ego, aes(x = SampleID, y = Description)) + 
  geom_point(aes(size = Count, color = p.adjust)) +
  scale_colour_gradient(limits=c(0, 0.01), low="red")+
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  scale_y_discrete(limits=c("lipid transport",
                            " ",
                            "very long-chain fatty acid metabolic process",
                            "syncytium formation",
                            "regulation of proton transport",
                            "proton transmembrane transport",
                            "polyol biosynthetic process",
                            "plant-type cell wall modification involved in multidimensional cell growth",
                            "plant-type cell wall loosening",
                            "pigment biosynthetic process",
                            "myo-inositol hexakisphosphate biosynthetic process",
                            "myo-inositol hexakisphosphate metabolic process",
                            "intracellular signal transduction",
                            "inositol phosphate metabolic process",
                            "inositol phosphate biosynthetic process",
                            "flavonoid metabolic process",
                            "flavonoid biosynthetic process",
                            "anthocyanin-containing compound metabolic process",
                            "anthocyanin-containing compound biosynthetic process",
                            "cell wall modification involved in multidimensional cell growth",
                            " ",
                            "response to heat",
                            "response to UV-B",
                            "response to high light intensity",
                            "response to light intensity",
                            " ",
                            "response to reactive oxygen species",
                            "response to oxidative stress",
                            "response to hydrogen peroxide",
                            "removal of superoxide radicals",
                            "cellular response to superoxide",
                            "cellular response to oxygen radical",
                            "cellular oxidant detoxification",
                            "cellular detoxification",
                            "superoxide metabolic process",
                            " ",
                            "response to karrikin",
                            "response to gibberellin",
                            "response to salicylic acid",
                            "response to jasmonic acid",
                            "jasmonic acid metabolic process",
                            "jasmonic acid mediated signaling pathway",
                            "jasmonic acid biosynthetic process",
                            "cellular response to jasmonic acid stimulus",
                            "response to ethylene",
                            "ethylene-activated signaling pathway",
                            " ",
                            "respiratory burst involved in defense response",
                            "respiratory burst",
                            " ",
                            "response to wounding",
                            "response to water deprivation",
                            "response to water",
                            "defense response to fungus",
                            "response to chitin",
                            "response to toxic substance",
                            "cellular response to toxic substance",
                            "response to organonitrogen compound",
                            "cellular response to antibiotic",
                            "regulation of innate immune response",
                            "regulation of immune system process",
                            "regulation of immune response",
                            "systemic acquired resistance"))+
  labs(title="Shoot_GO_top12")+
  theme(axis.text.x=element_text(size=6))+
  theme(axis.text.y = element_text(size=5))+
  theme(axis.title.y = element_text(size=5))+
  theme(axis.title.x=element_text(size=5))+
  theme(plot.title=element_text(size=5))


pdf("Extended_Data_Figure_5d.pdf",width = 5.5,height = 10,useDingbats = F)
p
dev.off()  