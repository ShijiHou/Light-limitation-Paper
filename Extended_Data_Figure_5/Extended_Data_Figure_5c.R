library(ggplot2)
library(stringr)
library(clusterProfiler)
library(org.At.tair.db)
library(forcats)
library(GOSemSim)
library(DOSE)

r1 <- read.table("R1.txt", header=T, sep="\t")
gener1 <- as.character(r1[,1])
egor1 <- enrichGO(gene = gener1, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r1 <- cbind(as.data.frame(egor1),rep("R1",nrow(egor1)))
colnames(r1)[10] = "SampleID"

r2 <- read.table("R2.txt", header=T, sep="\t")
gener2 <- as.character(r2[,1])
egor2 <- enrichGO(gene = gener2, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r2 <- cbind(as.data.frame(egor2),rep("R2",nrow(egor2)))
colnames(r2)[10] = "SampleID"

r3 <- read.table("R3.txt", header=T, sep="\t")
gener3 <- as.character(r3[,1])
egor3 <- enrichGO(gene = gener3, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r3 <- cbind(as.data.frame(egor3),rep("R3",nrow(egor3)))
colnames(r3)[10] = "SampleID"

r4 <- read.table("R4.txt", header=T, sep="\t")
gener4 <- as.character(r4[,1])
egor4 <- enrichGO(gene = gener4, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r4 <- cbind(as.data.frame(egor4),rep("R4",nrow(egor4)))
colnames(r4)[10] = "SampleID"

r5 <- read.table("R5.txt", header=T, sep="\t")
gener5 <- as.character(r5[,1])
egor5 <- enrichGO(gene = gener5, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r5 <- cbind(as.data.frame(egor5),rep("R5",nrow(egor5)))
colnames(r5)[10] = "SampleID"


r6 <- read.table("R6.txt", header=T, sep="\t")
gener6 <- as.character(r6[,1])
egor6 <- enrichGO(gene = gener6, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r6 <- cbind(as.data.frame(egor6),rep("R6",nrow(egor6)))
colnames(r6)[10] = "SampleID"


r7 <- read.table("R7.txt", header=T, sep="\t")
gener7 <- as.character(r7[,1])
egor7 <- enrichGO(gene = gener7, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r7 <- cbind(as.data.frame(egor7),rep("R7",nrow(egor7)))
colnames(r7)[10] = "SampleID"

r8 <- read.table("R8.txt", header=T, sep="\t")
gener8 <- as.character(r8[,1])
egor8 <- enrichGO(gene = gener8, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r8 <- cbind(as.data.frame(egor8),rep("R8",nrow(egor8)))
colnames(r8)[10] = "SampleID"

r9 <- read.table("R9.txt", header=T, sep="\t")
gener9 <- as.character(r9[,1])
egor9 <- enrichGO(gene = gener9, OrgDb = org.At.tair.db, keyType = 'TAIR', ont="BP", pvalueCutoff  = 0.05, pAdjustMethod = "BH")
r9 <- cbind(as.data.frame(egor9),rep("R9",nrow(egor9)))
colnames(r9)[10] = "SampleID"

R1<-r1[1:12,]
R2<-r2[1:12,]
R3<-r3[1:12,]
R6<-r6[1:12,]
R7<-r7[1:12,]
R9<-r9[1:12,]

ego <- rbind(R1,R2,R3,r4,r5,R6,R7,r8,R9)

p <- ggplot(ego, aes(x = SampleID, y = Description)) + 
  geom_point(aes(size = Count, color = p.adjust)) +
  scale_colour_gradient(limits=c(0, 0.01), low="red")+
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  scale_y_discrete(limits=c("alkene biosynthetic process",
                            "benzene-containing compound metabolic process",
                            "cation homeostasis",
                            "cellular alkene metabolic process",
                            "cellular cation homeostasis",
                            "cellular chemical homeostasis",
                            "cellular homeostasis",
                            "cellular ion homeostasis",
                            "cellular metal ion homeostasis",
                            "cellular response to inorganic substance",
                            "cellular response to iron ion",
                            "cellular response to metal ion",
                            "cellular response to nitric oxide",
                            "cellular response to nitrogen compound",
                            "cellular response to organic cyclic compound",
                            "cellular response to reactive nitrogen species",
                            "sulfate assimilation",
                            "sulfur compound biosynthetic process",
                            "S-glycoside biosynthetic process",
                            "S-glycoside metabolic process",
                            "glucosinolate biosynthetic process",
                            "glucosinolate metabolic process",
                            "glycosinolate biosynthetic process",
                            "glycosinolate metabolic process",
                            "glycosyl compound biosynthetic process",
                            "glycosyl compound metabolic process",
                            "olefin biosynthetic process",
                            "olefin metabolic process",
                            "secondary metabolite biosynthetic process",
                            "protein folding",
                            "intracellular signal transduction",
                            "inorganic anion transport",
                            "inorganic ion homeostasis",
                            "response to iron ion",
                            "response to nitric oxide",
                            "ion homeostasis",
                            "chemical homeostasis",
                            " ",
                            "root epidermal cell differentiation",
                            "plant epidermal cell differentiation",
                            "trichoblast differentiation",
                            " ",
                            "response to carbohydrate",
                            "response to fructose",
                            "response to hexose",
                            "response to sucrose",
                            "response to disaccharide",
                            "response to monosaccharide",
                            "photosynthesis, light reaction",
                            "photosystem II assembly",
                            "response to light intensity",
                            "response to high light intensity",
                            "heat acclimation",
                            "response to heat",
                            " ",
                            "cellular response to hypoxia",
                            "cellular response to decreased oxygen levels",
                            "cellular response to oxygen levels",
                            "cellular response to reactive oxygen species",
                            "regulation of hydrogen peroxide metabolic process",
                            "response to decreased oxygen levels",
                            "response to hydrogen peroxide",
                            "response to hypoxia",
                            "response to oxidative stress",
                            "response to oxygen levels",
                            "response to reactive oxygen species",
                            " ",
                            "ethylene biosynthetic process",
                            "ethylene metabolic process",
                            "response to ethylene",
                            "cellular response to salicylic acid stimulus",
                            "salicylic acid biosynthetic process",
                            "salicylic acid mediated signaling pathway",
                            "salicylic acid metabolic process",
                            "response to salicylic acid",
                            "response to jasmonic acid",
                            " ",
                            "cellular response to antibiotic",
                            "cellular response to drug",
                            "antibiotic biosynthetic process",
                            "response to endoplasmic reticulum stress",
                            "respiratory burst",
                            "respiratory burst involved in defense response",
                            "response to bronchodilator",
                            "response to organonitrogen compound",
                            "response to toxic substance",
                            "response to water",
                            "response to water deprivation",
                            "response to wounding",
                            "response to mechanical stimulus",
                            "response to chitin",
                            "defense response to fungus",
                            "defense response to insect",
                            "systemic acquired resistance, salicylic acid mediated signaling pathway",
                            "systemic acquired resistance"
    
  ))+
  labs(title="Root_GO_top12")+
  theme(axis.text.x=element_text(size=6))+
  theme(axis.text.y = element_text(size=5))+
  theme(axis.title.y = element_text(size=5))+
  theme(axis.title.x=element_text(size=5))+
  theme(plot.title=element_text(size=5))
p

pdf("Extended_Data_Figure_5c.pdf",width = 5.5,height = 10,useDingbats = F)
p
dev.off() 


