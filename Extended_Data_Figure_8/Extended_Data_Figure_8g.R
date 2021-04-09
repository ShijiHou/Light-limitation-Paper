library(ggplot2)
library(stringr)
library(clusterProfiler)
library(org.At.tair.db)
library(forcats)
library(GOSemSim)
library(DOSE)

###### Root #########################################################
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


R1<-r1[1:12,]
R2<-r2[1:12,]
R3<-r3[1:12,]
R4<-r4[1:12,]
R5<-r5[1:12,]
R6<-r6[1:12,]
R7<-r7[1:12,]
R8<-r8[1:12,]

tab1 <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
write.table(tab1, "ROOT_all_GO.txt", sep="\t", quote=FALSE)

ego <- rbind(R1,R2,R3,R4,R5,R6,R7,R8)

ego1 <- ego[match(unique(ego$Description),ego$Description),]

p <- ggplot(ego, aes(x = SampleID, y = Description)) + 
  geom_point(aes(size = Count, color = p.adjust)) +
  scale_colour_gradient(limits=c(0, 0.01), low="red")+
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  scale_y_discrete(limits=c(                                                      
    "isopentenyl diphosphate biosynthetic process, methylerythritol 4-phosphate pathway",
    "isopentenyl diphosphate biosynthetic process",                                      
    "isopentenyl diphosphate metabolic process",                                         
    "glyceraldehyde-3-phosphate metabolic process",
    "plastid transcription",                                                             
    "isoprenoid metabolic process",                                                      
    "isoprenoid biosynthetic process",                                                   
    "S-glycoside biosynthetic process",                                                 
    "glycosinolate biosynthetic process",                                                
    "glucosinolate biosynthetic process",                                                
    "glycosyl compound biosynthetic process",                                            
    "S-glycoside metabolic process",                                                     
    "glycosinolate metabolic process",                                                   
    "glucosinolate metabolic process" ,                                                  
    "glycosyl compound metabolic process",                                              
    "secondary metabolite biosynthetic process",                                        
    "alpha-amino acid biosynthetic process",                                             
    "cellular amino acid biosynthetic process" ,                                         
    "sulfur compound biosynthetic process",                                              
    "flavonoid metabolic process",                                                       
    "flavonoid biosynthetic process",                                                         
    "benzene-containing compound metabolic process",                                    
    "phenol-containing compound biosynthetic process",                                   
    "phenol-containing compound metabolic process" ,                                     
    "histone H3-K9 methylation"   ,                                                      
    "histone H3-K9 modification" ,                                                       
    "histone modification",                                                              
    "histone lysine methylation",                                                        
    "peptidyl-lysine methylation",
    "cysteine biosynthetic process",                                                     
    "cysteine metabolic process",                                                        
    "serine family amino acid biosynthetic process",                                     
    "antibiotic metabolic process",
    "cellular response to lipid",                  
    " ",
    "inorganic ion homeostasis",                                                         
    "transition metal ion homeostasis" ,                                                 
    "ion homeostasis" ,                                                                  
    "cation homeostasis" ,                                                               
    "chemical homeostasis" ,                                                             
    "cellular iron ion homeostasis" ,                                                    
    "iron ion homeostasis",                                                              
    "cellular chemical homeostasis",
    "iron ion transport" ,
    "cellular response to nutrient levels",                                              
    "cellular response to extracellular stimulus",
    "phosphorelay signal transduction system",
    "cellular metal ion homeostasis",                                                    
    "cellular transition metal ion homeostasis",                                         
    "metal ion homeostasis",                                                             
    "phenylpropanoid biosynthetic process",                                              
    "inorganic anion transport",                                                         
    "cellular cation homeostasis",                                                       
    "cellular ion homeostasis",    
    " ",
    "mitotic cell cycle process",                                                        
    "mitotic cytokinesis",                                                               
    "cytokinesis by cell plate formation",                                               
    "cytokinetic process",                                                               
    "mitotic cytokinetic process",                                                       
    "cytokinesis",                                                                       
    "cytoskeleton-dependent cytokinesis",
    "xylem development",
    "organ growth",
    " ",
    "response to sucrose",                                                               
    "response to disaccharide",
    "photosynthesis",                                                                    
    "photosynthesis, light reaction", 
    "response to UV" ,
    "response to far red light",                                                         
    "response to red light",                                                             
    "response to blue light",                                                            
    "circadian rhythm",                                                                  
    "rhythmic process",
    " ",
    "cellular response to ethylene stimulus",
    "abscisic acid-activated signaling pathway",                                        
    "cellular response to abscisic acid stimulus",                                       
    "cellular response to alcohol",                                                      
    "ethylene-activated signaling pathway",        
    "response to ethylene",
    "salicylic acid biosynthetic process", 
    "salicylic acid metabolic process", 
    "response to salicylic acid",  
    " ",
    "response to organonitrogen compound",
    "response to toxic substance",                                                       
    "defense response to fungus",
    "response to wounding",
    "response to water deprivation",                                                     
    "response to water",
    "response to chitin",
    "systemic acquired resistance" 
    
  ))+
  labs(title="Root_GO_top12")+
  theme(axis.text.x=element_text(size=8))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(plot.title=element_text(size=12))
p

pdf("Extended_Data_Figure_8g_Root.pdf",width = 10,height = 15,useDingbats = F)
p
dev.off() 


###### Shoot #########################################################
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

tab1 <- rbind(s1,s2,s3,s4,s5,s6,s7,s8)
write.table(tab1, "SHOOT_all_GO.txt", sep="\t", quote=FALSE)


S2 <- s2[1:12,]
S3 <- s3[1:12,]
S4 <- s4[1:12,]
S5 <- s5[1:12,]
S6 <- s6[1:12,]
S7 <- s7[1:12,]


ego <- rbind(s1,S2,S3,S4,S5,S6,S7,s8)

##plot microbes vs nonmicrobes

p <- ggplot(ego, aes(x = SampleID, y = Description)) + 
  geom_point(aes(size = Count, color = p.adjust)) +
  scale_colour_gradient(limits=c(0, 0.01), low="red")+
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  scale_y_discrete(limits=c( "amino acid transport" , 
                             "organic acid transport"  ,                              
                             "carboxylic acid transport"  ,                          
                             "organic anion transport",                               
                             "cellular response to antibiotic",
                             "flavonoid metabolic process"  ,                        
                             "flavonoid biosynthetic process",                        
                             "secondary metabolite biosynthetic process",            
                             "polyol biosynthetic process",                           
                             "S-glycoside biosynthetic process",                     
                             "glycosinolate biosynthetic process",                    
                             "glucosinolate biosynthetic process" ,                  
                             "myo-inositol hexakisphosphate biosynthetic process" ,   
                             "myo-inositol hexakisphosphate metabolic process"  ,    
                             "inositol phosphate biosynthetic process" ,              
                             "glycosyl compound biosynthetic process" ,              
                             "positive regulation of flavonoid biosynthetic process" ,
                             " ",
                             "DNA-templated transcription, elongation",
                             "cytokinesis by cell plate formation" ,                 
                             "cytokinetic process"  ,                                 
                             "mitotic cytokinetic process",                       
                             "mitotic cytokinesis",                                   
                             "cytokinesis" ,                                         
                             "cytoskeleton-dependent cytokinesis" ,                   
                             "mitotic cell cycle process",                           
                             "cell proliferation" ,                                   
                             "cell division",
                             "microtubule-based process" ,                            
                             "microtubule cytoskeleton organization" ,               
                             "mitotic cell cycle",
                             "response to auxin",
                             " ",
                             "oligosaccharide metabolic process",
                             "cellular response to organic cyclic compound",         
                             "polysaccharide catabolic process",                      
                             "terpenoid metabolic process",                          
                             "protein folding",                                       
                             "isoprenoid metabolic process",                         
                             "maltose metabolic process" ,                           
                             "glyceraldehyde-3-phosphate metabolic process" ,         
                             "pentose-phosphate shunt" ,                             
                             "glucose 6-phosphate metabolic process",                 
                             "disaccharide metabolic process",                       
                             "NADP metabolic process" ,                               
                             "cellular aldehyde metabolic process" ,                 
                             "plastid organization" ,                  
                             "glucan biosynthetic process" ,
                             " ",
                             "starch biosynthetic process",
                             "starch catabolic process",                              
                             "glucan catabolic process",                             
                             "cellular polysaccharide catabolic process",             
                             "starch metabolic process",                             
                             "carbohydrate catabolic process" ,                       
                             "cellular carbohydrate catabolic process" ,                                        
                             "photosynthesis, light reaction" ,                      
                             "photosynthesis, light harvesting",                      
                             "photosynthesis" ,
                             " ",
                             "cellular response to lipid",                           
                             " ",
                             "response to brassinosteroid",
                             "jasmonic acid metabolic process",                      
                             "abscisic acid-activated signaling pathway" ,           
                             "cellular response to abscisic acid stimulus" ,          
                             "cellular response to alcohol",                         
                             "jasmonic acid biosynthetic process" ,        
                             "response to ethylene" ,                                 
                             "response to jasmonic acid" , 
                             "response to salicylic acid" ,
                             " ",
                             "response to wounding",
                             "cellular response to drug" ,   
                             "respiratory burst involved in defense response",        
                             "respiratory burst",
                             "response to organonitrogen compound" ,                 
                             "defense response to fungus",
                             "response to chitin",
                             "systemic acquired resistance"
                             
  ))+
  labs(title="Shoot_GO_top12")+
  theme(axis.text.x=element_text(size=8))+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(plot.title=element_text(size=12))

p

pdf("Extended_Data_Figure_8g_Shoot.pdf",width = 8,height = 14,useDingbats = F)
p
dev.off()  

