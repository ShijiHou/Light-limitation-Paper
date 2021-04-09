library("ggplot2")

myc234 <- read.table("myc234_BC.txt", sep="\t",header=T)
bc <- read.table("mutants8_bc.txt", sep="\t",header=T)
cs <- read.table("mutants8_canopy_size.txt", sep="\t",header=T)



cs <- read.table("cs_myc2_myc234.txt", sep="\t",header=T)

cs$lab1 <- factor(cs$lab1,levels = levels(cs$lab1)[c(5,7,1,3,6,8,2,4)],ordered=T)

p1 <- ggplot(cs,aes(x=lab1, y=canopysize, group=lab1, fill=lab1))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab1))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  scale_x_discrete(breaks=c("NC_myc2","NC+M_myc2","LP_myc2","LP+M_myc2","NC_myc234","NC+M_myc234", "LP_myc234", "LP+M_myc234"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 5.5, by=1),limits=c(0, 5.5))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+
  theme(legend.position = "none")

pdf("cs_myc2_myc234.pdf", width=4, height=4,useDingbats = F)
p1
dev.off()

bc <- read.table("bc_myc2_myc234.txt", sep="\t",header=T)

bc$lab1 <- factor(bc$lab1,levels = levels(bc$lab1)[c(3,4,1,2,7,8,5,6)],ordered=T)

p2 <- ggplot(bc,aes(x=lab1, y=y, group=lab1, fill=lab1))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab1))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("lg(BcCutA/AtSKII)")+
  scale_x_discrete(breaks=c("myc2_NC","myc2_NC_M","myc2_LP","myc2_LP_M","myc234_NC","myc234_NC_M","myc234_LP","myc234_LP_M"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(-3, 0.5, by=0.5),limits=c(-3.2, 0.6))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+
  theme(legend.position = "none")

pdf("bc_myc2_myc234.pdf", width=4, height=4,useDingbats = F)
p2
dev.off()

library(ggpubr)

p <- ggarrange(p1, p2,
                ncol=1, nrow = 2)


pdf("Figure_S4_O.pdf", width=3.5, height=5,useDingbats = F)
p
dev.off()



#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(cs$lab1)){
  x <- subset(cs, lab1 == g, sel = c("canopysize"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

library(dunn.test)
kruskal.test(cs[["canopysize"]],cs[["lab1"]])
dt<-dunn.test(cs[["canopysize"]],cs[["lab1"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#       Group     Letter    MonoLetter
# 1     LP_myc2      b          b
# 2   LP_myc234      b          b
# 3   LP+M_myc2      b          b
# 4 LP+M_myc234      b          b
# 5     NC_myc2      a         a 
# 6   NC_myc234      a         a 
# 7   NC+M_myc2      a         a 
# 8 NC+M_myc234      a         a 

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(bc$lab1)){
  x <- subset(bc, lab == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

result <- aov(bc[["y"]] ~ bc[["lab1"]])
summary(result)
TukeyHSD(result)
### compact letter display
library(multcomp)
result <- aov(y ~ lab1, bc)
dt <- glht(result, linfct=mcp(lab1="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
# myc2_NC   myc2_NC_M     myc2_LP   myc2_LP_M   myc234_NC myc234_NC_M   myc234_LP myc234_LP_M 
# "c"         "d"         "a"         "d"         "b"         "d"         "a"         "d"

