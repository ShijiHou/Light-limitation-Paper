library("ggplot2")

cs <- read.table("mutants8_canopy_size.txt", sep="\t",header=T)
cs$geno <- factor(cs$geno,levels = levels(cs$geno)[c(2,9,5,4,7,6,1,3,8)],ordered = T)
cs$lab <- factor(cs$lab,levels = levels(cs$lab)[c(3,4,1,2)],ordered = T)

p1 <- ggplot(cs,aes(x=lab, y=canopysize, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  scale_x_discrete(breaks=c("NC","NC+M","LP","LP+M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 6, by=1),limits=c(0, 6))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

sfw <- read.table("mutants8_sfw.txt", sep="\t",header=T)
sfw$geno <- factor(sfw$geno, levels = levels(sfw$geno)[c(2,9,5,4,7,6,1,3,8)], ordered = T)

p2 <- ggplot(sfw,aes(x=lab, y=shootfreshweight, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Shoot fresh weight (g)")+
  scale_x_discrete(breaks=c("NC","NC_M","SC","SC_M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 0.15, by=0.05),limits=c(0, 0.15))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

pl <- read.table("mutants8_petiole_length.txt", sep="\t",header=T)
pl$geno <- factor(pl$geno, levels = levels(pl$geno)[c(2,9,5,4,7,6,1,3,8)], ordered = T)
pl$condition <- factor(pl$condition, levels = levels(pl$condition)[c(3,4,1,2)], ordered = T)

p3 <- ggplot(pl,aes(x=condition, y=petiole_length, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA, aes(fill=condition))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Petiole length (mean/plant - cm)")+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 1, by=0.2),limits=c(0, 1.2))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

ln <- read.table("mutants8_leaf_number.txt", sep="\t",header=T)
ln$geno <- factor(ln$geno, levels = levels(ln$geno)[c(2,9,5,4,7,6,1,3,8)], ordered = T)
ln$condition <- factor(ln$condition, levels = levels(ln$condition)[c(3,4,1,2)], ordered = T)

p4 <- ggplot(ln,aes(x=condition, y=total.number.of.leaves, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA, aes(fill=condition))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Leaf numbers/plant")+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(4, 18, by=5),limits=c(4, 18))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

ls <- read.table("mutants8_leaf_shape.txt", sep="\t",header=T)
ls$geno <- factor(ls$geno, levels = levels(ls$geno)[c(2,9,5,4,7,6,1,3,8)], ordered = T)
ls$condition <- factor(ls$condition, levels = levels(ls$condition)[c(3,4,1,2)], ordered = T)

p5 <- ggplot(ls,aes(x=condition, y=y, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA, aes(fill=condition))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Leaf length/width ratio")+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0, 4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

bc <- read.table("mutants8_bc.txt", sep="\t",header=T)
bc$geno <- factor(bc$geno,levels = levels(bc$geno)[c(2,9,5,4,7,6,1,3,8)],ordered = T)
bc$lab <- factor(bc$lab,levels = levels(bc$lab)[c(3,4,1,2)],ordered = T)

p6 <- ggplot(bc,aes(x=lab, y=y, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("lg(BcCutA/AtSKII)")+
  scale_x_discrete(breaks=c("NC","NC+M","LP","LP+M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(-4, 2, by=1),limits=c(-4, 2))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

pst <- read.table("mutants8_pst.txt", sep="\t",header=T)
pst$geno <- factor(pst$geno,levels = levels(pst$geno)[c(2,9,5,4,7,6,1,3,8)],ordered = T)
pst$lab <- factor(pst$lab,levels = levels(pst$lab)[c(3,4,1,2)],ordered = T)

p7 <- ggplot(pst,aes(x=lab, y=y, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=0.5)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("lg(CFU/g)")+
  scale_x_discrete(breaks=c("NC","NC+M","LP","LP+M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(5, 8, by=1),limits=c(5, 8.5))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,angle=0,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

library(ggpubr)
p <- ggarrange(p1,p2, p3, p4, p5,p6,p7,
               ncol=1, nrow = 7)

pdf("mutants8_all_parameters.pdf", width=8, height=14,useDingbats = F)
p
dev.off()

###################statistics#############################################
####### canopy size ######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(cs$lab1)){
  x <- subset(cs, lab1 == g, sel = c("canopysize"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

library(dunn.test)
library(rcompanion)

col0 <- subset(cs, geno == "col0")
kruskal.test(col0[["canopysize"]],col0[["lab"]])
dt<-dunn.test(col0[["canopysize"]],col0[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sid2 <- subset(cs, geno == "sid2")
kruskal.test(sid2[["canopysize"]],sid2[["lab"]])
dt<-dunn.test(sid2[["canopysize"]],sid2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

della <- subset(cs, geno == "della")
kruskal.test(della[["canopysize"]],della[["lab"]])
dt<-dunn.test(della[["canopysize"]],della[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

dde2 <- subset(cs, geno == "dde2")
kruskal.test(dde2[["canopysize"]],dde2[["lab"]])
dt<-dunn.test(dde2[["canopysize"]],dde2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

myc2 <- subset(cs, geno == "myc2")
kruskal.test(myc2[["canopysize"]],myc2[["lab"]])
dt<-dunn.test(myc2[["canopysize"]],myc2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

jazq <- subset(cs, geno == "jazq")
kruskal.test(jazq[["canopysize"]],jazq[["lab"]])
dt<-dunn.test(jazq[["canopysize"]],jazq[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

bri1 <- subset(cs, geno == "bri1")
kruskal.test(bri1[["canopysize"]],bri1[["lab"]])
dt<-dunn.test(bri1[["canopysize"]],bri1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

cry1cry2 <- subset(cs, geno == "cry1cry2")
kruskal.test(cry1cry2[["canopysize"]],cry1cry2[["lab"]])
dt<-dunn.test(cry1cry2[["canopysize"]],cry1cry2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sav <- subset(cs, geno == "sav")
kruskal.test(sav[["canopysize"]],sav[["lab"]])
dt<-dunn.test(sav[["canopysize"]],sav[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)


#######sfw######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(sfw$lab1)){
  x <- subset(sfw, lab1 == g, sel = c("shootfreshweight"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
library(dunn.test)

col0 <- subset(sfw, geno == "col0")
kruskal.test(col0[["shootfreshweight"]],col0[["lab"]])
dt<-dunn.test(col0[["shootfreshweight"]],col0[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

sid2 <- subset(sfw, geno == "sid2")
kruskal.test(sid2[["shootfreshweight"]],sid2[["lab"]])
dt<-dunn.test(sid2[["shootfreshweight"]],sid2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

della <- subset(sfw, geno == "della")
kruskal.test(della[["shootfreshweight"]],della[["lab"]])
dt<-dunn.test(della[["shootfreshweight"]],della[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

dde2 <- subset(sfw, geno == "dde2")
kruskal.test(dde2[["shootfreshweight"]],dde2[["lab"]])
dt<-dunn.test(dde2[["shootfreshweight"]],dde2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

myc2 <- subset(sfw, geno == "myc2")
kruskal.test(myc2[["shootfreshweight"]],myc2[["lab"]])
dt<-dunn.test(myc2[["shootfreshweight"]],myc2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

jazq <- subset(sfw, geno == "jazq")
kruskal.test(jazq[["shootfreshweight"]],jazq[["lab"]])
dt<-dunn.test(jazq[["shootfreshweight"]],jazq[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

bri1 <- subset(sfw, geno == "bri1")
kruskal.test(bri1[["shootfreshweight"]],bri1[["lab"]])
dt<-dunn.test(bri1[["shootfreshweight"]],bri1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

cry1cry2 <- subset(sfw, geno == "cry1cry2")
kruskal.test(cry1cry2[["shootfreshweight"]],cry1cry2[["lab"]])
dt<-dunn.test(cry1cry2[["shootfreshweight"]],cry1cry2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

sav <- subset(sfw, geno == "sav")
kruskal.test(sav[["shootfreshweight"]],sav[["lab"]])
dt<-dunn.test(sav[["shootfreshweight"]],sav[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

#######pl######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(pl$geno)){
  x <- subset(pl, geno == g, sel = c("petiole_length"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
library(dunn.test)

col0 <- subset(pl, geno == "col0")
library(multcomp)
result <- aov(petiole_length ~ lab, col0)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sid2 <- subset(pl, geno == "sid2")
library(multcomp)
result <- aov(petiole_length ~ lab, sid2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

della <- subset(pl, geno == "dellap")
library(multcomp)
result <- aov(petiole_length ~ lab, della)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

dde2 <- subset(pl, geno == "dde2")
library(multcomp)
result <- aov(petiole_length ~ lab, dde2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

myc2 <- subset(pl, geno == "myc2")
library(multcomp)
result <- aov(petiole_length ~ lab, myc2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

jazq <- subset(pl, geno == "jazq")
library(multcomp)
result <- aov(petiole_length ~ lab, jazq)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

bri1 <- subset(pl, geno == "bri1")
library(multcomp)
result <- aov(petiole_length ~ lab, bri1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

cry1cry2 <- subset(pl, geno == "cry1cry2")
library(multcomp)
result <- aov(petiole_length ~ lab, cry1cry2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sav <- subset(pl, geno == "sav")
library(multcomp)
result <- aov(petiole_length ~ lab, sav)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)


###### leaf number ######

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(ln$lab)){
  x <- subset(ln, lab == g, sel = c("total.number.of.leaves"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
library(dunn.test)
col0 <- subset(ln, geno == "col0")
kruskal.test(col0[["total.number.of.leaves"]],col0[["lab"]])
dt<-dunn.test(col0[["total.number.of.leaves"]],col0[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sid2 <- subset(ln, geno == "sid2")
kruskal.test(sid2[["total.number.of.leaves"]],sid2[["lab"]])
dt<-dunn.test(sid2[["total.number.of.leaves"]],sid2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

della <- subset(ln, geno == "dellap")
kruskal.test(della[["total.number.of.leaves"]],della[["condition"]])
dt<-dunn.test(della[["total.number.of.leaves"]],della[["condition"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

dde2 <- subset(ln, geno == "dde2")
kruskal.test(dde2[["total.number.of.leaves"]],dde2[["lab"]])
dt<-dunn.test(dde2[["total.number.of.leaves"]],dde2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

myc2 <- subset(ln, geno == "myc2")
kruskal.test(myc2[["total.number.of.leaves"]],myc2[["lab"]])
dt<-dunn.test(myc2[["total.number.of.leaves"]],myc2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

jazq <- subset(ln, geno == "jazq")
kruskal.test(jazq[["total.number.of.leaves"]],jazq[["lab"]])
dt<-dunn.test(jazq[["total.number.of.leaves"]],jazq[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

bri1 <- subset(ln, geno == "bri1")
kruskal.test(bri1[["total.number.of.leaves"]],bri1[["lab"]])
dt<-dunn.test(bri1[["total.number.of.leaves"]],bri1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

cry1cry2 <- subset(ln, geno == "cry1cry2")
kruskal.test(cry1cry2[["total.number.of.leaves"]],cry1cry2[["condition"]])
dt<-dunn.test(cry1cry2[["total.number.of.leaves"]],cry1cry2[["condition"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sav <- subset(ln, geno == "sav")
kruskal.test(sav[["total.number.of.leaves"]],sav[["lab"]])
dt<-dunn.test(sav[["total.number.of.leaves"]],sav[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)


#######ls######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(ls$lab)){
  x <- subset(ls, lab == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
library(dunn.test)

col0 <- subset(ls, geno == "col0")
kruskal.test(col0[["y"]],col0[["lab"]])
dt<-dunn.test(col0[["y"]],col0[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sid2 <- subset(ls, geno == "sid2")
kruskal.test(sid2[["y"]],sid2[["lab"]])
dt<-dunn.test(sid2[["y"]],sid2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

della <- subset(ls, geno == "dellap")
kruskal.test(della[["y"]],della[["condition"]])
dt<-dunn.test(della[["y"]],della[["condition"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

dde2 <- subset(ls, geno == "dde2")
kruskal.test(dde2[["y"]],dde2[["lab"]])
dt<-dunn.test(dde2[["y"]],dde2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

myc2 <- subset(ls, geno == "myc2")
kruskal.test(myc2[["y"]],myc2[["lab"]])
dt<-dunn.test(myc2[["y"]],myc2[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

jazq <- subset(ls, geno == "jazq")
kruskal.test(jazq[["y"]],jazq[["lab"]])
dt<-dunn.test(jazq[["y"]],jazq[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

bri1 <- subset(ls, geno == "bri1")
kruskal.test(bri1[["y"]],bri1[["lab"]])
dt<-dunn.test(bri1[["y"]],bri1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

cry1cry2 <- subset(ls, geno == "cry1cry2")
kruskal.test(cry1cry2[["y"]],cry1cry2[["condition"]])
dt<-dunn.test(cry1cry2[["y"]],cry1cry2[["condition"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

sav <- subset(ls, geno == "sav")
kruskal.test(sav[["y"]],sav[["lab"]])
dt<-dunn.test(sav[["y"]],sav[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

###################statistics#############################################
####### bc ######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(bc$Condition)){
  x <- subset(bc, Condition == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

library(dunn.test)
library(rcompanion)

### compact letter display
library(multcomp)
col0 <- subset(bc, geno == "col0")
result <- aov(y ~ lab, col0)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sid2 <- subset(bc, geno == "sid2")
result <- aov(y ~ lab, sid2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

della <- subset(bc, geno == "della")
result <- aov(y ~ lab, della)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

dde2 <- subset(bc, geno == "dde2")
result <- aov(y ~ lab, dde2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

myc2 <- subset(bc, geno == "myc2")
result <- aov(y ~ lab, myc2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

jazq <- subset(bc, geno == "jazq")
result <- aov(y ~ lab, jazq)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

bri1 <- subset(bc, geno == "bri1")
result <- aov(y ~ lab, bri1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

cry1cry2 <- subset(bc, geno == "cry1cry2")
result <- aov(y ~ lab, cry1cry2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sav <- subset(bc, geno == "sav")
result <- aov(y ~ lab, sav)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)


####### pst ######
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(pst$condition)){
  x <- subset(pst, condition == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

library(dunn.test)
library(rcompanion)

### compact letter display
library(multcomp)
col0 <- subset(pst, geno == "col0")
result <- aov(y ~ lab, col0)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sid2 <- subset(pst, geno == "sid2")
result <- aov(y ~ lab, sid2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

della <- subset(pst, geno == "della")
result <- aov(y ~ lab, della)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

dde2 <- subset(pst, geno == "dde2")
result <- aov(y ~ lab, dde2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

myc2 <- subset(pst, geno == "myc2")
result <- aov(y ~ lab, myc2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

jazq <- subset(pst, geno == "jazq")
result <- aov(y ~ lab, jazq)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

bri1 <- subset(pst, geno == "bri1")
result <- aov(y ~ lab, bri1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

cry1cry2 <- subset(pst, geno == "cry1cry2")
result <- aov(y ~ lab, cry1cry2)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

sav <- subset(pst, geno == "sav")
result <- aov(y ~ lab, sav)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)





