library("gplots")
library("ggplot2")

cs <- read.table("CS_mutants.txt", sep="\t",header=T)
bc <- read.table("BC_mutants.txt", sep="\t",header=T)
bc$y <- 10^bc$y

cs_col0 <- subset(cs, geno=="col0")

cs_col0_nc <- subset(cs_col0, lab=="NC")
cs_col0_ncm <- subset(cs_col0, lab=="NC+M")
cs_col0_lp <- subset(cs_col0, lab=="LP")
cs_col0_lpm <- subset(cs_col0, lab=="LP+M")

cs_sid2 <- subset(cs, geno=="sid2")

cs_sid2_nc <- subset(cs_sid2, lab=="NC")
cs_sid2_ncm <- subset(cs_sid2, lab=="NC+M")
cs_sid2_lp <- subset(cs_sid2, lab=="LP")
cs_sid2_lpm <- subset(cs_sid2, lab=="LP+M")

cs_dde2 <- subset(cs, geno=="dde2")

cs_dde2_nc <- subset(cs_dde2, lab=="NC")
cs_dde2_ncm <- subset(cs_dde2, lab=="NC+M")
cs_dde2_lp <- subset(cs_dde2, lab=="LP")
cs_dde2_lpm <- subset(cs_dde2, lab=="LP+M")

cs_myc2 <- subset(cs, geno=="myc2")

cs_myc2_nc <- subset(cs_myc2, lab=="NC")
cs_myc2_ncm <- subset(cs_myc2, lab=="NC+M")
cs_myc2_lp <- subset(cs_myc2, lab=="LP")
cs_myc2_lpm <- subset(cs_myc2, lab=="LP+M")

cs_jazq <- subset(cs, geno=="jazq")

cs_jazq_nc <- subset(cs_jazq, lab=="NC")
cs_jazq_ncm <- subset(cs_jazq, lab=="NC+M")
cs_jazq_lp <- subset(cs_jazq, lab=="LP")
cs_jazq_lpm <- subset(cs_jazq, lab=="LP+M")

cs_della <- subset(cs, geno=="della")

cs_della_nc <- subset(cs_della, lab=="NC")
cs_della_ncm <- subset(cs_della, lab=="NC+M")
cs_della_lp <- subset(cs_della, lab=="LP")
cs_della_lpm <- subset(cs_della, lab=="LP+M")

cs_bri1 <- subset(cs, geno=="bri1")

cs_bri1_nc <- subset(cs_bri1, lab=="NC")
cs_bri1_ncm <- subset(cs_bri1, lab=="NC+M")
cs_bri1_lp <- subset(cs_bri1, lab=="LP")
cs_bri1_lpm <- subset(cs_bri1, lab=="LP+M")

cs_cry1cry2 <- subset(cs, geno=="cry1cry2")

cs_cry1cry2_nc <- subset(cs_cry1cry2, lab=="NC")
cs_cry1cry2_ncm <- subset(cs_cry1cry2, lab=="NC+M")
cs_cry1cry2_lp <- subset(cs_cry1cry2, lab=="LP")
cs_cry1cry2_lpm <- subset(cs_cry1cry2, lab=="LP+M")

cs_sav <- subset(cs, geno=="sav")

cs_sav_nc <- subset(cs_sav, lab=="NC")
cs_sav_ncm <- subset(cs_sav, lab=="NC+M")
cs_sav_lp <- subset(cs_sav, lab=="LP")
cs_sav_lpm <- subset(cs_sav, lab=="LP+M")

col0_withoutM<- as.matrix(cs_col0_lp$canopysize/mean(cs_col0_nc$canopysize))
r1 <- cbind(as.data.frame(col0_withoutM),rep("col0-M",nrow(col0_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(col0_withoutM))
r1 <- cbind(r1,microbe)
col0_withM <- as.matrix(cs_col0_lpm$canopysize/mean(cs_col0_ncm$canopysize))
r2 <- cbind(as.data.frame(col0_withM),rep("col0+M",nrow(col0_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(col0_withM))
r2 <- cbind(r2,microbe)

col0 <- rbind(r1,r2)
geno <- rep("col0",nrow(col0))
col0 <- cbind(col0,geno)

sid2_withoutM<- as.matrix(cs_sid2_lp$canopysize/mean(cs_sid2_nc$canopysize))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(cs_sid2_lpm$canopysize/mean(cs_sid2_ncm$canopysize))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

sid2_withoutM<- as.matrix(cs_sid2_lp$canopysize/mean(cs_sid2_nc$canopysize))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(cs_sid2_lpm$canopysize/mean(cs_sid2_ncm$canopysize))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

dde2_withoutM<- as.matrix(cs_dde2_lp$canopysize/mean(cs_dde2_nc$canopysize))
r1 <- cbind(as.data.frame(dde2_withoutM),rep("dde2-M",nrow(dde2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(dde2_withoutM))
r1 <- cbind(r1,microbe)
dde2_withM <- as.matrix(cs_dde2_lpm$canopysize/mean(cs_dde2_ncm$canopysize))
r2 <- cbind(as.data.frame(dde2_withM),rep("dde2+M",nrow(dde2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(dde2_withM))
r2 <- cbind(r2,microbe)

dde2 <- rbind(r1,r2)
geno <- rep("dde2",nrow(dde2))
dde2 <- cbind(dde2,geno)

myc2_withoutM<- as.matrix(cs_myc2_lp$canopysize/mean(cs_myc2_nc$canopysize))
r1 <- cbind(as.data.frame(myc2_withoutM),rep("myc2-M",nrow(myc2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(myc2_withoutM))
r1 <- cbind(r1,microbe)
myc2_withM <- as.matrix(cs_myc2_lpm$canopysize/mean(cs_myc2_ncm$canopysize))
r2 <- cbind(as.data.frame(myc2_withM),rep("myc2+M",nrow(myc2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(myc2_withM))
r2 <- cbind(r2,microbe)

myc2 <- rbind(r1,r2)
geno <- rep("myc2",nrow(myc2))
myc2 <- cbind(myc2,geno)

jazq_withoutM<- as.matrix(cs_jazq_lp$canopysize/mean(cs_jazq_nc$canopysize))
r1 <- cbind(as.data.frame(jazq_withoutM),rep("jazq-M",nrow(jazq_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(jazq_withoutM))
r1 <- cbind(r1,microbe)
jazq_withM <- as.matrix(cs_jazq_lpm$canopysize/mean(cs_jazq_ncm$canopysize))
r2 <- cbind(as.data.frame(jazq_withM),rep("jazq+M",nrow(jazq_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(jazq_withM))
r2 <- cbind(r2,microbe)

jazq <- rbind(r1,r2)
geno <- rep("jazq",nrow(jazq))
jazq <- cbind(jazq,geno)

della_withoutM<- as.matrix(cs_della_lp$canopysize/mean(cs_della_nc$canopysize))
r1 <- cbind(as.data.frame(della_withoutM),rep("della-M",nrow(della_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(della_withoutM))
r1 <- cbind(r1,microbe)
della_withM <- as.matrix(cs_della_lpm$canopysize/mean(cs_della_ncm$canopysize))
r2 <- cbind(as.data.frame(della_withM),rep("della+M",nrow(della_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(della_withM))
r2 <- cbind(r2,microbe)

della <- rbind(r1,r2)
geno <- rep("della",nrow(della))
della <- cbind(della,geno)

bri1_withoutM<- as.matrix(cs_bri1_lp$canopysize/mean(cs_bri1_nc$canopysize))
r1 <- cbind(as.data.frame(bri1_withoutM),rep("bri1-M",nrow(bri1_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(bri1_withoutM))
r1 <- cbind(r1,microbe)
bri1_withM <- as.matrix(cs_bri1_lpm$canopysize/mean(cs_bri1_ncm$canopysize))
r2 <- cbind(as.data.frame(bri1_withM),rep("bri1+M",nrow(bri1_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(bri1_withM))
r2 <- cbind(r2,microbe)

bri1 <- rbind(r1,r2)
geno <- rep("bri1",nrow(bri1))
bri1 <- cbind(bri1,geno)

cry1cry2_withoutM<- as.matrix(cs_cry1cry2_lp$canopysize/mean(cs_cry1cry2_nc$canopysize))
r1 <- cbind(as.data.frame(cry1cry2_withoutM),rep("cry1cry2-M",nrow(cry1cry2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(cry1cry2_withoutM))
r1 <- cbind(r1,microbe)
cry1cry2_withM <- as.matrix(cs_cry1cry2_lpm$canopysize/mean(cs_cry1cry2_ncm$canopysize))
r2 <- cbind(as.data.frame(cry1cry2_withM),rep("cry1cry2+M",nrow(cry1cry2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(cry1cry2_withM))
r2 <- cbind(r2,microbe)

cry1cry2 <- rbind(r1,r2)
geno <- rep("cry1cry2",nrow(cry1cry2))
cry1cry2 <- cbind(cry1cry2,geno)

sav_withoutM<- as.matrix(cs_sav_lp$canopysize/mean(cs_sav_nc$canopysize))
r1 <- cbind(as.data.frame(sav_withoutM),rep("sav-M",nrow(sav_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sav_withoutM))
r1 <- cbind(r1,microbe)
sav_withM <- as.matrix(cs_sav_lpm$canopysize/mean(cs_sav_ncm$canopysize))
r2 <- cbind(as.data.frame(sav_withM),rep("sav+M",nrow(sav_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sav_withM))
r2 <- cbind(r2,microbe)

sav <- rbind(r1,r2)
geno <- rep("sav",nrow(sav))
sav <- cbind(sav,geno)

cs1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)

cs1$geno <- factor(cs1$geno, levels = levels(cs1$geno)[c(
  1,2,6,3,4,5,7,8,9
)], ordered = T)

cs_withouM <- subset(cs1, microbe=="-M")

p1 <- ggplot(cs_withouM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  geom_hline(yintercept=1, linetype="dashed",color="red",size=0.5)+
  scale_y_continuous(breaks=seq(0, 2, by=0.5),limits=c(0, 2.2))+
  xlab("")+ylab("Ratio of canopy Sizes (LP vs NC)")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p1

pdf("Figure_4a__withoutM.pdf", width=5, height=4,useDingbats = F)
p1
dev.off()


cs_withM <- subset(cs1, microbe=="+M")
p2 <- ggplot(cs_withM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  geom_hline(yintercept=1, linetype="dashed",color="red",size=0.5)+
  scale_y_continuous(breaks=seq(0, 2, by=0.5),limits=c(0, 2.2))+
  xlab("")+ylab("Ratio of canopy Sizes (LP vs NC)")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p2

pdf("Figure_4a_withM.pdf", width=5, height=4,useDingbats = F)
p2
dev.off()

##### canopy size statistics ##############################################

########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(cs$geno)){
  x <- subset(cs, geno == g, sel = c("canopysize"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 7.80911173073048e-05"
#[1] "p-value for dde2 : 7.89999221371281e-07"
#[1] "p-value for sid2 : 2.42876335363233e-08"
#[1] "p-value for della : 0.00815091648369761"
#[1] "p-value for jazq : 0.00912620557158613"
#[1] "p-value for cry1cry2 : 8.60325762400903e-08"
#[1] "p-value for bri1 : 2.211427331932e-06"
#[1] "p-value for sav : 9.15894129110374e-05"
#[1] "p-value for myc2 : 4.99126579761945e-06"


#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(cs_col0_lp$canopysize,cs_col0_nc$canopysize, paired = FALSE)
#W = 192, p-value < 2.2e-16
# ***

wilcox.test(cs_col0_lpm$canopysize,cs_col0_ncm$canopysize, paired = FALSE)
#W = 3030.5, p-value = 0.4136
# ns

wilcox.test(cs_sid2_lp$canopysize,cs_sid2_nc$canopysize, paired = FALSE)
#W = 21, p-value = 5.929e-14
# ***

wilcox.test(cs_sid2_lpm$canopysize,cs_sid2_ncm$canopysize, paired = FALSE)
#W = 424, p-value = 0.7062
# ns

wilcox.test(cs_della_lp$canopysize,cs_della_nc$canopysize, paired = FALSE)
#W = 45, p-value = 5.46e-10
# ***

wilcox.test(cs_della_lpm$canopysize,cs_della_ncm$canopysize, paired = FALSE)
#W = 207, p-value = 0.006605
# **

wilcox.test(cs_dde2_lp$canopysize,cs_dde2_nc$canopysize, paired = FALSE)
#W = 89, p-value = 5.868e-09
# ***

wilcox.test(cs_dde2_lpm$canopysize,cs_dde2_ncm$canopysize, paired = FALSE)
#W = 148, p-value = 2.67e-06
#***

wilcox.test(cs_myc2_lp$canopysize,cs_myc2_nc$canopysize, paired = FALSE)
#W = 87.5, p-value = 1.722e-06
# ***

wilcox.test(cs_myc2_lpm$canopysize,cs_myc2_ncm$canopysize, paired = FALSE)
#W = 39, p-value = 1.816e-10
# ***

wilcox.test(cs_jazq_lp$canopysize,cs_jazq_nc$canopysize, paired = FALSE)
#W = 112, p-value = 1.303e-05
# ***

wilcox.test(cs_jazq_lpm$canopysize,cs_jazq_ncm$canopysize, paired = FALSE)
#W = 113, p-value = 1.366e-05
# ***

wilcox.test(cs_bri1_lp$canopysize,cs_bri1_nc$canopysize, paired = FALSE)
#W = 71, p-value = 3.135e-08
# ***

wilcox.test(cs_bri1_lpm$canopysize,cs_bri1_ncm$canopysize, paired = FALSE)
#W = 50, p-value = 1.292e-09
# ***

wilcox.test(cs_cry1cry2_lp$canopysize,cs_cry1cry2_nc$canopysize, paired = FALSE)
#W = 13, p-value = 3.832e-13
# ***

wilcox.test(cs_cry1cry2_lpm$canopysize,cs_cry1cry2_ncm$canopysize, paired = FALSE)
#W = 3, p-value = 4.227e-10
# ***

wilcox.test(cs_sav_lp$canopysize,cs_sav_nc$canopysize, paired = FALSE)
#W = 258, p-value = 0.0664
# ns

wilcox.test(cs_sav_lpm$canopysize,cs_sav_ncm$canopysize, paired = FALSE)
#W = 286.5, p-value = 0.18
# ns

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(cs_withouM$geno)){
  x <- subset(cs_withouM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.00024949872128221"
#[1] "p-value for sid2 : 0.230906091587855"
#[1] "p-value for dde2 : 0.149322197252716"
#[1] "p-value for myc2 : 0.14076757795172"
#[1] "p-value for jazq : 0.0381073538606873"
#[1] "p-value for della : 0.00498815584315543"
#[1] "p-value for bri1 : 0.0276485761923154"
#[1] "p-value for cry1cry2 : 0.119734896133356"
#[1] "p-value for sav : 0.00919418144406263"

library(dunn.test)
kruskal.test(cs_withouM[["y"]],cs_withouM[["geno"]])
dt <- dunn.test(cs_withouM[["y"]],cs_withouM[["geno"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#1     bri1     ab         ab
#2      col     ab         ab
#3 cry1cry2      b          b
#4     dde2      a         a 
#5    della     ab         ab
#6     jazq      a         a 
#7     myc2      a         a 
#8      sav      a         a 
#9     sid2     ab         ab

### ab ab ab a a a ab b a

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(cs_withM$geno)){
  x <- subset(cs_withM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.0730820214919905"
#[1] "p-value for sid2 : 2.20774853519712e-05"
#[1] "p-value for dde2 : 0.21085757567218"
#[1] "p-value for myc2 : 0.00943708734323829"
#[1] "p-value for jazq : 0.00755627322266211"
#[1] "p-value for della : 0.145639888634761"
#[1] "p-value for bri1 : 0.107701484531279"
#[1] "p-value for cry1cry2 : 0.15060091791314"
#[1] "p-value for sav : 0.110974836052081"

library(dunn.test)
kruskal.test(cs_withM[["y"]],cs_withM[["geno"]])
dt <- dunn.test(cs_withM[["y"]],cs_withM[["geno"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#   Group Letter MonoLetter
#1     bri1     ef         ef
#2      col      d        d  
#3 cry1cry2      f          f
#4     dde2    bce      bc e 
#5    della    acd     a cd  
#6     jazq    bef      b  ef
#7     myc2    bef      b  ef
#8      sav    abc     abc   
#9     sid2     ad     a  d 

### d ad acd bce bef bef ef f abc

######## botrytis cinerea #######################################################

bc_col0 <- subset(bc, geno=="col0")

bc_col0_nc <- subset(bc_col0, lab=="NC")
bc_col0_ncm <- subset(bc_col0, lab=="NC+M")
bc_col0_lp <- subset(bc_col0, lab=="LP")
bc_col0_lpm <- subset(bc_col0, lab=="LP+M")

bc_sid2 <- subset(bc, geno=="sid2")

bc_sid2_nc <- subset(bc_sid2, lab=="NC")
bc_sid2_ncm <- subset(bc_sid2, lab=="NC+M")
bc_sid2_lp <- subset(bc_sid2, lab=="LP")
bc_sid2_lpm <- subset(bc_sid2, lab=="LP+M")

bc_dde2 <- subset(bc, geno=="dde2")

bc_dde2_nc <- subset(bc_dde2, lab=="NC")
bc_dde2_ncm <- subset(bc_dde2, lab=="NC+M")
bc_dde2_lp <- subset(bc_dde2, lab=="LP")
bc_dde2_lpm <- subset(bc_dde2, lab=="LP+M")

bc_myc2 <- subset(bc, geno=="myc2")

bc_myc2_nc <- subset(bc_myc2, lab=="NC")
bc_myc2_ncm <- subset(bc_myc2, lab=="NC+M")
bc_myc2_lp <- subset(bc_myc2, lab=="LP")
bc_myc2_lpm <- subset(bc_myc2, lab=="LP+M")

bc_jazq <- subset(bc, geno=="jazq")

bc_jazq_nc <- subset(bc_jazq, lab=="NC")
bc_jazq_ncm <- subset(bc_jazq, lab=="NC+M")
bc_jazq_lp <- subset(bc_jazq, lab=="LP")
bc_jazq_lpm <- subset(bc_jazq, lab=="LP+M")

bc_della <- subset(bc, geno=="della")

bc_della_nc <- subset(bc_della, lab=="NC")
bc_della_ncm <- subset(bc_della, lab=="NC+M")
bc_della_lp <- subset(bc_della, lab=="LP")
bc_della_lpm <- subset(bc_della, lab=="LP+M")

bc_bri1 <- subset(bc, geno=="bri1")

bc_bri1_nc <- subset(bc_bri1, lab=="NC")
bc_bri1_ncm <- subset(bc_bri1, lab=="NC+M")
bc_bri1_lp <- subset(bc_bri1, lab=="LP")
bc_bri1_lpm <- subset(bc_bri1, lab=="LP+M")

bc_cry1cry2 <- subset(bc, geno=="cry1cry2")

bc_cry1cry2_nc <- subset(bc_cry1cry2, lab=="NC")
bc_cry1cry2_ncm <- subset(bc_cry1cry2, lab=="NC+M")
bc_cry1cry2_lp <- subset(bc_cry1cry2, lab=="LP")
bc_cry1cry2_lpm <- subset(bc_cry1cry2, lab=="LP+M")

bc_sav <- subset(bc, geno=="sav")

bc_sav_nc <- subset(bc_sav, lab=="NC")
bc_sav_ncm <- subset(bc_sav, lab=="NC+M")
bc_sav_lp <- subset(bc_sav, lab=="LP")
bc_sav_lpm <- subset(bc_sav, lab=="LP+M")


col0_withoutM<- as.matrix(bc_col0_lp$y/mean(bc_col0_nc$y))
r1 <- cbind(as.data.frame(col0_withoutM),rep("col0-M",nrow(col0_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(col0_withoutM))
r1 <- cbind(r1,microbe)
col0_withM <- as.matrix(bc_col0_lpm$y/mean(bc_col0_ncm$y))
r2 <- cbind(as.data.frame(col0_withM),rep("col0+M",nrow(col0_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(col0_withM))
r2 <- cbind(r2,microbe)

col0 <- rbind(r1,r2)
geno <- rep("col0",nrow(col0))
col0 <- cbind(col0,geno)

sid2_withoutM<- as.matrix(bc_sid2_lp$y/mean(bc_sid2_nc$y))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(bc_sid2_lpm$y/mean(bc_sid2_ncm$y))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

sid2_withoutM<- as.matrix(bc_sid2_lp$y/mean(bc_sid2_nc$y))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(bc_sid2_lpm$y/mean(bc_sid2_ncm$y))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

dde2_withoutM<- as.matrix(bc_dde2_lp$y/mean(bc_dde2_nc$y))
r1 <- cbind(as.data.frame(dde2_withoutM),rep("dde2-M",nrow(dde2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(dde2_withoutM))
r1 <- cbind(r1,microbe)
dde2_withM <- as.matrix(bc_dde2_lpm$y/mean(bc_dde2_ncm$y))
r2 <- cbind(as.data.frame(dde2_withM),rep("dde2+M",nrow(dde2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(dde2_withM))
r2 <- cbind(r2,microbe)

dde2 <- rbind(r1,r2)
geno <- rep("dde2",nrow(dde2))
dde2 <- cbind(dde2,geno)

myc2_withoutM<- as.matrix(bc_myc2_lp$y/mean(bc_myc2_nc$y))
r1 <- cbind(as.data.frame(myc2_withoutM),rep("myc2-M",nrow(myc2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(myc2_withoutM))
r1 <- cbind(r1,microbe)
myc2_withM <- as.matrix(bc_myc2_lpm$y/mean(bc_myc2_ncm$y))
r2 <- cbind(as.data.frame(myc2_withM),rep("myc2+M",nrow(myc2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(myc2_withM))
r2 <- cbind(r2,microbe)

myc2 <- rbind(r1,r2)
geno <- rep("myc2",nrow(myc2))
myc2 <- cbind(myc2,geno)

jazq_withoutM<- as.matrix(bc_jazq_lp$y/mean(bc_jazq_nc$y))
r1 <- cbind(as.data.frame(jazq_withoutM),rep("jazq-M",nrow(jazq_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(jazq_withoutM))
r1 <- cbind(r1,microbe)
jazq_withM <- as.matrix(bc_jazq_lpm$y/mean(bc_jazq_ncm$y))
r2 <- cbind(as.data.frame(jazq_withM),rep("jazq+M",nrow(jazq_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(jazq_withM))
r2 <- cbind(r2,microbe)

jazq <- rbind(r1,r2)
geno <- rep("jazq",nrow(jazq))
jazq <- cbind(jazq,geno)

della_withoutM<- as.matrix(bc_della_lp$y/mean(bc_della_nc$y))
r1 <- cbind(as.data.frame(della_withoutM),rep("della-M",nrow(della_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(della_withoutM))
r1 <- cbind(r1,microbe)
della_withM <- as.matrix(bc_della_lpm$y/mean(bc_della_ncm$y))
r2 <- cbind(as.data.frame(della_withM),rep("della+M",nrow(della_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(della_withM))
r2 <- cbind(r2,microbe)

della <- rbind(r1,r2)
geno <- rep("della",nrow(della))
della <- cbind(della,geno)

bri1_withoutM<- as.matrix(bc_bri1_lp$y/mean(bc_bri1_nc$y))
r1 <- cbind(as.data.frame(bri1_withoutM),rep("bri1-M",nrow(bri1_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(bri1_withoutM))
r1 <- cbind(r1,microbe)
bri1_withM <- as.matrix(bc_bri1_lpm$y/mean(bc_bri1_ncm$y))
r2 <- cbind(as.data.frame(bri1_withM),rep("bri1+M",nrow(bri1_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(bri1_withM))
r2 <- cbind(r2,microbe)

bri1 <- rbind(r1,r2)
geno <- rep("bri1",nrow(bri1))
bri1 <- cbind(bri1,geno)

cry1cry2_withoutM<- as.matrix(bc_cry1cry2_lp$y/mean(bc_cry1cry2_nc$y))
r1 <- cbind(as.data.frame(cry1cry2_withoutM),rep("cry1cry2-M",nrow(cry1cry2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(cry1cry2_withoutM))
r1 <- cbind(r1,microbe)
cry1cry2_withM <- as.matrix(bc_cry1cry2_lpm$y/mean(bc_cry1cry2_ncm$y))
r2 <- cbind(as.data.frame(cry1cry2_withM),rep("cry1cry2+M",nrow(cry1cry2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(cry1cry2_withM))
r2 <- cbind(r2,microbe)

cry1cry2 <- rbind(r1,r2)
geno <- rep("cry1cry2",nrow(cry1cry2))
cry1cry2 <- cbind(cry1cry2,geno)

sav_withoutM<- as.matrix(bc_sav_lp$y/mean(bc_sav_nc$y))
r1 <- cbind(as.data.frame(sav_withoutM),rep("sav-M",nrow(sav_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sav_withoutM))
r1 <- cbind(r1,microbe)
sav_withM <- as.matrix(bc_sav_lpm$y/mean(bc_sav_ncm$y))
r2 <- cbind(as.data.frame(sav_withM),rep("sav+M",nrow(sav_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sav_withM))
r2 <- cbind(r2,microbe)

sav <- rbind(r1,r2)
geno <- rep("sav",nrow(sav))
sav <- cbind(sav,geno)

bc1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
bc1$y <- log(bc1$y,10)

bc1$geno <- factor(bc1$geno, levels = levels(bc1$geno)[c(
  1,2,6,3,4,5,7,8,9
)], ordered = T)

bc_withouM <- subset(bc1, microbe=="-M")

p1 <- ggplot(bc_withouM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  scale_y_continuous(breaks=seq(-1, 2, by=0.5),limits=c(-1.5, 2.5))+
  geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  xlab("")+ylab("lg(Ratio of BcCutA/AtSKII (LP vs NC))")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p1

pdf("Figure_4b_withoutM.pdf", width=5, height=4,useDingbats = F)
p1
dev.off()


bc_withM <- subset(bc1, microbe=="+M")

p2 <- ggplot(bc_withM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  scale_y_continuous(breaks=seq(-1, 2, by=0.5),limits=c(-1.5, 2.5))+
  xlab("")+ylab("lg(Ratio of BcCutA/AtSKII (LP vs NC))")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p2

pdf("Figure_4b_withM.pdf", width=5, height=4,useDingbats = F)
p2
dev.off()


##### botrytis cinerea statistics ##############################################

########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(bc$geno)){
  x <- subset(bc, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 3.56245476533096e-08"
#[1] "p-value for dde2 : 1.83605409545429e-06"
#[1] "p-value for sid2 : 5.31459502098499e-08"
#[1] "p-value for myc2 : 7.07963735745208e-08"
#[1] "p-value for della : 5.76302685019517e-08"
#[1] "p-value for jazq : 1.42851983041094e-06"
#[1] "p-value for bri1 : 4.14632300098353e-07"
#[1] "p-value for sav : 1.19029065826159e-07"
#[1] "p-value for cry1cry2 : 4.39286681169973e-08"

#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(bc_col0_lp$y,bc_col0_nc$y, paired = FALSE)
#W = 144, p-value = 7.396e-07
# ***

wilcox.test(bc_col0_lpm$y,bc_col0_ncm$y, paired = FALSE)
#W = 141, p-value = 5.177e-06
# ***

wilcox.test(bc_sid2_lp$y,bc_sid2_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(bc_sid2_lpm$y,bc_sid2_ncm$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(bc_della_lp$y,bc_della_nc$y, paired = FALSE)
#W = 81, p-value = 0.0003927
# ***

wilcox.test(bc_della_lpm$y,bc_della_ncm$y, paired = FALSE)
#W = 81, p-value = 0.0003765
# ***

wilcox.test(bc_dde2_lp$y,bc_dde2_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(bc_dde2_lpm$y,bc_dde2_ncm$y, paired = FALSE)
#W = 77, p-value = 0.0004936
# ***

wilcox.test(bc_myc2_lp$y,bc_myc2_nc$y, paired = FALSE)
#W = 81, p-value = 0.0004038
# ***

wilcox.test(bc_myc2_lpm$y,bc_myc2_ncm$y, paired = FALSE)
#W = 61, p-value = 0.07701
# ns

wilcox.test(bc_jazq_lp$y,bc_jazq_nc$y, paired = FALSE)
#W = 42.5, p-value = 0.8946
# ns

wilcox.test(bc_jazq_lpm$y,bc_jazq_ncm$y, paired = FALSE)
#W = 57, p-value = 0.1615
# ns

wilcox.test(bc_bri1_lp$y,bc_bri1_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(bc_bri1_lpm$y,bc_bri1_ncm$y, paired = FALSE)
#W = 22, p-value = 0.1118
# ns

wilcox.test(bc_cry1cry2_lp$y,bc_cry1cry2_nc$y, paired = FALSE)
#W = 76, p-value = 0.001987
# **

wilcox.test(bc_cry1cry2_lpm$y,bc_cry1cry2_ncm$y, paired = FALSE)
#W = 74, p-value = 0.001851
# **

wilcox.test(bc_sav_lp$y,bc_sav_nc$y, paired = FALSE)
#W = 81, p-value = 0.0004038
# ***

wilcox.test(bc_sav_lpm$y,bc_sav_ncm$y, paired = FALSE)
#W = 79, p-value = 0.0001645
# ***

for (g in unique(bc_withouM$geno)){
  x <- subset(bc_withouM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.595581328621083"
#[1] "p-value for sid2 : 0.090482851747075"
#[1] "p-value for dde2 : 0.521519795358137"
#[1] "p-value for myc2 : 0.319826874509034"
#[1] "p-value for jazq : 0.948573956966417"
#[1] "p-value for della : 0.513629193353982"
#[1] "p-value for bri1 : 0.717669147262712"
#[1] "p-value for cry1cry2 : 0.635297080042829"
#[1] "p-value for sav : 0.401004093566949"

### compact letter display
library(multcomp)
result <- aov(y ~ geno, bc_withouM)
dt <- glht(result, linfct=mcp(geno="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

#col0     sid2    della     dde2     myc2     jazq     bri1 cry1cry2      sav 
#"cd"     "ab"     "bc"      "d"     "bc"      "e"     "cd"      "d"      "a" 

for (g in unique(bc_withM$geno)){
  x <- subset(bc_withM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.970228670868852"
#[1] "p-value for sid2 : 0.476463580932157"
#[1] "p-value for dde2 : 0.758713566077256"
#[1] "p-value for myc2 : 0.548140569425805"
#[1] "p-value for jazq : 0.625882667748163"
#[1] "p-value for della : 0.096947493720316"
#[1] "p-value for bri1 : 0.404205808198998"
#[1] "p-value for cry1cry2 : 0.806777483221102"
#[1] "p-value for sav : 0.0178063475953204"

### compact letter display
library(multcomp)
result <- aov(y ~ geno, bc_withM)
dt <- glht(result, linfct=mcp(geno="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

#col0     sid2    della     dde2     myc2     jazq     bri1 cry1cry2      sav 
#"bc"     "ab"     "cd"     "bc"      "d"      "d"      "e"     "bd"      "a" 


library("gplots")
library("ggplot2")

pto <- read.table("Pst_mutants.txt", sep="\t",header=T)

pto$y <- 10^pto$y

pto_col0 <- subset(pto, geno=="col0")

pto_col0_nc <- subset(pto_col0, lab=="NC")
pto_col0_ncm <- subset(pto_col0, lab=="NC+M")
pto_col0_lp <- subset(pto_col0, lab=="LP")
pto_col0_lpm <- subset(pto_col0, lab=="LP+M")

pto_sid2 <- subset(pto, geno=="sid2")

pto_sid2_nc <- subset(pto_sid2, lab=="NC")
pto_sid2_ncm <- subset(pto_sid2, lab=="NC+M")
pto_sid2_lp <- subset(pto_sid2, lab=="LP")
pto_sid2_lpm <- subset(pto_sid2, lab=="LP+M")

pto_dde2 <- subset(pto, geno=="dde2")

pto_dde2_nc <- subset(pto_dde2, lab=="NC")
pto_dde2_ncm <- subset(pto_dde2, lab=="NC+M")
pto_dde2_lp <- subset(pto_dde2, lab=="LP")
pto_dde2_lpm <- subset(pto_dde2, lab=="LP+M")

pto_myc2 <- subset(pto, geno=="myc2")

pto_myc2_nc <- subset(pto_myc2, lab=="NC")
pto_myc2_ncm <- subset(pto_myc2, lab=="NC+M")
pto_myc2_lp <- subset(pto_myc2, lab=="LP")
pto_myc2_lpm <- subset(pto_myc2, lab=="LP+M")

pto_jazq <- subset(pto, geno=="jazq")

pto_jazq_nc <- subset(pto_jazq, lab=="NC")
pto_jazq_ncm <- subset(pto_jazq, lab=="NC+M")
pto_jazq_lp <- subset(pto_jazq, lab=="LP")
pto_jazq_lpm <- subset(pto_jazq, lab=="LP+M")

pto_della <- subset(pto, geno=="della")

pto_della_nc <- subset(pto_della, lab=="NC")
pto_della_ncm <- subset(pto_della, lab=="NC+M")
pto_della_lp <- subset(pto_della, lab=="LP")
pto_della_lpm <- subset(pto_della, lab=="LP+M")

pto_bri1 <- subset(pto, geno=="bri1")

pto_bri1_nc <- subset(pto_bri1, lab=="NC")
pto_bri1_ncm <- subset(pto_bri1, lab=="NC+M")
pto_bri1_lp <- subset(pto_bri1, lab=="LP")
pto_bri1_lpm <- subset(pto_bri1, lab=="LP+M")

pto_cry1cry2 <- subset(pto, geno=="cry1cry2")

pto_cry1cry2_nc <- subset(pto_cry1cry2, lab=="NC")
pto_cry1cry2_ncm <- subset(pto_cry1cry2, lab=="NC+M")
pto_cry1cry2_lp <- subset(pto_cry1cry2, lab=="LP")
pto_cry1cry2_lpm <- subset(pto_cry1cry2, lab=="LP+M")

pto_sav <- subset(pto, geno=="sav")

pto_sav_nc <- subset(pto_sav, lab=="NC")
pto_sav_ncm <- subset(pto_sav, lab=="NC+M")
pto_sav_lp <- subset(pto_sav, lab=="LP")
pto_sav_lpm <- subset(pto_sav, lab=="LP+M")



col0_withoutM<- as.matrix(pto_col0_lp$y/mean(pto_col0_nc$y))
r1 <- cbind(as.data.frame(col0_withoutM),rep("col0-M",nrow(col0_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(col0_withoutM))
r1 <- cbind(r1,microbe)
col0_withM <- as.matrix(pto_col0_lpm$y/mean(pto_col0_ncm$y))
r2 <- cbind(as.data.frame(col0_withM),rep("col0+M",nrow(col0_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(col0_withM))
r2 <- cbind(r2,microbe)

col0 <- rbind(r1,r2)
geno <- rep("col0",nrow(col0))
col0 <- cbind(col0,geno)

sid2_withoutM<- as.matrix(pto_sid2_lp$y/mean(pto_sid2_nc$y))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(pto_sid2_lpm$y/mean(pto_sid2_ncm$y))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

sid2_withoutM<- as.matrix(pto_sid2_lp$y/mean(pto_sid2_nc$y))
r1 <- cbind(as.data.frame(sid2_withoutM),rep("sid2-M",nrow(sid2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sid2_withoutM))
r1 <- cbind(r1,microbe)
sid2_withM <- as.matrix(pto_sid2_lpm$y/mean(pto_sid2_ncm$y))
r2 <- cbind(as.data.frame(sid2_withM),rep("sid2+M",nrow(sid2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sid2_withM))
r2 <- cbind(r2,microbe)

sid2 <- rbind(r1,r2)
geno <- rep("sid2",nrow(sid2))
sid2 <- cbind(sid2,geno)

dde2_withoutM<- as.matrix(pto_dde2_lp$y/mean(pto_dde2_nc$y))
r1 <- cbind(as.data.frame(dde2_withoutM),rep("dde2-M",nrow(dde2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(dde2_withoutM))
r1 <- cbind(r1,microbe)
dde2_withM <- as.matrix(pto_dde2_lpm$y/mean(pto_dde2_ncm$y))
r2 <- cbind(as.data.frame(dde2_withM),rep("dde2+M",nrow(dde2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(dde2_withM))
r2 <- cbind(r2,microbe)

dde2 <- rbind(r1,r2)
geno <- rep("dde2",nrow(dde2))
dde2 <- cbind(dde2,geno)

myc2_withoutM<- as.matrix(pto_myc2_lp$y/mean(pto_myc2_nc$y))
r1 <- cbind(as.data.frame(myc2_withoutM),rep("myc2-M",nrow(myc2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(myc2_withoutM))
r1 <- cbind(r1,microbe)
myc2_withM <- as.matrix(pto_myc2_lpm$y/mean(pto_myc2_ncm$y))
r2 <- cbind(as.data.frame(myc2_withM),rep("myc2+M",nrow(myc2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(myc2_withM))
r2 <- cbind(r2,microbe)

myc2 <- rbind(r1,r2)
geno <- rep("myc2",nrow(myc2))
myc2 <- cbind(myc2,geno)

jazq_withoutM<- as.matrix(pto_jazq_lp$y/mean(pto_jazq_nc$y))
r1 <- cbind(as.data.frame(jazq_withoutM),rep("jazq-M",nrow(jazq_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(jazq_withoutM))
r1 <- cbind(r1,microbe)
jazq_withM <- as.matrix(pto_jazq_lpm$y/mean(pto_jazq_ncm$y))
r2 <- cbind(as.data.frame(jazq_withM),rep("jazq+M",nrow(jazq_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(jazq_withM))
r2 <- cbind(r2,microbe)

jazq <- rbind(r1,r2)
geno <- rep("jazq",nrow(jazq))
jazq <- cbind(jazq,geno)

della_withoutM<- as.matrix(pto_della_lp$y/mean(pto_della_nc$y))
r1 <- cbind(as.data.frame(della_withoutM),rep("della-M",nrow(della_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(della_withoutM))
r1 <- cbind(r1,microbe)
della_withM <- as.matrix(pto_della_lpm$y/mean(pto_della_ncm$y))
r2 <- cbind(as.data.frame(della_withM),rep("della+M",nrow(della_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(della_withM))
r2 <- cbind(r2,microbe)

della <- rbind(r1,r2)
geno <- rep("della",nrow(della))
della <- cbind(della,geno)

bri1_withoutM<- as.matrix(pto_bri1_lp$y/mean(pto_bri1_nc$y))
r1 <- cbind(as.data.frame(bri1_withoutM),rep("bri1-M",nrow(bri1_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(bri1_withoutM))
r1 <- cbind(r1,microbe)
bri1_withM <- as.matrix(pto_bri1_lpm$y/mean(pto_bri1_ncm$y))
r2 <- cbind(as.data.frame(bri1_withM),rep("bri1+M",nrow(bri1_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(bri1_withM))
r2 <- cbind(r2,microbe)

bri1 <- rbind(r1,r2)
geno <- rep("bri1",nrow(bri1))
bri1 <- cbind(bri1,geno)

cry1cry2_withoutM<- as.matrix(pto_cry1cry2_lp$y/mean(pto_cry1cry2_nc$y))
r1 <- cbind(as.data.frame(cry1cry2_withoutM),rep("cry1cry2-M",nrow(cry1cry2_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(cry1cry2_withoutM))
r1 <- cbind(r1,microbe)
cry1cry2_withM <- as.matrix(pto_cry1cry2_lpm$y/mean(pto_cry1cry2_ncm$y))
r2 <- cbind(as.data.frame(cry1cry2_withM),rep("cry1cry2+M",nrow(cry1cry2_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(cry1cry2_withM))
r2 <- cbind(r2,microbe)

cry1cry2 <- rbind(r1,r2)
geno <- rep("cry1cry2",nrow(cry1cry2))
cry1cry2 <- cbind(cry1cry2,geno)

sav_withoutM<- as.matrix(pto_sav_lp$y/mean(pto_sav_nc$y))
r1 <- cbind(as.data.frame(sav_withoutM),rep("sav-M",nrow(sav_withoutM)))
colnames(r1) <- c("y","lab")
microbe <- rep("-M",nrow(sav_withoutM))
r1 <- cbind(r1,microbe)
sav_withM <- as.matrix(pto_sav_lpm$y/mean(pto_sav_ncm$y))
r2 <- cbind(as.data.frame(sav_withM),rep("sav+M",nrow(sav_withM)))
colnames(r2) <- c("y","lab")
microbe <- rep("+M",nrow(sav_withM))
r2 <- cbind(r2,microbe)

sav <- rbind(r1,r2)
geno <- rep("sav",nrow(sav))
sav <- cbind(sav,geno)

pto1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
pto1$y <- log(pto1$y,10)

pto1$geno <- factor(pto1$geno, levels = levels(pto1$geno)[c(
  1,2,6,3,4,5,7,8,9
)], ordered = T)


pto_withouM <- subset(pto1, microbe=="-M")


p1 <- ggplot(pto_withouM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  scale_y_continuous(breaks=seq(-1, 2, by=0.5),limits=c(-1, 2))+
  geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  xlab("")+ylab("lg(Ratio of CFU/g (LP vs NC))")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p1

pdf("Figure_4c_withoutM.pdf", width=5, height=4,useDingbats = F)
p1
dev.off()


pto_withM <- subset(pto1, microbe=="+M")


p2 <- ggplot(pto_withM,aes(x=geno, y=y, group=geno))+
  geom_boxplot(outlier.shape=NA, aes(fill=geno))+
  scale_fill_manual(values=c("black","yellow","green","red","darkred","pink","orange","blue","gray"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  scale_y_continuous(breaks=seq(-1, 2, by=0.5),limits=c(-1, 2))+
  xlab("")+ylab("lg(Ratio of CFU/g (LP vs NC))")+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#+facet_wrap(~geno,nrow=1)

p2

pdf("Figure_4c_withM.pdf", width=5, height=4,useDingbats = F)
p2
dev.off()

########################################################################################
##statistics############################################################################
########################################################################################

for (g in unique(pto1$geno)){
  x <- subset(pto1, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.245949472260843"
#[1] "p-value for sid2 : 0.503239965444132"
#[1] "p-value for dde2 : 0.0465531951776556"
#[1] "p-value for myc2 : 0.000438843367851652"
#[1] "p-value for jazq : 0.0772966223573237"
#[1] "p-value for della : 0.49107055753986"
#[1] "p-value for bri1 : 0.0115108037211481"
#[1] "p-value for cry1cry2 : 0.00591769124744677"
#[1] "p-value for sav : 0.781018626286802"

#Test for variance homogeneity
library(lawstat)
levene.test(pto1$y,pto1$geno)
# data:  pto1$y
#Test Statistic = 17.77, p-value < 2.2e-16

#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(pto_col0_lp$y,pto_col0_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_col0_lpm$y,pto_col0_ncm$y, paired = FALSE)
#W = 77, p-value = 0.00147
# **

wilcox.test(pto_sid2_lp$y,pto_sid2_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_sid2_lpm$y,pto_sid2_ncm$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_della_lp$y,pto_della_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_della_lpm$y,pto_della_ncm$y, paired = FALSE)
#W = 75, p-value = 0.001234
# **

wilcox.test(pto_dde2_lp$y,pto_dde2_nc$y, paired = FALSE)
#W = 81, p-value = 0.0004095
# ***

wilcox.test(pto_dde2_lpm$y,pto_dde2_ncm$y, paired = FALSE)
#W = 68, p-value = 0.01419
# *

wilcox.test(pto_myc2_lp$y,pto_myc2_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_myc2_lpm$y,pto_myc2_ncm$y, paired = FALSE)
#W = 36, p-value = 0.7304
# ns

wilcox.test(pto_jazq_lp$y,pto_jazq_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_jazq_lpm$y,pto_jazq_ncm$y, paired = FALSE)
#W = 62, p-value = 0.06253
# ns

wilcox.test(pto_bri1_lp$y,pto_bri1_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_bri1_lpm$y,pto_bri1_ncm$y, paired = FALSE)
#W = 37, p-value = 0.7962
# ns

wilcox.test(pto_cry1cry2_lp$y,pto_cry1cry2_nc$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

wilcox.test(pto_cry1cry2_lpm$y,pto_cry1cry2_ncm$y, paired = FALSE)
#W = 58, p-value = 0.1359
# ns

wilcox.test(pto_sav_lp$y,pto_sav_nc$y, paired = FALSE)
#W = 81, p-value = 0.0004095
# ***

wilcox.test(pto_sav_lpm$y,pto_sav_ncm$y, paired = FALSE)
#W = 81, p-value = 4.114e-05
# ***

for (g in unique(pto_withouM$geno)){
  x <- subset(pto_withouM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.21332583513159"
#[1] "p-value for sid2 : 0.712816757573399"
#[1] "p-value for dde2 : 0.930674477177211"
#[1] "p-value for myc2 : 0.82047419318925"
#[1] "p-value for jazq : 0.881380002547287"
#[1] "p-value for della : 0.27666680769157"
#[1] "p-value for bri1 : 0.244744109621656"
#[1] "p-value for cry1cry2 : 0.847228925010563"
#[1] "p-value for sav : 0.158582235243446"

### compact letter display
library(multcomp)
result <- aov(y ~ geno, pto_withouM)
dt <- glht(result, linfct=mcp(geno="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
#col0     sid2    della     dde2     myc2     jazq     bri1 cry1cry2      sav 
#"b"     "bc"     "ab"     "ab"     "ab"     "bc"     "ab"      "a"      "c" 

for (g in unique(pto_withM$geno)){
  x <- subset(pto_withM, geno == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for col0 : 0.310605600645399"
#[1] "p-value for sid2 : 0.745205014375857"
#[1] "p-value for dde2 : 0.625517666915469"
#[1] "p-value for myc2 : 0.0143769773265727"
#[1] "p-value for jazq : 0.820130614120228"
#[1] "p-value for della : 0.137198415542748"
#[1] "p-value for bri1 : 0.235175004575128"
#[1] "p-value for cry1cry2 : 0.845059474202254"
#[1] "p-value for sav : 0.939576838829579"

### compact letter display
library(multcomp)
result <- aov(y ~ geno, pto_withM)
dt <- glht(result, linfct=mcp(geno="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
#col0     sid2    della     dde2     myc2     jazq     bri1 cry1cry2      sav 
#"bc"      "b"     "bd"     "cd"      "e"    "cde"      "e"     "de"      "a" 

