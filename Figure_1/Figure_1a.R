library("ggplot2")

Input1 <- read.table("pl_quantified_by_leaflength.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,3,4,1,2)], ordered = T)

pl_col0 <- ggplot(Input1,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Petiole length / leaf length ratio")+
  #ggtitle("Sum of petiole length by plant")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM","FR","FRM"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,angle = 45, hjust=1))+
  theme(axis.text.y = element_text(size=12,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")+
  theme(legend.position = "none")

print(pl_col0)


pdf("Figure_1e.pdf", width=4, height=5, useDingbats = F)
pl_col0
dev.off()

########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input1$lab)){
  x <- subset(Input1, lab == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

# [1] "p-value for NC : 0.0877972655921816"
# [1] "p-value for NCM : 0.198952540614094"
# [1] "p-value for LP : 0.00309297415390245"
# [1] "p-value for LPM : 0.211647065662677"
# [1] "p-value for FR : 0.0780868349382468"
# [1] "p-value for FRM : 0.0139989712742082"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab)
#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
# data:  Input1$y
# Test Statistic = 20.182, p-value < 2.2e-16


## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab"]])
dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")

### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["y"]],Input1[["lab"]], wrap=T, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

#     Group  Letter  MonoLetter
# 1    FR      a       a   
# 2   FRM      b        b  
# 3    LP      c         c 
# 4   LPM      c         c 
# 5    NC      d          d
# 6   NCM      d          d

#### calculate the effect size of BFO on petiole length under NC, LP, and EODFR
library(effsize)
nc <- subset(Input1,lab=="NC")
ncm <- subset(Input1,lab=="NCM")
lp <- subset(Input1, lab=="LP")
lpm <- subset(Input1,lab=="LPM")
fr <- subset(Input1,lab=="FR")
frm <- subset(Input1,lab=="FRM")

NC <- cohen.d(ncm$y,nc$y)
LP <- cohen.d(lpm$y,lp$y)
FR <- cohen.d(frm$y,fr$y)

pl <- c(NC$estimate,LP$estimate,FR$estimate)
pl <- as.data.frame(pl)
rownames(pl) <- c("NC","LP","FR")
write.table(pl, "pl_effsize_byM.txt", sep="\t", quote=FALSE)



nc <- subset(Input1, lab=="NC")
lp <- subset(Input1, lab=="LP")
fr <- subset(Input1, lab=="FR")

mean(lp$y)/mean(nc$y)
#[1] 1.855028

mean(fr$y)/mean(nc$y)
#[1] 2.842011


#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(lp$y,nc$y, paired = FALSE)
#p-value < 2.2e-16

#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(fr$y,nc$y, paired = FALSE)
#p-value < 2.2e-16