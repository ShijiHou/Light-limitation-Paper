library("ggplot2")

Input1 <- read.table("CS_col0_withFR.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,3,4,1,2)], ordered = T)

CS_col0 <- ggplot(Input1,aes(x=lab, y=canopysize, group=lab, fill=light))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy Size/cm2")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NC+M","LP","LP+M","FR","FR+M"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 5, by=1),limits=c(0,5))+
  guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,hjust=0.5))+
  theme(axis.text.y = element_text(size=12,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")


print(CS_col0)


pdf("Figure_1a.pdf", width=4, height=5, useDingbats = F)
CS_col0
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
 for (g in unique(Input1$lab)){
   x <- subset(Input1, lab == g, sel = c("canopysize"))
   y <- shapiro.test(x[,1])
   print(paste("p-value for", g, ":", y[2]))
 }

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

#[1] "p-value for NC : 0.0634030024268104"
#[1] "p-value for LP : 0.0324914682353262"
#[1] "p-value for NC+M : 0.260967951375001"
#[1] "p-value for LP+M : 0.00725042583659634"
#[1] "p-value for FR : 0.738790828096633"
#[1] "p-value for FR+M : 0.161131336714359"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$canopysize,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$shootfreshweight
#Test Statistic = 1.8323, p-value = 0.1096


## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["canopysize"]],Input1[["lab"]])
dunn.test(Input1[["canopysize"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")

### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["canopysize"]],Input1[["lab"]], wrap=T, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05, reversed=T)


#    Group  Letter   MonoLetter
# 1    FR      c          c
# 2  FR+M      b         b 
# 3    LP      c          c
# 4  LP+M     ab        ab 
# 5    NC     ab        ab 
# 6  NC+M      a        a  

## ab a c ab c b


####calculate the effect size of BFO on canopysize under NC, LP, and EODFR
library(effsize)
nc <- subset(Input1,lab=="NC")
ncm <- subset(Input1,lab=="NC+M")
lp <- subset(Input1, lab=="LP")
lpm <- subset(Input1,lab=="LP+M")
fr <- subset(Input1,lab=="FR")
frm <- subset(Input1,lab=="FR+M")

NC <- cohen.d(ncm$canopysize,nc$canopysize)
LP <- cohen.d(lpm$canopysize,lp$canopysize)
FR <- cohen.d(frm$canopysize,fr$canopysize)

cs <- c(NC$estimate,LP$estimate,FR$estimate)
cs <- as.data.frame(cs)
rownames(cs) <- c("NC","LP","FR")
write.table(cs, "cs_effsize_byM.txt", sep="\t", quote=FALSE) 








