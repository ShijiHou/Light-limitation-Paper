library("ggplot2")

Input1 <- read.table("myc2protein.txt", sep="\t",header=T)

shoot <- subset(Input1,com=="shoot")

shoot$lab <- factor(shoot$lab,levels = levels(shoot$lab)[c(3,4,1,2)],ordered = T)

p1 <- ggplot(shoot,aes(x=lab, y=y, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("MYC2 protein level")+
  ggtitle("Shoot")+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 2.2, by=0.4),limits=c(0,2.3))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,hjust=0.5))+
  theme(axis.text.y = element_text(size=12,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(p1)

pdf("Figure_6b.pdf", width=4, height=5, useDingbats = F)
p1
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test

shapiro.test(subset(Input1,lab1=="NCM_root")$y)
#W = 0.75284, p-value = 0.04105
shapiro.test(subset(Input1,lab1=="LP_root")$y)
#W = 0.93616, p-value = 0.6311
shapiro.test(subset(Input1,lab1=="LPM_root")$y)
#W = 0.84935, p-value = 0.2241

shapiro.test(subset(Input1,lab1=="NCM_shoot")$y)
#W = 0.84207, p-value = 0.1356
shapiro.test(subset(Input1,lab1=="LP_shoot")$y)
#W = 0.77434, p-value = 0.03412
shapiro.test(subset(Input1,lab1=="LPM_shoot")$y)
#W = 0.98433, p-value = 0.9709


#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab1)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$y
#Test Statistic = 2.5365, p-value = 0.03396

## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(shoot[["y"]],shoot[["lab"]])
dt <- dunn.test(shoot[["y"]],shoot[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#1    LP      b          b
#2   LPM      a         a 
#3    NC      a         a 
#4   NCM      a         a 

# a a b a

library(dunn.test)
kruskal.test(root[["y"]],root[["lab"]])
dt <- dunn.test(root[["y"]],root[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)
#Group Letter MonoLetter
#1    LP      a         a 
#2   LPM      b          b
#3    NC     ab         ab
#4   NCM      b         b

# ab b a b