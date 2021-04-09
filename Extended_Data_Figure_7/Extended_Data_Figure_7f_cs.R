library("ggplot2")

Input1 <- read.table("canopysize.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,8,7,1,2,4,3)], ordered = T)

p1 <- ggplot(Input1,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC-BFO" ,"NC+(B-Pseu)","NC+Pseu","NC+B","LP-BFO","LP+(B-Pseu)","LP+Pseu","LP+B"),
                   labels=c("-BFO","+(B-Pseu)","+Pseu","+B","-BFO","+(B-Pseu)","+Pseu","+B"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4.2))+
  #guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,angle=45,hjust=1))+
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

pdf("CS.pdf", width=4.5, height=4,useDingbats = F)
p1
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

#[1] "p-value for NC-BFO : 0.27632119506685"
#[1] "p-value for NC+Pseu : 0.581289765763601"
#[1] "p-value for NC+(B-Pseu) : 0.079920046717082"
#[1] "p-value for NC+B : 0.00641872009431678"
#[1] "p-value for LP-BFO : 0.0133354136705763"
#[1] "p-value for LP+Pseu : 0.826125918351075"
#[1] "p-value for LP+(B-Pseu) : 0.534099660533955"
#[1] "p-value for LP+B : 0.54891489986565"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$y
#Test Statistic = 3.6461, p-value = 0.0008781


## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab1"]])
dunn.test(Input1[["y"]],Input1[["lab1"]], wrap=TRUE, method="bonferroni")


### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["y"]],Input1[["lab1"]], wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

#Group Letter MonoLetter
#1     1      a         a 
#2     2      a         a 
#3     3      a         a 
#4     4      a         a 
#5     5      b          b
#6     6      b          b
#7     7      b          b
#8     8      a         a 

### a a a a b b b a









