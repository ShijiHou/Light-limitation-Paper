library("ggplot2")

Input1 <- read.table("CS_BFOwithB.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(4,5,6,1,2,3)], ordered = T)

CS_col0 <- ggplot(Input1,aes(x=lab, y=cs, group=lab, fill=light))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NCB","NCM","LP","LPB","LPM"),labels=c("-BFO","+B","+BFO","-BFO","+B","+BFO"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4.2))+
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

pdf("Figure_1d.pdf", width=4, height=5,useDingbats = F)
CS_col0
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
 for (g in unique(Input1$lab)){
   x <- subset(Input1, lab == g, sel = c("cs"))
   y <- shapiro.test(x[,1])
   print(paste("p-value for", g, ":", y[2]))
 }

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

#[1] "p-value for NCM : 0.421366691818795"
#[1] "p-value for NCB : 0.0267069936674694"
#[1] "p-value for NC : 0.0481854996373134"
#[1] "p-value for LPM : 0.0266157616691938"
#[1] "p-value for LPB : 0.325887137015489"
#[1] "p-value for LP : 0.104016408631481"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$cs,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$cs
#Test Statistic = 1.5313, p-value = 0.1867


## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["cs"]],Input1[["lab"]])
dunn.test(Input1[["cs"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")



### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["cs"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

#Group Letter MonoLetter
#1    LP      c          c
#2   LPB      b         b 
#3   LPM      a        a  
#4    NC     ab        ab 
#5   NCB     ab        ab 
#6   NCM     ab        ab 

### ab ab ab c b a 









