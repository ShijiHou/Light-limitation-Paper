library("ggplot2")

Input1 <- read.table("CAS_soil_cs.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(6,9,8,10,7,1,4,3,5,2)], ordered = T)


cs <- ggplot(Input1,aes(x=lab, y=cs, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","white","gray48","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  scale_x_discrete(breaks=c("NC-BFO", "NC+HK",  "NC+CAS", "NC+SF",  "NC+B",   "LP-BFO", "LP+HK",  "LP+CAS", "LP+SF",  "LP+B" ),
                   labels=c("-BFO", "+HK",  "+CAS", "+SF",  "+B",   "-BFO", "+HK",  "+CAS", "+SF",  "+B"))+
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


print(cs)

pdf("Extended_Data_Figure_3a.pdf", width=5, height=4,useDingbats = F)
cs
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

# [1] "p-value for NC-B : 0.457168998869135"
# [1] "p-value for NC+SVM : 0.212132771501078"
# [1] "p-value for NC+SL : 0.382432578274862"
# [1] "p-value for NC+B : 0.937397631082589"
# [1] "p-value for LP-B : 0.414462884267146"
# [1] "p-value for LP+SVM : 0.112466645427071"
# [1] "p-value for LP+SL : 0.209182244964103"
# [1] "p-value for LP+B : 0.802038440018673"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$sfw,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
# data:  Input1$sfw
# Test Statistic = 6.1802, p-value = 1.395e-06

## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["cs"]],Input1[["lab"]])
dunn.test(Input1[["cs"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")


### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["cs"]],Input1[["lab1"]], wrap=T, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

#      Group  Letter  MonoLetter
# 1      A     ab        ab 
# 2      B     ab        ab 
# 3      C      a        a  
# 4      D     ab        ab 
# 5      E     ab        ab 
# 6      F      c          c
# 7      G      c          c
# 8      H     ab        ab 
# 9      I     ab        ab 
# 10     J      b         b 





