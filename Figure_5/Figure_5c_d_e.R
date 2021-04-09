library("ggplot2")

Input1 <- read.table("SVM_cs.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,7,8,6,1,3,4,2)], ordered = T)

cs <- ggplot(Input1,aes(x=lab, y=cs, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy size (cm2)")+
  scale_x_discrete(breaks=c("NC-B","NC+SL","NC+SVM","NC+B","LP-B","LP+SL","LP+SVM","LP+B"),
                   labels=c("-BFO","+(B-SVM)","+SVM","+B","-BFO","+(B-SVM)","+SVM","+B"))+
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

pdf("Figure_5c.pdf", width=5, height=6,useDingbats = F)
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
kruskal.test(Input1[["cs"]],Input1[["lab1"]])
dunn.test(Input1[["cs"]],Input1[["lab1"]], wrap=TRUE, method="bonferroni")


### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["cs"]],Input1[["lab1"]], wrap=T, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05)

# Group Letter MonoLetter
# 1     1      a       a   
# 2     2      a       a   
# 3     3      a       a   
# 4     4      a       a   
# 5     5      b        b  
# 6     6     bc        bc 
# 7     7     cd         cd
# 8     8     ad       a  d

# a a a a b bc cd ad

##########################################################################################################################
##############################Bc##########################################################################################
Input <- read.table("SVM_bc.txt", sep="\t",header=T)
Input$lab <- factor(Input$lab, levels = levels(Input$lab)[c(5,6,8,7,1,2,4,3)], ordered = T)

p <- ggplot(Input,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab(" ")+ylab("lg(BcCutA/AtSKII)")+
  #ggtitle("Leaf fungal pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC+(B-SVM)","NC+SVM","NC+B","LP","LP+(B-SVM)","LP+SVM","LP+B"),
                   labels=c("-BFO","+(B-SVM)","+SVM","+B","-BFO","+(B-SVM)","+SVM","+B"))+
  scale_y_continuous(breaks=seq(-3, 0.5, by=1),limits=c(-3, 0.5))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=12,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+ theme(legend.position = "none") 


print(p)

pdf("Figure_5d.pdf", width=5.5, height=6.5,useDingbats = F)
p
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$lab)){
  x <- subset(Input, lab == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
# [1] "p-value for NC : 0.654772712517442"
# [1] "p-value for NC+(B-SVM) : 0.337882196284486"
# [1] "p-value for NC+SVM : 0.237310736428578"
# [1] "p-value for NC+B : 0.280068932397924"
# [1] "p-value for LP : 0.963954025652747"
# [1] "p-value for LP+(B-SVM) : 0.322143539981351"
# [1] "p-value for LP+SVM : 0.129120106872632"
# [1] "p-value for LP+B : 0.328199200218495"


#Test for variance homogeneity
library(lawstat)
levene.test(Input$y,Input$lab)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

# data:  Input$y
# Test Statistic = 2.2885, p-value = 0.03813


result <- aov(Input[["y"]] ~ Input[["lab"]])
summary(result)

#                   Df Sum Sq Mean Sq F value Pr(>F)    
# Input[["lab"]]  7  44.67   6.381   225.2 <2e-16 ***
#   Residuals      64   1.81   0.028                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

#  NC    NC+(B-SVM)     NC+SVM     NC+B        LP    LP+(B-SVM)     LP+SVM     LP+B 
# "b"        "d"        "d"        "d"        "a"        "d"        "c"        "c" 


#################################################################################################################
#######################################Pst#######################################################################

Input1 <- read.table("SVM_pst.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,7,8,6,1,3,4,2)], ordered = T)


p1 <- ggplot(Input1,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab(" ")+ylab("lg(CFU/g)")+
  #ggtitle("Leaf bacterial pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC+SL","NC+SVM","NC+B","LP","LP+SL","LP+SVM","LP+B"),
                   labels=c("-BFO","+(B-SVM)","+SVM","+B","-BFO","+(B-SVM)","+SVM","+B"))+
  scale_y_continuous(breaks=seq(5.5, 8, by=0.5),limits=c(5.5,8))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=12,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=12,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+ theme(legend.position = "none") 


print(p1)

pdf("SVM_pst.pdf", width=5.5, height=6.5,useDingbats = F)
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

# [1] "p-value for NC : 0.397888584882847"
# [1] "p-value for NC+SL : 0.630309153774488"
# [1] "p-value for NC+SVM : 0.493887284469841"
# [1] "p-value for NC+B : 0.166031028450588"
# [1] "p-value for LP : 0.832012059683385"
# [1] "p-value for LP+SL : 0.0700850839461947"
# [1] "p-value for LP+SVM : 0.309860568754933"
# [1] "p-value for LP+B : 0.336030063563599"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

#data:  Input$y
#Test Statistic = 0.7027, p-value = 0.6696


result <- aov(Input1[["y"]] ~ Input1[["lab"]])
summary(result)


### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)


# NC  NC+SL NC+SVM   NC+B     LP  LP+SL LP+SVM   LP+B 
# "b"    "c"    "c"    "c"    "a"    "c"    "b"    "b"

library(ggpubr)
pp <- ggarrange(cs,p,p1,
               ncol=1, nrow = 3)

pdf("Figure_5c_d_e.pdf", width=6, height=20,useDingbats = F)
pp
dev.off()


