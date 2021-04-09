library("ggplot2")

Input <- read.table("BC_col0.txt", sep="\t",header=T)


p <- ggplot(Input,aes(x=Con, y=y, group=Con))+
  geom_boxplot(outlier.shape=NA, aes(fill=Con))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.2), size=2)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab(" ")+ylab("lg(BcCutA/AtSKII)")+
  ggtitle("Leaf fungal pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC_M","SC","SC_M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(-3, 0.5, by=1),limits=c(-3, 0.5))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+ theme(legend.position = "none") 


print(p)

pdf("Figure_3f.pdf", width = 3,height = 3)
p
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$Con)){
  x <- subset(Input, Con == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#[1] "p-value for NC : 0.813351272827419"
#[1] "p-value for SC : 0.595581328621077"
#[1] "p-value for NC_M : 0.505043512585006"
#[1] "p-value for SC_M : 0.970228670868851"

#Test for variance homogeneity
library(lawstat)
levene.test(Input$y,Input$Con)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

#data:  Input$y
#Test Statistic = 3.6435, p-value = 0.0197

result <- aov(Input[["y"]] ~ Input[["Con"]])
summary(result)

#                Df Sum Sq Mean Sq F value Pr(>F)    
#Input[["Con"]]  3  59.79  19.929   378.1 <2e-16 ***
#Residuals      44   2.32   0.053                   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TukeyHSD(result)

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = Input[["y"]] ~ Input[["Con"]])

#$`Input[["Con"]]`
#diff        lwr        upr p adj
#NC_M-NC   -2.1539549 -2.4041884 -1.9037214     0
#SC-NC      0.7092059  0.4589724  0.9594394     0
#SC_M-NC   -1.3265138 -1.5767473 -1.0762803     0
#SC-NC_M    2.8631608  2.6129273  3.1133943     0
#SC_M-NC_M  0.8274411  0.5772076  1.0776746     0
#SC_M-SC   -2.0357197 -2.2859532 -1.7854862     0

### compact letter display
library(multcomp)
result <- aov(y ~ Con, Input)
dt <- glht(result, linfct=mcp(Con="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
# NC NC_M   SC SC_M 
# "b"  "d"  "a"  "c" 
