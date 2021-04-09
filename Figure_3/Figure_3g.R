library("ggplot2")

Input <- read.table("pst1.txt", sep="\t",header=T)

p <- ggplot(Input,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.2), size=2)+
  xlab(" ")+ylab("lg(CFU/g)")+
  ggtitle("Leaf bacterial pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC_M","SC","SC_M"),labels=c("-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(5.5, 8, by=0.5),limits=c(5.5,8))+
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

pdf("Figure_3g.pdf", width=3, height=3)
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

#[1] "p-value for NC : 0.320907343821844"
#[1] "p-value for SC : 0.0434749710621874"
#[1] "p-value for NC_M : 0.904330050852306"
#[1] "p-value for SC_M : 0.665869162157804"


#Test for variance homogeneity
library(lawstat)
levene.test(Input$y,Input$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

#data:  Input$y
#Test Statistic = 2.1034, p-value = 0.1193


result <- aov(Input[["y"]] ~ Input[["lab"]])
summary(result)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#  Input[["lab"]]  3  8.019  2.6729   89.93 1.12e-15 ***
#  Residuals      32  0.951  0.0297                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TukeyHSD(result)

#Tukey multiple comparisons of means
#95% family-wise confidence level

#Fit: aov(formula = Input[["y"]] ~ Input[["lab"]])

#$`Input[["lab"]]`
#diff        lwr        upr     p adj
#NC_M-NC   -0.4627468 -0.6829396 -0.2425540 0.0000152
#SC-NC      0.8239320  0.6037392  1.0441248 0.0000000
#SC_M-NC   -0.1147064 -0.3348992  0.1054864 0.5016781
#SC-NC_M    1.2866788  1.0664860  1.5068716 0.0000000
#SC_M-NC_M  0.3480403  0.1278476  0.5682331 0.0008656
#SC_M-SC   -0.9386384 -1.1588312 -0.7184456 0.0000000

### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
### compact letter display
library(multcomp)
result <- aov(y ~ Con, Input)
dt <- glht(result, linfct=mcp(Con="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
# NC NC_M   SC SC_M 
# "b"  "c"  "a"  "b" 

