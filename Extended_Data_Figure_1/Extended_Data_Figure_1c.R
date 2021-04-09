library(ggplot2)

Input <- read.table("RFA.txt", sep="\t",header=T)

RFA_FW <- ggplot(Input,aes(x=RatioNumber, y=FreshWeight, group=RatioNumber))+
  #geom_boxplot(fill="white")+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(position=position_jitter(width=0.4), size=1, fill="white")+
  xlab("")+ylab("Shoot fresh weight/g")+
  #labs(title="Ratio_FlpwPot_All")+
  scale_x_discrete(limits=c("-BFO", "LLL", "HLL", "LHL", "LLH", "HHL", "HLH", "LHH", "HHH"))+
  scale_y_continuous(breaks=seq(0, 0.1, by=0.04),limits=c(0,0.1))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y = element_text(angle=90,hjust=1))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))

RFA_FW
pdf("Extended_Data_Figure_1c.pdf", width=5.5, height=3,useDingbats = F)
RFA_FW
dev.off()

########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$RatioNumber)){
  x <- subset(Input, RatioNumber == g, sel = c("FreshWeight"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for -BFO : 0.000179279928965389"
#[1] "p-value for LLL : 0.190306760839517"
#[1] "p-value for LLH : 0.00152036482923873"
#[1] "p-value for LHL : 0.347443873633669"
#[1] "p-value for LHH : 0.0206240740064674"
#[1] "p-value for HLL : 1.00934726701808e-05"
#[1] "p-value for HLH : 0.240623743349517"
#[1] "p-value for HHL : 0.0118369840066099"
#[1] "p-value for HHH : 0.0622649129899603"

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

#Test for variance homogeneity
library(lawstat)
levene.test(Input$FreshWeight,Input$RatioNumber)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

#data:  Input$FreshWeight
#Test Statistic = 0.28314, p-value = 0.9709

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input[["FreshWeight"]],Input[["RatioNumber"]])
dt <- dunn.test(Input[["FreshWeight"]],Input[["RatioNumber"]], wrap=TRUE, method="bonferroni")

#Kruskal-Wallis rank sum test

#data: x and group
#Kruskal-Wallis chi-squared = 10.5936, df = 8, p-value = 0.23


#Comparison of x by group                            
#(Bonferroni)                                  
#  Col Mean-|
#  Row Mean |       -BFO        HHH        HHL        HLH        HLL        LHH        LHL        LLH
#  ---------+----------------------------------------------------------------------------------------
#       HHH |  -2.032848
#           |     0.7572
#           |
#       HHL |  -1.803159   0.248557
#           |     1.0000     1.0000
#           |
#       HLH |  -2.785926  -0.736525  -0.993597
#           |     0.0961     1.0000     1.0000
#           |
#       HLL |  -1.319109   0.698050   0.457563   1.434575
#           |     1.0000     1.0000     1.0000     1.0000
#           |
#       LHH |  -1.653498   0.371010   0.126742   1.107536  -0.327039
#           |     1.0000     1.0000     1.0000     1.0000     1.0000
#           |
#       LHL |  -1.867058   0.162145  -0.084537   0.898671  -0.535904  -0.208865
#           |     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000
#           |
#       LLH |  -2.460120  -0.393622  -0.649780   0.351417  -1.099743  -0.768923  -0.557643
#           |     0.2500     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000
#           |
#       LLL |  -2.354246  -0.290131  -0.545063   0.454909  -0.996251  -0.665431  -0.454151   0.104716
#           |     0.3341     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000     1.0000

#alpha = 0.05
#Reject Ho if p <= alpha/2

# no significant differences

# a a a a a a a a a 



