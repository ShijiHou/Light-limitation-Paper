library("ggplot2")

Input1 <- read.table("hl.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(2,1)], ordered = T)

hl <- ggplot(Input1,aes(x=lab, y=hl, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Hypocotyl length (cm)")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_y_continuous(breaks=seq(0, 0.6, by=0.2),limits=c(0,0.7))+
  #guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,hjust = 0.5))+
  theme(axis.text.y = element_text(size=12,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")


print(hl)

pdf("Extended_Data_Figure_2e.pdf", width=2.5, height=3,useDingbats = F)
hl
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
 for (g in unique(Input1$lab)){
   x <- subset(Input1, lab == g, sel = c("hl"))
   y <- shapiro.test(x[,1])
   print(paste("p-value for", g, ":", y[2]))
 }

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

# [1] "p-value for NC : 0.266052058650671"
# [1] "p-value for LP : 0.32392718063204"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$hl,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

# Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
# 
# data:  Input1$hl
# Test Statistic = 23.598, p-value = 2.153e-06

## Kruskal-Wallis test and Dunn test

#Mann-Whitney-U-test (n=2, unpaired)

wilcox.test(hl~lab, data = Input1, paired =F)

# Wilcoxon rank sum test with continuity correction
# 
# data:  hl by lab
# W = 0.5, p-value < 2.2e-16


