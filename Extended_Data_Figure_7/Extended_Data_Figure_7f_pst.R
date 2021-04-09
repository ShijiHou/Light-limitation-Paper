library("ggplot2")

Input1 <- read.table("pst_pseu.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,8,7,1,2,4,3)], ordered = T)


p2 <- ggplot(Input1,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab(" ")+ylab("lg(CFU/g)")+
  #ggtitle("Leaf bacterial pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC+(B-Pseu)","NC+Pseu","NC+B","LP","LP+(B-Pseu)","LP+Pseu","LP+B"),
                   labels=c("-BFO","+(B-Pseu)","+Pseu","+B","-BFO","+(B-Pseu)","+Pseu","+B"))+
  scale_y_continuous(breaks=seq(5.5, 8, by=0.5),limits=c(5.4,8))+
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


print(p2)

pdf("pst_pseu.pdf", width=4.5, height=4,useDingbats = F)
p2
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

# [1] "p-value for NC : 0.977059729163158"
# [1] "p-value for NC+(B-Pseu) : 0.113234940284958"
# [1] "p-value for NC+Pseu : 0.389148382041351"
# [1] "p-value for NC+B : 0.492899658564633"
# [1] "p-value for LP : 0.908613351823109"
# [1] "p-value for LP+(B-Pseu) : 0.0704702777333752"
# [1] "p-value for LP+Pseu : 0.687830933005542"
# [1] "p-value for LP+B : 0.606888370736399"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

#data:  Input$y
#Test Statistic = 1.949, p-value = 0.07618


### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)
### compact letter display
library(multcomp)
result <- aov(y ~ lab, Input1)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

#  NC    NC+(B-Pseu)     NC+Pseu       NC+B        LP   LP+(B-Pseu)     LP+Pseu      LP+B 
# "b"         "d"         "c"         "d"         "a"         "d"         "c"         "c"  
