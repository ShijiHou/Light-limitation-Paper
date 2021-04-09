library("ggplot2")

Input1 <- read.table("myc2_myc2flag_BC.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(3,4,1,2)], ordered = T)
Input1$condition <- factor(Input1$condition, levels = levels(Input1$condition)[c(
  7,8,5,6,3,4,1,2
)], ordered = T)


BCall <- ggplot(Input1,aes(x=condition, y=y, group=condition, fill=light))+
  #geom_rect(data=NULL,aes(xmin=0.25,xmax=8.5,ymin=-Inf,ymax=Inf),
        #   fill="snow2")+
  #geom_rect(data=NULL,aes(xmin=16.55,xmax=24.55,ymin=-Inf,ymax=Inf),
        #   fill="snow2")+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  #geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  xlab("")+ylab("lg(BcCutA/AtSKII)")+
  scale_x_discrete(breaks=c("myc2flag_NC","myc2flag_NC_M","myc2flag_LP","myc2flag_LP_M","myc2_NC","myc2_NC_M","myc2_LP","myc2_LP_M"),
                   labels=c("-BFO", "+BFO","-BFO", "+BFO","-BFO", "+BFO","-BFO", "+BFO"))+
  scale_y_continuous(breaks=seq(-3, 0.5, by=0.5),limits=c(-3, 0.5))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
  #facet_wrap(~geno,nrow=1)


print(BCall)


pdf("myc2_myc2flag_BC.pdf", width=4, height=5,useDingbats = F)
BCall
dev.off()

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input1$condition)){
  x <- subset(Input1, condition == g, sel = c("y1"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$condition)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#
#data:  Input1$y
#Test Statistic = 2.6267, p-value = 2.08e-07

result <- aov(Input1[["y"]] ~ Input1[["condition"]])
summary(result)



TukeyHSD(result)


library("ggplot2")

Input1 <- read.table("pto_myc2_myc2flag.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(3,4,1,2)], ordered = T)
Input1$condition <- factor(Input1$condition, levels = levels(Input1$condition)[c(
  7,8,5,6,3,4,1,2
)], ordered = T)


pto <- ggplot(Input1,aes(x=condition, y=y, group=condition, fill=light))+
  #geom_rect(data=NULL,aes(xmin=0.25,xmax=8.5,ymin=-Inf,ymax=Inf),
  #   fill="snow2")+
  #geom_rect(data=NULL,aes(xmin=16.55,xmax=24.55,ymin=-Inf,ymax=Inf),
  #   fill="snow2")+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.2), size=0.6)+
  #geom_hline(yintercept=0, linetype="dashed",color="red",size=0.5)+
  xlab("")+ylab("lg(CFU/g)")+
  scale_x_discrete(breaks=c("myc2flag_NC","myc2flag_NC+M","myc2flag_LP","myc2flag_LP+M","myc2_NC","myc2_NC+M","myc2_LP","myc2_LP+M"),
                   labels=c("-BFO", "+BFO","-BFO", "+BFO","-BFO", "+BFO","-BFO", "+BFO"))+
  scale_y_continuous(breaks=seq(5, 8, by=0.5),limits=c(5, 8))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=15))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+ 
  theme(legend.position = "none")
#facet_wrap(~geno,nrow=1)


print(pto)


pdf("myc2_myc2flag_pto.pdf", width=4, height=5,useDingbats = F)
pto
dev.off()

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input1$condition)){
  x <- subset(Input1, condition == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$condition)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#
#data:  Input1$y
#Test Statistic = 2.6267, p-value = 2.08e-07

result <- aov(Input1[["y"]] ~ Input1[["condition"]])
summary(result)



TukeyHSD(result)


