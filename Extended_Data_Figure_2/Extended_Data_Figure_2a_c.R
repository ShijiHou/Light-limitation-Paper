library(ggplot2)
library(ggparty)

Input <- read.table("light_condition.txt", sep="\t",header=T)

Input$condition <- factor(Input$condition, levels = levels(Input$condition)[c(2,1)], ordered = T)

light <- Input[,c(1,8,12,9,10,4,5)]

temperature <- read.table("temperature.txt",sep = "\t", header = T)
temperature$condition <- factor(temperature$condition, levels = levels(temperature$condition)[c(2,1)], ordered = T)

library(reshape)

light1 <- melt(light)

p1 <- ggplot(light1,aes(x=condition, y=value, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  ylab("")+
  scale_x_discrete(limits=c("NC","LP"))+
  scale_y_continuous(breaks=seq(0, 80, by=20),limits=c(0,80))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")+
  facet_wrap(~variable,nrow=1)


rfar <- ggplot(Input,aes(x=condition, y=PFD_RtoFR, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  ylab("PFD-R:PFD-FR")+
  scale_x_discrete(limits=c("NC","LP"))+
  scale_y_continuous(breaks=seq(0, 10, by=2),limits=c(0, 10))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

rpv <- ggplot(Input,aes(x=condition, y=rpV, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  ylab("rpv")+
  scale_x_discrete(limits=c("NC","LP"))+
  scale_y_continuous(breaks=seq(110, 335, by=50),limits=c(110, 335))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

tem <- ggplot(temperature,aes(x=condition, y=temperature, group=condition, fill=condition))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  ylab("Temperature")+
  scale_x_discrete(limits=c("NC","LP"))+
  scale_y_continuous(breaks=seq(15, 25, by=5),limits=c(15, 25))+
  theme_bw()+
  theme(plot.title=element_text(size=12))+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

p <- cowplot::plot_grid(rfar, rpv, tem, ncol=3)

pdf("Extended_Data_Figure_2a.pdf",width = 6, height = 4,useDingbats = F)
p1
dev.off()

pdf("Extended_Data_Figure_2c.pdf",width = 5, height = 2,useDingbats = F)
p
dev.off()
#############################################################################################################
###########################statistics########################################################################
#############################################################################################################


####PPFD###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PPFD"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}


####PFD###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####PFD-B###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD_B"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####PFD-G###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD_G"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####PFD-R###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD_R"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####PFD-FR###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD_FR"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####PFD-R:FR###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("PFD_RtoFR"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####rpv###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input$condition)){
  x <- subset(Input, condition == g, sel = c("rpV"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

####temperature###
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(temperature$condition)){
  x <- subset(temperature, condition == g, sel = c("temperature"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(Input[["PPFD"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value < 2.2e-16

wilcox.test(Input[["PFD"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value < 2.2e-16

wilcox.test(Input[["PFD_B"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value = 3.016e-11

wilcox.test(Input[["PFD_G"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value = 3.012e-11

wilcox.test(Input[["PFD_R"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value < 2.2e-16

wilcox.test(Input[["PFD_FR"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value = 3.018e-11

wilcox.test(Input[["rpV"]] ~ Input[["condition"]], paired = FALSE)
#W = 900, p-value = 3.018e-11

#Independent two-sample t-test (n=2, unpaired)
t.test(Input[["PFD_RtoFR"]] ~ Input[["condition"]], paired = FALSE, var.equal = TRUE)
#t = 2.4553, df = 58, p-value = 0.0171

#Mann-Whitney-U-test (n=2, unpaired)
wilcox.test(temperature[["temperature"]] ~ temperature[["condition"]], paired = FALSE)
#W = 277.5, p-value = 0.03289
