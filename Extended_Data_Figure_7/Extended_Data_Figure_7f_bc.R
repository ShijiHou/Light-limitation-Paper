library("ggplot2")

Input <- read.table("bc_pseu.txt", sep="\t",header=T)
Input$lab <- factor(Input$lab, levels = levels(Input$lab)[c(5,6,8,7,1,2,4,3)], ordered = T)

p <- ggplot(Input,aes(x=lab, y=y, group=lab))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","white","white","gray48","gray48","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab(" ")+ylab("lg(BcCutA/AtSKII)")+
  #ggtitle("Leaf fungal pathogen growth")+
  scale_x_discrete(breaks=c("NC","NC+(B-P)","NC+P","NC+B","LP","LP+(B-P)","LP+P","LP+B"),
                   labels=c("-BFO","+(B-Pseu)","+Pseu","+B","-BFO","+(B-Pseu)","+Pseu","+B"))+
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

pdf("bc_pseu.pdf", width=4.5, height=4,useDingbats = F)
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
# [1] "p-value for NC : 0.441647083224441"
# [1] "p-value for NC+(B-P) : 0.525893038366876"
# [1] "p-value for NC+P : 0.992945821430607"
# [1] "p-value for NC+B : 0.121007878168057"
# [1] "p-value for LP : 0.204923309918948"
# [1] "p-value for LP+(B-P) : 0.599146753063039"
# [1] "p-value for LP+P : 0.450464077103428"
# [1] "p-value for LP+B : 0.615274800318646"

#Test for variance homogeneity
library(lawstat)
levene.test(Input$y,Input$lab)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median

# data:  Input$y
# Test Statistic = 1.8816, p-value = 0.0872


result <- aov(Input[["y"]] ~ Input[["lab"]])
summary(result)

# Df Sum Sq Mean Sq F value Pr(>F)    
# Input[["lab"]]  7  39.88   5.698     119 <2e-16 ***
#   Residuals      64   3.06   0.048                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# ### compact letter display

library(multcomp)
result <- aov(y ~ lab, Input)
dt <- glht(result, linfct=mcp(lab="Tukey"))
cld(dt,alpha=0.05,decreasing = T)

# NC    NC+(B-P)    NC+P     NC+B     LP    LP+(B-P)    LP+P    LP+B 
# "b"      "e"      "c"      "e"      "a"      "e"      "c"      "d" 
