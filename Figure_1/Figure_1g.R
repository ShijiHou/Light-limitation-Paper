library("ggplot2")

Input1 <- read.table("leaf_ratio_col0_withFR.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(3,4,5,6,1,2)], ordered = T)

ratio_col0 <- ggplot(Input1,aes(x=lab, y=y, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Leaf length/width")+
  #ggtitle("Sum of petiole length by plant")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NCM","SC","SCM","FR","FRM"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  #guides(fill = guide_legend(title = "Leaf length/width", title.position = "top"))+
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

print(ratio_col0)


pdf("Figure_1g.pdf", width=4, height=5, useDingbats = F)
ratio_col0
dev.off()

##add statistics letters on the boxplot

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input1$lab)){
  x <- subset(Input1, lab == g, sel = c("y"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}


#[1] "p-value for SCM : 0.139177422450912"
#[1] "p-value for NCM : 0.026447556519955"
#[1] "p-value for SC : 0.0748718894435181"
#[1] "p-value for NC : 0.0655801650786734"
#[1] "p-value for FRM : 2.73419778095303e-14"
#[1] "p-value for FR : 0.674418435406963"

#Test for variance homogeneity
library(lawstat)
levene.test(Input1$y,Input1$lab)

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$y
#Test Statistic = 17.342, p-value = 8.424e-16

library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab"]])
dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")


### compact letter display
library(rcompanion)
dt <-dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
# Group Letter MonoLetter
#1    FR     de         de
#2   FRM      c        c  
#3    NC      e          e
#4   NCM      b       b   
#5    SC      a      a    
#6   SCM      d         d

### e b a d de c 

#### calculate the effect size of BFO on leaf length/width under NC, LP, and EODFR
library(effsize)
nc <- subset(Input1,lab=="NC")
ncm <- subset(Input1,lab=="NCM")
lp <- subset(Input1, lab=="SC")
lpm <- subset(Input1,lab=="SCM")
fr <- subset(Input1,lab=="FR")
frm <- subset(Input1,lab=="FRM")

NC <- cohen.d(ncm$y,nc$y)
LP <- cohen.d(lpm$y,lp$y)
FR <- cohen.d(frm$y,fr$y)

shape <- c(NC$estimate,LP$estimate,FR$estimate)
shape <- as.data.frame(shape)
rownames(shape) <- c("NC","LP","FR")
write.table(shape, "shape_effsize_byM.txt", sep="\t", quote=FALSE) 

