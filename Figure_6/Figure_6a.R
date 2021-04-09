library("ggplot2")
library("DOSE")
library("cowplot")

cs <- read.table("cs_FR_col0_myc2_myc2flag.txt", sep="\t",header=T)

cs$lab <- factor(cs$lab, levels = levels(cs$lab)[c(3:6,1,2)], ordered = T)


p1<-ggplot(cs,aes(x=lab, y=y, group=lab, fill=light))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Canopy sizes (cm2)")+
  #ggtitle("The original data")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NC+M","SC","SC+M","FR","FR+M"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=10,hjust=0.5))+
  theme(axis.title.y = element_text(size=10))+
  theme(axis.title.x=element_text(size=10))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=10))+
  theme(legend.text=element_text(size=10))+
  theme(legend.title=element_text(size=10))+ 
  theme(legend.position = "none")+
  facet_wrap(~geno,nrow=1)

p1

pdf("Figure_6a.pdf",width =6,height = 5,useDingbats = F)
p1
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
 for (g in unique(cs$geno)){
   x <- subset(cs, geno == g, sel = c("y"))
   y <- shapiro.test(x[,1])
   print(paste("p-value for", g, ":", y[2]))
 }

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

#[1] "p-value for col0 : 0.622347828143374"
#[1] "p-value for myc2 : 0.00014180253054962"
#[1] "p-value for myc2flag : 0.101321656926096"

library(dunn.test)

for (g in unique(cs$geno)){
  x <- subset(cs, geno ==g)
  print(g)
  kruskal.test(x[["y"]],x[["lab"]])
  dunn.test(x[["y"]],x[["lab"]], wrap=TRUE, method="bonferroni")
}

## Kruskal-Wallis test and Dunn test

### col0 ####
Input1 <- subset(cs,geno=="col0")
library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab"]])
dt <- dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#1    FR      b          b
#2  FR+M      a         a 
#3    NC      a         a 
#4  NC+M      a         a 
#5    SC      b          b
#6  SC+M      a         a 
# a a b a b a 

### myc2 ####
Input1 <- subset(cs,geno=="myc2")
library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab"]])
dt <- dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

#Group Letter MonoLetter
#1    FR      b          b
#2  FR+M      b          b
#3    NC      a         a 
#4  NC+M      a         a 
#5    SC      b          b
#6  SC+M      b          b
# a a b b b b

### myc2flag ####
Input1 <- subset(cs,geno=="myc2flag")
library(dunn.test)
kruskal.test(Input1[["y"]],Input1[["lab"]])
dt <- dunn.test(Input1[["y"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)
#Group Letter MonoLetter
#1    FR      b          b
#2  FR+M      a         a 
#3    NC      a         a 
#4  NC+M      a         a 
#5    SC      b          b
#6  SC+M      a         a 

# a a b a b a 







