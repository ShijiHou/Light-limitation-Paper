library("ggplot2")

Input1 <- read.table("SFW_col0_withFR.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,3,4,1,2)], ordered = T)

SFW_col0 <- ggplot(Input1,aes(x=lab, y=shootfreshweight, group=lab, fill=light))+
  geom_boxplot(outlier.shape=NA, aes(fill=lab))+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Shoot Fresh Weight/g")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NC+M","LP","LP+M","FR","FR+M"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(0.02, 0.18, by=0.04),limits=c(0,0.186))+
  guides(fill = guide_legend(title = "Light Condition", title.position = "top"))+
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


print(SFW_col0)

pdf("Figure_1c.pdf", width=4, height=5,useDingbats = F)
SFW_col0
dev.off()


########################################################################################
##statistics############################################################################
########################################################################################

#Test for nomal distribution using Shapiro-Wilk test
 for (g in unique(Input1$lab)){
   x <- subset(Input1, lab == g, sel = c("shootfreshweight"))
   y <- shapiro.test(x[,1])
   print(paste("p-value for", g, ":", y[2]))
 }

#The p-value has to be >0.05 (not less!) to accept H0 that the data are normal distributed.
#You can use Shapiro-Wilk test for sample size 3 up to 5000.

#[1] "p-value for NC : 0.000362464784421061"
#[1] "p-value for LP : 0.986365946715914"
#[1] "p-value for NC+M : 0.0725638525003056"
#[1] "p-value for LP+M : 0.539401807498957"
#[1] "p-value for FR : 0.69762457109735"
#[1] "p-value for FR+M : 0.494223896871091"


#Test for variance homogeneity
library(lawstat)
levene.test(Input1$shootfreshweight,Input1$lab)

#The p-value has to be >0.05 (not less!) to accept H0 that variances are equal for all groups

#Modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
#data:  Input1$shootfreshweight
#Test Statistic = 10.419, p-value = 1.221e-08


## Kruskal-Wallis test and Dunn test

library(dunn.test)
kruskal.test(Input1[["shootfreshweight"]],Input1[["lab"]])
dunn.test(Input1[["shootfreshweight"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")

#data: x and group
#Kruskal-Wallis chi-squared = 76.419, df = 5, p-value = 0


#Comparison of x by group                            
#(Bonferroni)                                  
#  Col Mean-|
#  Row Mean |         FR       FR+M         LP       LP+M         NC
#  ---------+-------------------------------------------------------
#      FR+M |  -4.331248
#           |    0.0001*
#           |
#        LP |  -0.707075   4.771568
#           |     1.0000    0.0000*
#           |
#      LP+M |  -3.994579   1.484064  -5.198000
#           |    0.0005*     1.0000    0.0000*
#           |
#        NC |  -3.748294   1.730348  -4.808590   0.389410
#           |    0.0013*     0.6268    0.0000*     1.0000
#           |
#      NC+M |  -5.295916   0.182727  -7.255594  -2.057594  -2.447004
#           |    0.0000*     1.0000    0.0000*     0.2972     0.1080


### compact letter display
library(rcompanion)
dt <- dunn.test(Input1[["shootfreshweight"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

#Group Letter MonoLetter
#1    FR      b          b
#2  FR+M      a         a 
#3    LP      b          b
#4  LP+M      a         a 
#5    NC      a         a 
#6  NC+M      a         a 

## a a b a b a


### calculate the effect size of BFO on shoot fresh weight under NC, LP, and EODFR
library(effsize)
nc <- subset(Input1,lab=="NC")
ncm <- subset(Input1,lab=="NC+M")
lp <- subset(Input1, lab=="LP")
lpm <- subset(Input1,lab=="LP+M")
fr <- subset(Input1,lab=="FR")
frm <- subset(Input1,lab=="FR+M")

NC <- cohen.d(ncm$shootfreshweight,nc$shootfreshweight)
LP <- cohen.d(lpm$shootfreshweight,lp$shootfreshweight)
FR <- cohen.d(frm$shootfreshweight,fr$shootfreshweight)

sfw <- c(NC$estimate,LP$estimate,FR$estimate)
sfw <- as.data.frame(sfw)
rownames(sfw) <- c("NC","LP","FR")
write.table(sfw, "sfw_effsize_byM.txt", sep="\t", quote=FALSE) 




