library("ggplot2")

Input1 <- read.table("leaf_number_col0_withFR.txt", sep="\t",header=T)
Input1$lab <- factor(Input1$lab, levels = levels(Input1$lab)[c(5,6,3,4,1,2)], ordered = T)

p <- ggplot(Input1,aes(x=lab, y=leaf.number, group=lab, fill=lab))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(values=c("white","white","gray48","gray48","darkred","darkred"))+
  geom_jitter(position=position_jitter(width=0.2), size=1)+
  #geom_hline(aes(yintercept=median(shootfreshweight), group="col0"), color="black",size=2)+
  xlab("")+ylab("Leaf number")+
  #ggtitle("Sum of petiole length by plant")+
  #scale_x_discrete(limits=c("NC","NC_M","SC","SC_M","FR","FR_M"))+
  scale_x_discrete(breaks=c("NC","NCM","LP","LPM","FR","FRM"),labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  scale_y_continuous(breaks=seq(8, 18, by=2),limits=c(7,18))+
  #guides(fill = guide_legend(title = "Leaf length/width", title.position = "top"))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,hjust=0.5))+
  theme(axis.text.y = element_text(size=12,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(p)


pdf("Figure_1f.pdf", width=4, height=5, useDingbats = F)
p
dev.off()

##add statistics letters on the boxplot

#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(Input1$lab)){
  x <- subset(Input1, lab == g, sel = c("leaf.number"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}

#[1] "p-value for NCM : 0.00167345561917942"
#[1] "p-value for NC : 0.000363833771429181"
#[1] "p-value for LP : 0.000176818060491353"
#[1] "p-value for LPM : 0.00542667389160233"
#[1] "p-value for FR : 0.248496220931045"
#[1] "p-value for FRM : 0.407395639115932

library(dunn.test)
kruskal.test(Input1[["leaf.number"]],Input1[["lab"]])
dt <- dunn.test(Input1[["leaf.number"]],Input1[["lab"]], wrap=TRUE, method="bonferroni")

# Kruskal-Wallis rank sum test

# data: x and group
# Kruskal-Wallis chi-squared = 105.7868, df = 5, p-value = 0


### compact letter display
library(rcompanion)
cldList(P.adjusted~comparisons,data=dt,threshold = 0.05,reversed=T)

# Group Letter MonoLetter
#1    FR     cd         cd
#2   FRM     ab       ab  
#3    LP      d          d
#4   LPM      b        b  
#5    NC     bc        bc 
#6   NCM      a       a      

### bc a d b cd ab

### calculated the effect size of BFO on leaf numbers under NC, LP, and EODFR
library(effsize)
nc <- subset(Input1,lab=="NC")
ncm <- subset(Input1,lab=="NCM")
lp <- subset(Input1, lab=="LP")
lpm <- subset(Input1,lab=="LPM")
fr <- subset(Input1,lab=="FR")
frm <- subset(Input1,lab=="FRM")

NC <- cohen.d(ncm$leaf.number,nc$leaf.number)
LP <- cohen.d(lpm$leaf.number,lp$leaf.number)
FR <- cohen.d(frm$leaf.number,fr$leaf.number)

ln <- c(NC$estimate,LP$estimate,FR$estimate)
ln <- as.data.frame(ln)
rownames(ln) <- c("NC","LP","FR")
write.table(ln, "leafnumber_effsize_byM.txt", sep="\t", quote=FALSE) 


