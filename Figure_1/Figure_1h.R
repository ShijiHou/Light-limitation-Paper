library("ggplot2")

cs <- read.table("cs_effsize_byM.txt", sep="\t",header=T)
sfw <- read.table("sfw_effsize_byM.txt", sep="\t",header=T)
pl <- read.table("pl_effsize_byM.txt", sep="\t",header=T)
shape <- read.table("shape_effsize_byM.txt", sep="\t",header=T)
ln <- read.table("leafnumber_effsize_byM.txt",sep="\t",header=T)

cs <- cs$cs
sfw <- sfw$sfw
pl <- pl$pl
shape <- shape$shape
ln <- ln$ln

tt <- rbind(cs,sfw,pl,shape,ln)
colnames(tt) <- c("NC","LP","FR")
rownames(tt) <- c("canopy size","shoot fresh weight","petiole length","leaf length/width","leaf number")


library(reshape2)
tt1 <- melt(tt)

pdf("figure_1h.pdf", width=5, height=4,useDingbats = F)

ggplot(tt1, aes(x=Var2, y=value, group=Var1)) +
  geom_line(aes(color=Var1))+
  geom_point()+
  xlab("")+ylab("The effect size by BFO")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

dev.off()
