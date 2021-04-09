library("ggplot2")

Input <- read.table("sum_CPCoA.txt", sep="\t",header=T)

p <- ggplot(Input, aes(x=x, y=variation, group=x)) +
  geom_bar(stat="identity", position="stack", aes(fill=Compartment)) +
  scale_fill_manual(values=c("orange","gray","orange","gray","orange","gray"))+
  xlab("")+ylab("% variance explained by light condition")+
  geom_text(aes(label=p), position=position_stack(vjust=1))+
  #geom_text(aes(label=rate))+
  scale_x_discrete(limits=c("bac_R","bac_M","fun_R","fun_M","oom_R","oom_M"))+
  scale_y_continuous(breaks=seq(0, 10, by=5),limits=c(0, 15))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,hjust=0.5))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=1))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))

p

pdf("Figure_2B.pdf", width=6, height=4, useDingbats = F)
p
dev.off()

