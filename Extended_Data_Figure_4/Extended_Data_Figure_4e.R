library("ggplot2")

Input <- read.table("sample_size.txt", sep="\t",header=T)
Input$lab <- factor(Input$lab, levels = levels(Input$lab)[c(1,3,5,2,4)], ordered = T)


p <- ggplot(Input, aes(x=lab, y=average_reads, group=lab)) +
  geom_bar(stat="identity", position="stack") +
  xlab("")+ylab("Sequnce reads after filer")+
  #geom_text(aes(label=p), position=position_stack(vjust=1))+
  #geom_text(aes(label=rate))+
  scale_x_discrete(breaks=c( "H2O","leaf_germfree","root_germfree","leaf_BFO","root_BFO"),
                   labels=c("H2O","Leaf-BFO","Root-BFO","Leaf+BFO","Root+BFO"))+
  scale_y_continuous(breaks=seq(0, 60000, by=10000),limits=c(0, 64000))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=12,hjust=1))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))

p

pdf("sample_size.pdf", width=5, height=5, useDingbats = F)
p
dev.off()

