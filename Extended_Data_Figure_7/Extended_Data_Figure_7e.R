# Written by Fantin Mesny (mesny@mpipz.mpg.de) - October 2020 

library(ape)
library(cluster)
library(ggplot2)
library(ggtree)
library(vegan)
library(dplyr)
library(ggnewscale)

## PARSING
variantToStrains<-read.csv('variant_to_strains.csv')
phylo<-read.csv('strainPhylo.tsv',sep='\t') 
l37<- read.csv('svm_output.csv')
l37$svm_coeff<-abs(l37$svm_coeff)
l37<-merge(l37, phylo, by = "names",all.x = TRUE)
l37<-merge(l37,variantToStrains,by="names",all.x=TRUE)
rownames(l37)<-l37$strains
tree<-read.tree('37Strains_16s_Mafft-RAxML.TREE')

## FIGURE
g1<-gheatmap(ggtree(tree)+ geom_tiplab(hjust=-0.05,size=2,align=TRUE), l37 %>% select(O), offset=0.75, width=.1, colnames_angle=90, colnames_offset_y = .25)
g2 <- g1 + new_scale_fill()
pdf('SVM_figure.pdf')
gheatmap(g2, l37 %>% select(svm_coeff), offset=.8, width=.1, colnames_angle=90, colnames_offset_y = .25,low="white",high="black")
dev.off()

