library(vegan)
library(tidyr)

####Permanova Global Model####
metadata=read.table("description_matrix_3470.txt", h=T, row.names = 2)
reads=read.table("log2cpm_col0.txt")

metadata=separate(data = metadata, col = Group, into = c("Light", "Compartment","Microbes"), sep = "_")
reads=reads[,which(colnames(reads)%in%rownames(metadata))]

dist.mat=vegdist(t(reads), method="euclidean")
mod=adonis(dist.mat~metadata$Compartment*metadata$Microbes*metadata$Light)
mod

####Permanova based on roots samples####
metadata=read.table("description_matrix_3470.txt", h=T, row.names = 2)
reads=read.table("log2cpm_col0.txt")

metadata=separate(data = metadata, col = Group, into = c("Light", "Compartment","Microbes"), sep = "_")
metadata=metadata[which(metadata$Compartment=="Root"),]
reads=reads[,which(colnames(reads)%in%rownames(metadata))]

dist.mat=vegdist(t(reads), method="euclidean")
mod=adonis(dist.mat~metadata$Microbes*metadata$Light)
mod

####Permanova based on shoots samples####
metadata=read.table("description_matrix_3470.txt", h=T, row.names = 2)
reads=read.table("log2cpm_col0.txt")

metadata=separate(data = metadata, col = Group, into = c("Light", "Compartment","Microbes"), sep = "_")
metadata=metadata[which(metadata$Compartment=="Shoot"),]
reads=reads[,which(colnames(reads)%in%rownames(metadata))]

dist.mat=vegdist(t(reads), method="euclidean")
mod=adonis(dist.mat~metadata$Microbes*metadata$Light)
mod
