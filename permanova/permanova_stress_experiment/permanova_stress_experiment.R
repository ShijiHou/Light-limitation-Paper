library(vegan)
library(phyloseq)

###################
######Bacteria#####
###################

###Permanova Global model####
data=read.csv("bac/all_bac_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("bac/otu_table_0995.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Compartment*sample_data(GP1_norm)$Condition)
mod


###Permanova based on roots samples####
data=read.csv("bac/all_bac_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("bac/otu_table_0995.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Root"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod

###Permanova based on matrix samples####
data=read.csv("bac/all_bac_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("bac/otu_table_0995.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Matrix"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod


###################
#####Fungi#########
###################

###Permanova Global model####
data=read.csv("fun/all_fun_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("fun/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Compartment*sample_data(GP1_norm)$Condition)
mod

###Permanova based on roots samples####
data=read.csv("fun/all_fun_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("fun/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Root"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod

###Permanova based on matrix samples####
data=read.csv("fun/all_fun_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("fun/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Matrix"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod


###################
#####Oomycetes#####
###################


###Permanova Global model####
data=read.csv("oomyc/all_oo_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("oomyc/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Compartment*sample_data(GP1_norm)$Condition)
mod

###Permanova based on roots samples####
data=read.csv("oomyc/all_oo_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("oomyc/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Root"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod

###Permanova based on matrix samples####
data=read.csv("oomyc/all_oo_map.txt", sep="", h=T, row.names = 1,check.names = FALSE)
otu=read.csv("oomyc/otu_table.txt", sep="",check.names = FALSE)

otu_test=otu[,which(!colnames(otu)%in%rownames(data))]
data_test=data[which(!rownames(data)%in%colnames(otu)),]

split_data=data[which(!data$Condition=="Salt.Microbes"),]
split_data=split_data[which(!split_data$Condition=="PEG.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Pst.Microbes"),]
split_data=split_data[which(!split_data$Condition=="Input"),]
split_data=split_data[which(split_data$Compartment=="Matrix"),]
split_data=droplevels(split_data)

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]


samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$Condition)
mod