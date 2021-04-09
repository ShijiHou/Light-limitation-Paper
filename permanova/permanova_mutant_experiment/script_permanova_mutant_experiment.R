library(phyloseq)
library(tidyr)
library(vegan)
#### Permanova Global Model ####
data=read.csv("mapping_with_canopy.txt", sep="")
otu=read.csv("otu_table_renamed_correct.txt", sep="")

data=data[which(!data$Compartment=="Input"),]
split_data=data %>% separate(Condition, c("light","genotype","compartment"), sep="_")

split_data=split_data[which(!split_data$genotype=="noplant"),]
split_data=split_data[which(!split_data$genotype=="nopant"),]

split_data=split_data[which(!split_data$light=="FR"),]

split_data=split_data[which(!split_data$genotype=="BRI1"),]

split_data=split_data[which(!split_data$genotype=="phyA"),]

split_data=split_data[which(!split_data$genotype=="phyB"),]

split_data=split_data[which(!split_data$genotype=="phyAB"),]

split_data=split_data[which(!split_data$genotype=="myc2flag"),]

split_data=split_data[which(!split_data$genotype=="col0fc"),]

split_data=split_data[which(!split_data$genotype=="myc234"),]

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]

split_data=droplevels(split_data)

samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )
GP1_filter=filter_taxa(GP1_norm, function(x) mean(x) > 0.001, TRUE)
dist.mat=vegdist(t(otu_table(GP1_filter)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_filter)$genotype*sample_data(GP1_filter)$compartment*sample_data(GP1_filter)$light)
mod

#### Permanova  Roots samples Model ####
data=read.csv("mapping_with_canopy.txt", sep="")
otu=read.csv("otu_table_renamed_correct.txt", sep="")

data=data[which(!data$Compartment=="Input"),]
split_data=data %>% separate(Condition, c("light","genotype","compartment"), sep="_")

split_data=split_data[which(!split_data$genotype=="noplant"),]
split_data=split_data[which(!split_data$genotype=="nopant"),]

split_data=split_data[which(!split_data$light=="FR"),]

split_data=split_data[which(!split_data$genotype=="BRI1"),]

split_data=split_data[which(!split_data$genotype=="phyA"),]

split_data=split_data[which(!split_data$genotype=="phyB"),]

split_data=split_data[which(!split_data$genotype=="phyAB"),]

split_data=split_data[which(!split_data$genotype=="myc2flag"),]

split_data=split_data[which(!split_data$genotype=="col0fc"),]

split_data=split_data[which(!split_data$genotype=="myc234"),]

split_data=split_data[which(split_data$Compartment=="Root"),]

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]

split_data=droplevels(split_data)

samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$genotype*sample_data(GP1_norm)$light)
mod

#### Permanova  Matrix samples Model ####
data=read.csv("mapping_with_canopy.txt", sep="")
otu=read.csv("otu_table_renamed_correct.txt", sep="")

data=data[which(!data$Compartment=="Input"),]
split_data=data %>% separate(Condition, c("light","genotype","compartment"), sep="_")

split_data=split_data[which(!split_data$genotype=="noplant"),]
split_data=split_data[which(!split_data$genotype=="nopant"),]

split_data=split_data[which(!split_data$light=="FR"),]

split_data=split_data[which(!split_data$genotype=="BRI1"),]

split_data=split_data[which(!split_data$genotype=="phyA"),]

split_data=split_data[which(!split_data$genotype=="phyB"),]

split_data=split_data[which(!split_data$genotype=="phyAB"),]

split_data=split_data[which(!split_data$genotype=="myc2flag"),]

split_data=split_data[which(!split_data$genotype=="col0fc"),]

split_data=split_data[which(!split_data$genotype=="myc234"),]

split_data=split_data[which(split_data$Compartment=="Matrix"),]

otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(split_data))]

split_data=droplevels(split_data)

samples = sample_data(split_data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )

dist.mat=vegdist(t(otu_table(GP1_norm)), method="bray")
mod=adonis(dist.mat~sample_data(GP1_norm)$genotype*sample_data(GP1_norm)$light)
mod