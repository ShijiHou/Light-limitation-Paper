library(phyloseq)
library(dplyr)
library(ggplot2)
data=read.csv("filtered_dataset_corrected_canopy.txt", sep="", h=T)
otu=read.csv("otu_table.txt", sep="")

#### Filter the dataset, aggregate the OTU read counts per mutant ####
rownames(data)=data$names
otu_filtered_canopy=otu[,which(colnames(otu)%in%row.names(data))]
samples = sample_data(data)
OTU = otu_table(otu_filtered_canopy, taxa_are_rows = TRUE)
GP1 <- phyloseq(OTU,samples)
GP1_norm  = transform_sample_counts(GP1, function(x) x / sum(x) )
GP1_filter=filter_taxa(GP1_norm, function(x) mean(x) > 0.001, TRUE)
GP1_filter_merged_mutants=merge_samples(GP1_filter,"Condition")
GP1_filter_merged_mutants= transform_sample_counts(GP1_filter_merged_mutants, function(x) x / sum(x) )

#### Make the PCOA and extract coordinates of mutants on axes####
GP.ord <- ordinate(GP1_filter_merged_mutants, "MDS", "bray")
p1 = plot_ordination(GP1_filter_merged_mutants, GP.ord, type="samples",axes = c(1,2))
p1
x=GP.ord$vectors
x=as.matrix(x)
x=x[,1]
x=as.data.frame(x)
x$names<-rownames(x)
y=GP.ord$vectors
y=as.matrix(y)
y=y[,2]
y=as.data.frame(y)
y$names<-rownames(y)
z=GP.ord$vectors
z=as.matrix(z)
z=z[,3]
z=as.data.frame(z)
z$names<-rownames(z)

####Calculate canopy size increase between LP-BFo and LP+BFO####
nomic=aggregate(data$canop_size_no_mic, list(data$genotype), mean)
colnames(nomic)[1]<-"genotype"
joined_data=left_join(data,nomic, by="genotype" )
joined_data$corrected_canopy<-(joined_data$canopy_size/joined_data$x)
mean_corrected_canopy=aggregate(joined_data$corrected_canopy, list(joined_data$genotype), mean)
mean_corrected_canopy$names<-x$names
colnames(mean_corrected_canopy)[2]<-"corrected_canopy_size"
####Join the datasets####
data=inner_join(mean_corrected_canopy, x, by="names")
data=inner_join(data, y, by="names")
data=inner_join(data, z, by="names")



####Make the regressions per axis of the####
mod=lm(corrected_canopy_size~x, data=data)
qqnorm(residuals(mod))
qqline(residuals(mod))
shapiro.test(residuals(mod))
anova(mod)
summary(mod)

p <- ggplot(data, aes(x=x, y=corrected_canopy_size, label=Group.1)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="#69b3a2", se=TRUE)
p+geom_text()

mod=lm(corrected_canopy_size~y, data=data)
qqnorm(residuals(mod))
qqline(residuals(mod))
shapiro.test(residuals(mod))
anova(mod)
summary(mod)

mod=lm(corrected_canopy_size~z, data=data)
qqnorm(residuals(mod))
qqline(residuals(mod))
shapiro.test(residuals(mod))
anova(mod)
summary(mod)

