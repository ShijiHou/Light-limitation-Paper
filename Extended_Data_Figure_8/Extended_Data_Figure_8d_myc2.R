####################################################################################################
## Include required packages
####################################################################################################

library(limma)
library(edgeR)
library(affy)
library(gplots)
library(DESeq)


####################################################################################################
## Read and pre-process count table (with number of reads per gene per lane)
####################################################################################################

## Read description matrix and extract factors for design matrix
des.mat <- readTargets("description_matrix_4340.txt")
group <- as.factor(des.mat$Group)
geno <- as.factor(des.mat$Condition)
treat <- as.factor(des.mat$Microbes_Innoculation)
rep <- as.factor(des.mat$Replicate)


## Read count tables (htseq-count) and extract relevant read count values
dat_tab <- read.table("CountTable_4340_raw.txt",sep="\t", as.is=TRUE, header=TRUE)
dat_sum <- as.matrix(dat_tab[1:(dim(dat_tab)[1]-5), ])


## Calculate normalization factors (TMM-normalization)
nf<-calcNormFactors(dat_sum)

## Create design matrix and fit linear model
design <- model.matrix(~0 + group)
colnames(design) <- levels(group)

## Transform count data to log2-cpm
y <- voom(dat_sum, design, lib.size=colSums(dat_sum)*nf)


####################################################################################################
### Create Cluster Dendrogram
####################################################################################################

## -------------------------------------------------------------------------------------------------
## Define correlation to use as distance measure:
## ------------------------------------------------------------------------------------------------- 

distSpearman <- function(x){mydist<-as.dist(1-cor(t(x), meth="spearman"))}


## -------------------------------------------------------------------------------------------------
## Cluster samples, using log2cpm:
## -------------------------------------------------------------------------------------------------

## Transfer cpm_table to matrix and transpose
cpm_matrix <- t(as.matrix(y$E))

## Make cluster dendrogramm
dist_cpm <- distSpearman(cpm_matrix)
clust_cpm <- hclust(dist_cpm)

pdf("ClustDendro.pdf", height=10, width=30)
plot(clust_cpm)
dev.off()


####################################################################################################
### Create heatmap of sample vs sample distances
####################################################################################################

## -------------------------------------------------------------------------------------------------
## Calculate sample vs sample distances based on DESeq variance stabilized counts
## with dispersions estimated "blind" i.e. unbiased by experimental design
## ------------------------------------------------------------------------------------------------- 

## Create DESeq Count set 
cdsFull <- newCountDataSet(dat_sum, design)
cdsFull <- estimateSizeFactors(cdsFull)
## Estimate dispersions "blind" i.e. unbiased by experimental design 
cdsFullBlind <- estimateDispersions(cdsFull, method = "blind")
## Perform variance stabilizing transformation
vsdFull <- varianceStabilizingTransformation(cdsFullBlind)

## Create heatmap of sample vs sample distances
dist3 <- dist(t(exprs(vsdFull)))

pdf("Extended_Data_Figure_8a_myc2.pdf", width=10, height=10)
heatmap.2(as.matrix(dist3), trace="none", margin=c(10, 10))
dev.off()

