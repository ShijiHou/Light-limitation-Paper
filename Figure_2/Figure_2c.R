###Partial least squares discriminant analysis (PLS-DA)###
###Projection to latent structures discriminant analysis (PLS-DA)###

library(RVAideMemoire)
library(pls)

################ bacteria ####################################################
otu_table.file <- paste("bac_otu_table_norm.txt", sep="")
otu_table <- read.table(otu_table.file, sep="\t", header=T, check.names=F)

design.file <- paste("bac_design.txt", sep="")
design <- read.table(design.file, header=T, sep="\t")

taxonomy.file <- paste("bac_taxonomy.txt", sep="")
taxonomy <- read.table(taxonomy.file, sep="\t", header=F, fill=T)

# re-order data matrices

idx <- match(design$SampleID, colnames(otu_table))
otu_table <- otu_table[, idx]

# subset samples of interest from distance matrices

excluded_samples <- design$SampleID[design$not_include=="yes"]
idx <- which(!design$SampleID %in% excluded_samples)

design <- design[idx, ]
otu_table <- otu_table[, idx]

write.table(design, "design_sub.txt", sep="\t", row=TRUE, quote=FALSE)

design <- read.table("design_sub.txt", header=T, sep="\t")

table <- otu_table
table=t(table)

##subset the otu table, remove Root12220 and Soil522 two strains since they are empty evule. It will give error if there is empty value.
table <- subset(table, select = -Root198D2)
table <- subset(table, select = -Root745)
table <- subset(table, select = -Root535)

table<-as.matrix(table)
typeof(table)

var.ind<-dummy(design$ConCom)

###Create the PLS-DA model###
PLSDA<-cppls(var.ind~table, ncomp=2)
###Actual test###
test=MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE,scale = TRUE)
test

###Cross validation###
result=MVA.cmv(table,design$ConCom,model="PPLS-DA",crit.inn="NMC")
result

#Permutation test based on cross model validation

#data:  table and design$ConCom
#Model: PPLS-DA
#8 components maximum
#999 permutations
#CER = 0.34681, p-value = 0.001

###Pair comparison###
pairwise=pairwise.MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE)
pairwise

#Pairwise comparisons using permutation tests based on cross model validation 

#data:  table and Y
#Model: PPLS-DA
#8 components maximum
#999 permutations 

#                 Normal.Microbes.Matrix Normal.Microbes.Root Shade.Microbes.Matrix
#Normal.Microbes.Root  0.0012                 -                    -                    
#  Shade.Microbes.Matrix 0.7970                 0.0012               -                    
#  Shade.Microbes.Root   0.0012                 0.0012               0.0012               

#P value adjustment method: fdr 


###Plotting ###
p1 <- MVA.plot(PLSDA,fac=design$ConCom)
#MVA.corplot(PLSDA,points = TRUE, keepmar = FALSE)

library("ggplot2")

#MVA.plot(PLSDA,"corr")
MVA.plot(PLSDA,"corr")
p2 <- MVA.plot(PLSDA,"corr",thresh=0.1)
print(p2)


#Error in if (sum(to.keep) == 0) { : missing value where TRUE/FALSE needed
#  In addition: Warning message:
#    In cor(X, sco, use = "pairwise") : Standardabweichung ist Null
#  > 

MVA.synt(PLSDA)
#Criterion: X total variance (%)
#Axis Proportion Cumulative
#1      28.52      28.52
#2       3.48      31.99

pchs <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
                     pch=c(2,1,17,16))
#cols <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
#                   col=c(1,1,"gray48","gray48"))

p3 <- MVA.scoreplot(PLSDA,fac=design$ConCom,  pch = pchs$pch,
                    cex = 3, points = TRUE, col = c(1,1,"gray48","gray48"),legend = F)
print(p3)


pdf("Figure_2c_bacteria.pdf", width=6, height=4, useDingbats = F)
p3
dev.off()


################# fungi ###############################################


library(RVAideMemoire)
library(pls)

otu_table.file <- paste("fun_otu_table_norm.txt", sep="")
otu_table <- read.table(otu_table.file, sep="\t", header=T, check.names=F)

design.file <- paste("fun_design.txt", sep="")
design <- read.table(design.file, header=T, sep="\t")

taxonomy.file <- paste("fun_taxonomy.txt", sep="")
taxonomy <- read.table(taxonomy.file, sep="\t", header=F, fill=T)

# re-order data matrices

idx <- match(design$SampleID, colnames(otu_table))
otu_table <- otu_table[, idx]


# subset samples of interest from distance matrices

excluded_samples <- design$SampleID[design$not_include=="yes"]
idx <- which(!design$SampleID %in% excluded_samples)

design <- design[idx, ]
otu_table <- otu_table[, idx]

write.table(design, "design_sub.txt", sep="\t", row=TRUE, quote=FALSE)

design <- read.table("design_sub.txt", header=T, sep="\t")

table <- otu_table
table=t(table)

table<-as.matrix(table)
typeof(table)

var.ind<-dummy(design$ConCom)

###Create the PLS-DA model###
PLSDA<-cppls(var.ind~table, ncomp=2)
###Actual test###
test=MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE,scale = TRUE)
test
#Permutation test based on cross model validation

#data:  table and design$ConCom
#Model: PPLS-DA
#8 components maximum
#999 permutations
#CER = 0.53333, p-value = 0.002


###Cross validation###
result=MVA.cmv(table,design$ConCom,model="PPLS-DA",crit.inn="NMC")
result

#Cross model validation (2CV)

#Model: PPLS-DA 
#Inner loop: 6-fold validation
#Outer loop: 7-fold validation
#Validation repeated 10 times
#70 submodels generated (1 to 8 components)

#Inner loop criterion: number of misclassifications

#Mean (standard error) classification error rate (%): 55.4 (1.6)


###Pair comparison###
pairwise=pairwise.MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE)
pairwise

#Pairwise comparisons using permutation tests based on cross model validation 

#data:  table and Y
#Model: PPLS-DA
#8 components maximum
#999 permutations 

#            Normal.Microbes.Matrix Normal.Microbes.Root Shade.Microbes.Matrix
#Normal.Microbes.Root  0.028                  -                    -                    
# Shade.Microbes.Matrix 0.232                  0.003                -                    
# Shade.Microbes.Root   0.010                  0.232                0.003                

#P value adjustment method: fdr


###Plotting ###
p1 <- MVA.plot(PLSDA,fac=design$ConCom)
#MVA.corplot(PLSDA,points = TRUE, keepmar = FALSE)

library("ggplot2")

print(p1)

#MVA.plot(PLSDA,"corr")
MVA.plot(PLSDA,"corr")
p2 <- MVA.plot(PLSDA,"corr",thresh=0.1)
print(p2)

MVA.synt(PLSDA)
#Criterion: Y cumulative total variance (%)

#Criterion: X total variance (%)
#Axis Proportion Cumulative
#1       8.78       8.78
#2       9.98      18.77

pchs <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
                   pch=c(2,1,17,16))
#cols <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
#                   col=c(1,1,"gray48","gray48"))

p3 <- MVA.scoreplot(PLSDA,fac=design$ConCom,  pch = pchs$pch,
                    cex =3, points = TRUE, col = c(1,1,"gray48","gray48"),legend = F)
print(p3)

pdf("Figure_2c_fungi.pdf", width=6, height=4, useDingbats = F)
p3
dev.off()

################# oomycetes ####################################
library(RVAideMemoire)
library(pls)

design <- read.table("oom_design.txt", header=T, sep="\t")
table <- read.table("oom_otu.txt", header=T, sep="\t")

table<-as.matrix(table)
typeof(table)


#table <- cbind(table,table)
colnames(table) <- c("O210","O229","O132","O029")

var.ind<-dummy(design$ConCom)

###Create the PLS-DA model###
PLSDA<-cppls(var.ind~table, ncomp=2)
###Actual test###
test=MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE,scale = TRUE)
test
###Cross validation###
result=MVA.cmv(table,design$ConCom,model="PPLS-DA",crit.inn="NMC")
result

###Pair comparison###
pairwise=pairwise.MVA.test(table,design$ConCom,model="PPLS-DA",cmv=TRUE)
pairwise


###Plotting ###
p1 <- MVA.plot(PLSDA,fac=design$ConCom)
#MVA.corplot(PLSDA,points = TRUE, keepmar = FALSE)


library("ggplot2")
library("export")

print(p1)
graph2ppt(file="PLSDA.pptx", width=6, height=6)

#MVA.plot(PLSDA,"corr")
MVA.plot(PLSDA,"corr")
p2 <- MVA.plot(PLSDA,"corr",thresh=0.1)
print(p2)
graph2ppt(file="PLSDACOR.pptx", width=6, height=6)



MVA.synt(PLSDA)
#Criterion: Y cumulative total variance (%)

#Criterion: X total variance (%)
#Axis Proportion Cumulative
#1      27.04      27.04
#2       8.13      35.17

pchs <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
                   pch=c(2,1,17,16))
#cols <- data.frame(group=c("Normal.Microbes.Matrix", "Normal.Microbes.Root", "Shade.Microbes.Matrix", "Shade.Microbes.Root" ),
#                   col=c(1,1,"gray48","gray48"))

p3 <- MVA.scoreplot(PLSDA,fac=design$ConCom,  pch = pchs$pch,
                    cex = 3, points = TRUE, col = c(1,1,"gray48","gray48"),legend = F)
print(p3)

pdf("Figure_2c_oomycetes.pdf", width=6, height=4, useDingbats = F)
p3
dev.off()