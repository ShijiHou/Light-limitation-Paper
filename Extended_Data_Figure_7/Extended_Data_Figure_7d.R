#############PLS-DA all strains###############
library(RVAideMemoire)
library(pls)
library(dummies)
data=read.table("otu_table_PLSDA.csv", h=T, sep=",", row.names = 1)
group=data[,107]
otu=data[,1:105]
tableau<-otu
tableau=scale(tableau, center = TRUE, scale= TRUE)
tableau=as.data.frame(tableau)
tableau=as.matrix(tableau)
var.ind<-dummy(group)
PLSDA<-cppls(var.ind~tableau)
MVA.cmv(tableau,group,model="PPLS-DA",crit.inn="NMC")
MVA.test(tableau,group,model="PPLS-DA",cmv=TRUE)

pdf("p1.pdf", width=5, height=5,useDingbats = F)
MVA.scoreplot(PLSDA,fac=group, xax=1, yax=2, xlab="Comp 1. (9.50%)", ylab="Comp 2. (6.12%)", col=c("cadetblue","darkgoldenrod3"), cex=2, pch=16, barycenters=FALSE)
dev.off()

MVA.corplot(PLSDA, thresh = 0.5, xax=1, yax=2, circle=TRUE, intcircle=1, lwd=2, cex=1.2,col="dimgrey", keepmar=FALSE)


#############PLS-DA strains selected by the SVM RFE model###############
library(RVAideMemoire)
library(pls)
data=read.table("otu_table_PLSDA.csv", h=T, sep=",", row.names = 1)
list_strains=read.table("List_strains_SVM.csv", h=F, sep=",")
group=data[,107]
otu=data[,1:105]
tableau<-otu
tableau=tableau[,which((colnames(tableau) %in% list_strains$V1))]
tableau=scale(tableau, center = TRUE, scale= TRUE)
tableau=as.data.frame(tableau)
tableau=as.matrix(tableau)
var.ind<-dummy(group)
PLSDA<-cppls(var.ind~tableau)
MVA.cmv(tableau,group,model="PPLS-DA",crit.inn="NMC")
MVA.test(tableau,group,model="PPLS-DA",cmv=TRUE)

pdf("p2.pdf", width=5, height=5,useDingbats = F)
MVA.scoreplot(PLSDA,fac=group, xax=1, yax=2, xlab="Comp 1. (12.63%)", ylab="Comp 2. (6.74%)", col=c("cadetblue","darkgoldenrod3"), cex=2, pch=16, barycenters=FALSE)
dev.off()

MVA.corplot(PLSDA, thresh = 0.5, xax=1, yax=2, circle=TRUE, intcircle=1, lwd=2, cex=1.2,col="dimgrey", keepmar=FALSE)

#############PLS-DA with strains NOT selected by the SVM RFE model###############
library(RVAideMemoire)
library(pls)
data=read.table("otu_table_PLSDA.csv", h=T, sep=",", row.names = 1)
list_strains=read.table("List_strains_SVM.csv", h=F, sep=",")
group=data[,107]
otu=data[,1:105]
tableau<-otu
tableau=tableau[,-which((colnames(tableau) %in% list_strains$V1))]
tableau=scale(tableau, center = TRUE, scale= TRUE)
tableau=as.data.frame(tableau)
tableau=as.matrix(tableau)
var.ind<-dummy(group)
PLSDA<-cppls(var.ind~tableau,  ncomp=2)

MVA.cmv(tableau,group,model="PPLS-DA",crit.inn="NMC")
MVA.test(tableau,group,model="PPLS-DA",cmv=TRUE)

PLSDA<-cppls(var.ind~tableau)

pdf("p3.pdf", width=5, height=5,useDingbats = F)
MVA.scoreplot(PLSDA,fac=group, xax=1, yax=2, xlab="Comp 1. (8.64%)", ylab="Comp 2. (6.32%)", col=c("cadetblue","darkgoldenrod3"), cex=2, pch=16, barycenters=FALSE)
dev.off()
