tab=read.table("bray_curtis_tab.txt",header=T)

xx=grep("(NC|LP)_(SF|CAS)_(R|M)",rownames(tab))

bc_bac=tab[xx,xx]
pcoa_bac <- cmdscale(bc_bac, k=2, eig=T)
bac_v1=format(100*pcoa_bac$eig[1]/sum(pcoa_bac$eig),digits=4)
bac_v2=format(100*pcoa_bac$eig[2]/sum(pcoa_bac$eig),digits=4)

dev.new(height=4.208145,width=4.253394)

plot(pcoa_bac$points[,1],pcoa_bac$points[,2],cex=0.5,col="white",xlab=c("PC1",bac_v1),ylab=c("PC2",bac_v2))

aa_0=grep("NC_SF_R",row.names(bc_bac),perl=T)
aa_1=grep("LP_SF_R",row.names(bc_bac),perl=T)
aa_2=grep("LP_SF_M",row.names(bc_bac),perl=T)
aa_3=grep("NC_SF_M",row.names(bc_bac),perl=T)
points(pcoa_bac$points[aa_0,1],pcoa_bac$points[aa_0,2],pch=1,col="tan",cex=1.5)
points(pcoa_bac$points[aa_1,1],pcoa_bac$points[aa_1,2],pch=19,col="tan",cex=1.5)
points(pcoa_bac$points[aa_2,1],pcoa_bac$points[aa_2,2],pch=17,col="orange",cex=1.5)
points(pcoa_bac$points[aa_3,1],pcoa_bac$points[aa_3,2],pch=2,col="orange",cex=1.5)

aa_4=grep("NC_CAS_R",row.names(bc_bac),perl=T)
aa_5=grep("LP_CAS_R",row.names(bc_bac),perl=T)
aa_6=grep("LP_CAS_M",row.names(bc_bac),perl=T)
aa_7=grep("NC_CAS_M",row.names(bc_bac),perl=T)
points(pcoa_bac$points[aa_4,1],pcoa_bac$points[aa_4,2],pch=1,col="tan",cex=1.5)
points(pcoa_bac$points[aa_5,1],pcoa_bac$points[aa_5,2],pch=19,col="tan",cex=1.5)
points(pcoa_bac$points[aa_6,1],pcoa_bac$points[aa_6,2],pch=17,col="orange",cex=1.5)
points(pcoa_bac$points[aa_7,1],pcoa_bac$points[aa_7,2],pch=2,col="orange",cex=1.5)