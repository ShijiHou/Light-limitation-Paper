library(pheatmap)
library(gplots)

## Read DE results with count data
tab <- read.table("DiffExp_myc2_vs_myc2flag.txt", header=TRUE, sep="\t", strip.white=TRUE, 
                  as.is=TRUE, quote="", comment.char="")


##subset root

tab_root <- tab[,c(1:3,8:11,16:19)]

tab <- tab_root

# subset of significantly DE genes regulated shade vs normal (fdr<0.05, |log2FC|>1) 
tab_r1 <- tab[tab$FDR.NC_N_Root < 0.05 & abs(tab$log2FC.NC_N_Root) >= 1, ]
tab_r2 <- tab[tab$FDR.NC_Y_Root < 0.05 & abs(tab$log2FC.NC_Y_Root) >= 1, ]

# subset of significantly DE genes regulated withmicrobes (fdr<0.05, |log2FC|>1) 
tab_r3 <- tab[tab$FDR.SC_N_Root < 0.05 & abs(tab$log2FC.SC_N_Root) >= 1, ]
tab_r4 <- tab[tab$FDR.SC_Y_Root < 0.05 & abs(tab$log2FC.SC_Y_Root)  >= 1, ]
##merge all subsets

cpm_root <- rbind(tab_r1,tab_r2,tab_r3,tab_r4)
cpm_root_dup <- cpm_root[!duplicated(cpm_root[,1]),]

cpm_root <- cpm_root_dup

write.table(cpm_root,"DEGs_myc2vsmyc2flag_root.txt",sep="\t", quote=FALSE, row=T)

## Extract shoot log2cpm matrix (and re-order columns for heatmap)
cpm <- as.matrix(cpm_root[,c(4:7)])
rownames(cpm) <- cpm_root$GeneID
colnames(cpm) <- gsub(".log2cpm", "", colnames(cpm))

cpm <- as.data.frame(cpm)

cpm_root <- cpm

p <- pheatmap(cpm_root,  color = colorRampPalette(c("blue","white","red"))(100), scale="row",
              cluster_rows=T, cluster_cols=FALSE, 
              clustering_distance_rows="euclidean",
              clustering_method="complete",legend_breaks = -3:3, 
              cutree_rows = 8,
              annotation_names_row = FALSE,show_rownames = FALSE)


rownames(cpm[p$tree_row[["order"]],])
plot(p$tree_row)


p.clust <- cbind(p, cluster =sort(cutree(p$tree_row, k=9)))
p.clust <- as.data.frame(unlist(p.clust[,2]))
colnames(p.clust) <- c("Cluster")

#mycolors <- c("red","orange","#ddf542","#5af542","#42f575","#42f575","#42f5c8","#42d4f5")
#names(cluster) <- unique(genenlist_shoot$clusterNO)

#ann_colors = list(Cluster = c("orange","red","darkviolet","plum","lightsalmon","deeppink","orange4","pink","darkred"))


p <- pheatmap(cpm_root,  color = colorRampPalette(c("blue","white","red"))(50), scale="row",
              cluster_rows=T, cluster_cols=FALSE, 
              clustering_distance_rows="euclidean",
              clustering_method="complete",legend_breaks = -3:3, 
              cutree_rows = 8,
              annotation_names_row = FALSE,show_rownames = FALSE,
              show_colnames = FALSE,
              legend = FALSE,
              treeheight_row = 20,
              #annotation_row = p.clust,
              #annotation_colors = ann_colors,
              cellwidth=18,
              cellheight=0.03)

#rownames(cpm[p$tree_row[["order"]],])
#plot(p$tree_row)


pdf("Extended_Data_Figure_8f.pdf", width=2, height=4,useDingbats = F)
p
dev.off()


#####check cluster dendrogram to choose cutnumber
rownames(cpm[p$tree_row[["order"]],])
plot(p$tree_row)

genenlist_root <- as.data.frame(sort(cutree(p$tree_row, k=9)))
GeneID <- rownames(genenlist_root)
genenlist_root <- cbind(GeneID,genenlist_root)
colnames(genenlist_root) <- c("GeneID","clusterNO")
rownames(genenlist_root) <- NULL

#### exact gene name from same cluster (15 clusters in totall)

clusterR1 <- genenlist_root[genenlist_root$clusterNO == 1, ]
clusterR1 <- clusterR1[,1]
write.table(clusterR1,"clusterR1.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR2 <- genenlist_root[genenlist_root$clusterNO == 2, ]
clusterR2 <- clusterR2[,1]
write.table(clusterR2,"clusterR2.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR3 <- genenlist_root[genenlist_root$clusterNO == 3, ]
clusterR3 <- clusterR3[,1]
write.table(clusterR3,"clusterR3.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR4 <- genenlist_root[genenlist_root$clusterNO == 4, ]
clusterR4 <- clusterR4[,1]
write.table(clusterR4,"clusterR4.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR5 <- genenlist_root[genenlist_root$clusterNO == 5, ]
clusterR5 <- clusterR5[,1]
write.table(clusterR5,"clusterR5.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR6 <- genenlist_root[genenlist_root$clusterNO == 6, ]
clusterR6 <- clusterR6[,1]
write.table(clusterR6,"clusterR6.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR7 <- genenlist_root[genenlist_root$clusterNO == 7, ]
clusterR7 <- clusterR7[,1]
write.table(clusterR7,"clusterR7.txt", sep="\t", quote=FALSE, row=FALSE)

clusterR8 <- genenlist_root[genenlist_root$clusterNO == 8, ]
clusterR8 <- clusterR8[,1]
write.table(clusterR8,"clusterR8.txt", sep="\t", quote=FALSE, row=FALSE)
