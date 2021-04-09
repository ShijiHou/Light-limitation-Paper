## Read DE results with count data
tab <- read.table("DiffExp_myc2_vs_myc2flag.txt", header=TRUE, sep="\t", strip.white=TRUE, 
                  as.is=TRUE, quote="", comment.char="")


##subset root

tab_root <- tab[,c(1:3,8:11,16:19)]

tab <- tab_root

# subset of significantly DE genes regulated shade vs normal (fdr<0.05, |log2FC|>1) 
tab_r4 <- tab[tab$FDR.SC_Y_Root < 0.05 & abs(tab$log2FC.SC_Y_Root)  >= 1, ]

## Read DE results with count data
tab <- read.table("DiffExp_myc2_vs_myc2flag.txt", header=TRUE, sep="\t", strip.white=TRUE, 
                  as.is=TRUE, quote="", comment.char="")


##subset shoot

tab_shoot <- tab[,c(1:7,12:15)]

# subset of significantly DE genes regulated myc2 vs col0 (fdr<0.05, |log2FC|>1) 

tab_s4 <- tab_shoot[tab_shoot$FDR.SC_Y_Shoot < 0.05 & abs(tab_shoot$log2FC.SC_Y_Shoot) >= 1, ]

library(dplyr)
library(pheatmap)

shared <- inner_join(tab_r4,tab_s4,by = "GeneID")

## Extract shoot log2cpm matrix (and re-order columns for heatmap)
cpm <- as.matrix(shared[,c(17,7)])
rownames(cpm) <- shared$GeneID
colnames(cpm) <- gsub(".log2cpm", "", colnames(cpm))

cpm <- as.data.frame(cpm)


p <- pheatmap(cpm,  color = colorRampPalette(c("blue","white","red"))(100), scale="row",
              cluster_rows=T, cluster_cols=FALSE, 
              clustering_distance_rows="euclidean",
              clustering_method="complete",legend_breaks = -3:3, 
              cutree_rows = 6,
              annotation_names_row = FALSE,show_rownames = FALSE)


rownames(cpm[p$tree_row[["order"]],])
plot(p$tree_row)


p.clust <- cbind(p, cluster =sort(cutree(p$tree_row, k=6)))
p.clust <- as.data.frame(unlist(p.clust[,2]))
colnames(p.clust) <- c("Cluster")


#mycolors <- c("red","orange","#ddf542","#5af542","#42f575","#42f575","#42f5c8","#42d4f5")
#names(cluster) <- unique(genenlist_shoot$clusterNO)

#ann_colors = list(Cluster = c("orange","red","darkviolet","plum","lightsalmon","deeppink","orange4","pink","darkred"))


p <- pheatmap(cpm,  color = colorRampPalette(c("blue","white","red"))(50), scale="row",
              cluster_rows=T, cluster_cols=FALSE, 
              clustering_distance_rows="euclidean",
              clustering_method="complete",legend_breaks = -3:3, 
              cutree_rows = 6,
              annotation_names_row = FALSE,show_rownames = FALSE,
              show_colnames = FALSE,
              legend = FALSE,
              treeheight_row = 20,
              #annotation_row = p.clust,
              #annotation_colors = ann_colors,
              cellwidth=10,
              cellheight=0.1)


pdf("shared_1.pdf", useDingbats = F)
p
dev.off()



