library(pheatmap)
library(gplots)

## Read DE results with count data
tab <- read.table("DiffExp_All.txt", header=TRUE, sep="\t", strip.white=TRUE, 
                  as.is=TRUE, quote="", comment.char="")


##subset shoot

tab_shoot <- tab[,c(1:3,5,7,9,11,13,15,17,19,31:33,37:39,43:45,49:51)]

# subset of significantly DE genes regulated shade vs normal (fdr<0.05, |log2FC|>1) 
tab_s1 <- tab_shoot[tab_shoot$FDR.Shoot_N < 0.05 & abs(tab_shoot$log2FC.Shoot_N) >= 1, ]
tab_s2 <- tab_shoot[tab_shoot$FDR.Shoot_Y < 0.05 & abs(tab_shoot$log2FC.Shoot_Y) >= 1, ]

# subset of significantly DE genes regulated withmicrobes (fdr<0.05, |log2FC|>1) 
tab_s3 <- tab_shoot[tab_shoot$FDR.NC_Shoot < 0.05 & abs(tab_shoot$log2FC.NC_Shoot) >= 1, ]
tab_s4 <- tab_shoot[tab_shoot$FDR.SC_Shoot < 0.05 & abs(tab_shoot$log2FC.SC_Shoot) >= 1, ]

##merge all subsets

cpm_shoot <- rbind(tab_s1,tab_s2,tab_s3,tab_s4)
cpm_shoot_dup <- cpm_shoot[!duplicated(cpm_shoot[,1]),]

cpm_shoot <- cpm_shoot_dup


## Extract shoot log2cpm matrix (and re-order columns for heatmap)
cpm <- as.matrix(cpm_shoot[,c(12:23)])
rownames(cpm) <- cpm_shoot$GeneID
colnames(cpm) <- gsub(".log2cpm", "", colnames(cpm))

cpm <- as.data.frame(cpm)

cpm <- cpm[,c(1:3,7:9,4:6,10:12)]

cpm_shoot <- cpm


p <- pheatmap(cpm_shoot,  color = colorRampPalette(c("blue","white","red"))(100), scale="row",
              cluster_rows=T, cluster_cols=FALSE, 
              clustering_distance_rows="euclidean",
              clustering_method="complete",legend_breaks = -3:3, 
              cutree_rows = 8,
              annotation_names_row = FALSE,show_rownames = FALSE)


#####check cluster dendrogram to choose cutnumber
rownames(cpm_shoot[p$tree_row[["order"]],])
plot(p$tree_row)

genenlist_shoot <- as.data.frame(cutree(p$tree_row, k=8))
GeneID <- rownames(genenlist_shoot)
genenlist_shoot <- cbind(GeneID,genenlist_shoot)
colnames(genenlist_shoot) <- c("GeneID","clusterNO")
rownames(genenlist_shoot) <- NULL

###reorder rows as the trees
#cpm_shoot1 <- cpm_shoot
#cpm_shoot1 <- cpm_shoot1[match(rownames(cpm[p$tree_row[["order"]],]),rownames(cpm_shoot1)),,drop=FALSE]


p.clust <- cbind(p, cluster =sort(cutree(p$tree_row, k=8)))
p.clust <- as.data.frame(unlist(p.clust[,2]))
colnames(p.clust) <- c("Cluster")

#mycolors <- c("red","orange","#ddf542","#5af542","#42f575","#42f575","#42f5c8","#42d4f5")
#names(cluster) <- unique(genenlist_shoot$clusterNO)

#ann_colors = list(Cluster = c("gray","yellow","skyblue","green","darkgreen","black","blue","yellow4"))


p <- pheatmap(cpm_shoot,  color = colorRampPalette(c("blue","white","red"))(100), scale="row",
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
              cellwidth=6,
              cellheight=0.07)

rownames(cpm[p$tree_row[["order"]],])
plot(p$tree_row)

pdf("Figure_3b.pdf", width=3, height=5)
p
dev.off()

#####check cluster dendrogram to choose cutnumber
rownames(cpm[p$tree_row[["order"]],])
plot(p$tree_row)

genenlist_shoot <- as.data.frame(sort(cutree(p$tree_row, k=8)))
GeneID <- rownames(genenlist_shoot)
genenlist_shoot <- cbind(GeneID,genenlist_shoot)
colnames(genenlist_shoot) <- c("GeneID","clusterNO")
rownames(genenlist_shoot) <- NULL

#### exact gene name from same cluster (15 clusters in totall)

clusterS1 <- genenlist_shoot[genenlist_shoot$clusterNO == 1, ]
clusterS1 <- clusterS1[,1]
write.table(clusterS1,"clusterS1.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS2 <- genenlist_shoot[genenlist_shoot$clusterNO == 2, ]
clusterS2 <- clusterS2[,1]
write.table(clusterS2,"clusterS2.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS3 <- genenlist_shoot[genenlist_shoot$clusterNO == 3, ]
clusterS3 <- clusterS3[,1]
write.table(clusterS3,"clusterS3.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS4 <- genenlist_shoot[genenlist_shoot$clusterNO == 4, ]
clusterS4 <- clusterS4[,1]
write.table(clusterS4,"clusterS4.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS5 <- genenlist_shoot[genenlist_shoot$clusterNO == 5, ]
clusterS5 <- clusterS5[,1]
write.table(clusterS5,"clusterS5.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS6 <- genenlist_shoot[genenlist_shoot$clusterNO == 6, ]
clusterS6 <- clusterS6[,1]
write.table(clusterS6,"clusterS6.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS7 <- genenlist_shoot[genenlist_shoot$clusterNO == 7, ]
clusterS7 <- clusterS7[,1]
write.table(clusterS7,"clusterS7.txt", sep="\t", quote=FALSE, row=FALSE)

clusterS8 <- genenlist_shoot[genenlist_shoot$clusterNO == 8, ]
clusterS8 <- clusterS8[,1]
write.table(clusterS8,"clusterS8.txt", sep="\t", quote=FALSE, row=FALSE)
