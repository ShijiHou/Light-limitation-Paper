library(pheatmap)
library(gplots)

## Read DE results with count data
tab <- read.table("DiffExp_myc2_vs_myc2flag.txt", header=TRUE, sep="\t", strip.white=TRUE, 
                  as.is=TRUE, quote="", comment.char="")


##subset shoot

tab_shoot <- tab[,c(1:7,12:15)]

# subset of significantly DE genes regulated myc2 vs col0 (fdr<0.05, |log2FC|>1) 
tab_s1 <- tab_shoot[tab_shoot$FDR.NC_N_Shoot < 0.05 & abs(tab_shoot$log2FC.NC_N_Shoot) >= 1, ]
tab_s2 <- tab_shoot[tab_shoot$FDR.NC_Y_Shoot < 0.05 & abs(tab_shoot$log2FC.NC_Y_Shoot) >= 1, ]

tab_s3 <- tab_shoot[tab_shoot$FDR.SC_N_Shoot < 0.05 & abs(tab_shoot$log2FC.SC_N_Shoot) >= 1, ]
tab_s4 <- tab_shoot[tab_shoot$FDR.SC_Y_Shoot < 0.05 & abs(tab_shoot$log2FC.SC_Y_Shoot) >= 1, ]

# count significant genes

NC_N_Shoot_up <- tab_shoot[tab_shoot$FDR.NC_N_Shoot < 0.05 & tab_shoot$log2FC.NC_N_Shoot >= 1, ]
nrow(NC_N_Shoot_up)
#[1] 795

NC_N_Shoot_down <- tab_shoot[tab_shoot$FDR.NC_N_Shoot < 0.05 & tab_shoot$log2FC.NC_N_Shoot <= -1, ]
nrow(NC_N_Shoot_down)
#[1] 382

NC_Y_Shoot_up <- tab_shoot[tab_shoot$FDR.NC_Y_Shoot < 0.05 & tab_shoot$log2FC.NC_Y_Shoot >= 1, ]
nrow(NC_Y_Shoot_up)
#[1] 1115

NC_Y_Shoot_down <- tab_shoot[tab_shoot$FDR.NC_Y_Shoot < 0.05 & tab_shoot$log2FC.NC_Y_Shoot <= -1, ]
nrow(NC_Y_Shoot_down)
#[1] 833

SC_N_Shoot_up <- tab_shoot[tab_shoot$FDR.SC_N_Shoot < 0.05 & tab_shoot$log2FC.SC_N_Shoot >= 1, ]
nrow(SC_N_Shoot_up)
#[1] 346

SC_N_Shoot_down <- tab_shoot[tab_shoot$FDR.SC_N_Shoot < 0.05 & tab_shoot$log2FC.SC_N_Shoot <= -1, ]
nrow(SC_N_Shoot_down)
#[1] 430

SC_Y_Shoot_up <- tab_shoot[tab_shoot$FDR.SC_Y_Shoot < 0.05 & tab_shoot$log2FC.SC_Y_Shoot >= 1, ]
nrow(SC_Y_Shoot_up)
#[1] 1386

SC_Y_Shoot_down <- tab_shoot[tab_shoot$FDR.SC_Y_Shoot < 0.05 & tab_shoot$log2FC.SC_Y_Shoot <= -1, ]
nrow(SC_Y_Shoot_down)
#[1] 1926


##merge all subsets

cpm_shoot <- rbind(tab_s1,tab_s2,tab_s3,tab_s4)
cpm_shoot_dup <- cpm_shoot[!duplicated(cpm_shoot[,1]),]

cpm_shoot <- cpm_shoot_dup

write.table(cpm_shoot,"DEGs_myc2vsmyc2flag_shoot.txt",sep="\t", quote=FALSE, row=T)


## Extract shoot log2cpm matrix (and re-order columns for heatmap)
cpm <- as.matrix(cpm_shoot[,c(4:7)])
rownames(cpm) <- cpm_shoot$GeneID
colnames(cpm) <- gsub(".log2cpm", "", colnames(cpm))

cpm <- as.data.frame(cpm)

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
              cellwidth=18,
              cellheight=0.03)

#rownames(cpm[p$tree_row[["order"]],])
#plot(p$tree_row)

pdf("Figure_6c.pdf", width=2, height=4,useDingbats = F)
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


