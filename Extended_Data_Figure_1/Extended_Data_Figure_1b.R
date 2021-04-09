# load plotting functions
library(ggplot2, quietly=T, warn.conflicts=F)
library(scales, quietly=T, warn.conflicts=F)
library(gridExtra, quietly=T, warn.conflicts=F)

alpha <- .7
c_red <-             rgb(200 / 255,   0 / 255,   0 / 255, alpha)
c_dark_red <-        rgb(255 / 255, 130 / 255,   0 / 255, alpha)
c_violet <-          rgb(238 / 255, 130 / 255, 238 / 255, alpha)
c_dark_violet <-     rgb(148 / 255, 0 / 255, 211 / 255, alpha)
c_pink <-            rgb(255 / 255, 192 / 255, 203 / 255, alpha)
c_orange <-          rgb(255 / 255,  69 / 255,   0 / 255, alpha)
c_dark_orange <-     rgb(255 / 255,  140 / 255,   0 / 255, alpha)
c_yellow <-          rgb(255 / 255, 255 / 255,   0 / 255, alpha)
c_blue <-            rgb(  0 / 255, 0 / 255, 255 / 255, alpha)
c_dark_blue <-       rgb(  0 / 255, 0 / 255, 139 / 255, alpha)
c_green <-           rgb(  50/ 255, 220 / 255,  50 / 255, alpha)
c_dark_green <-      rgb( 50 / 255, 200 / 255, 100 / 255, alpha)
c_very_dark_green <- rgb( 50 / 255, 150 / 255, 100 / 255, alpha)
c_sea_green <-       rgb( 46 / 255, 129 / 255,  90 / 255, alpha)
c_grey <-            rgb(180 / 255, 180 / 255,  180 / 255, alpha)
c_dark_brown <-      rgb(101 / 255,  67 / 255,  33 / 255, alpha)
c_black <-           rgb(  0 / 255,   0 / 255,   0 / 255, alpha)



# plotting stuff

main_theme <- theme(panel.background=element_blank(),
                    panel.grid=element_blank(),
                    axis.line=element_line(color="black"),
                    axis.ticks=element_line(color="black"),
                    axis.text=element_text(colour="black", size=10),
                    legend.position="top",
                    legend.background=element_blank(),
                    legend.key=element_blank(),
                    axis.text.x=element_text(angle=45, hjust=1),
                    text=element_text(family="sans"))

######## bacteria#####################
# load data

design <- read.table("bac_design.txt", header=T, sep="\t")
bray_curtis <- read.table("bac_bray_curtis_otu_table_norm.txt", sep="\t", header=T, check.names=F)

# re-order data matrices

idx <- match(design$SampleID, colnames(bray_curtis))
bray_curtis <- bray_curtis[idx, idx]


# subset samples of interest from distance matrices

excluded_samples <- design$SampleID[design$not_include=="yes"]
idx <- which(!design$SampleID %in% excluded_samples)
design <- design[idx, ]
bray_curtis <- bray_curtis[idx, idx]

### beta diversity
colors <- data.frame(group=c("Matrix", "Root","Input"), 
                     color=c("orange", "gray","black"))

shapes <- data.frame(group=c("LLL_F", "LLH_F", "LHH_F", "HHH_F", "HLL_F", "HHL_F", "HLH_F", "LHL_F", "Input"),
                     shape=c(0,1,2,3,4,5,6,7,19))


# PCoA Bray-Curtis

k <- 2
pcoa <- cmdscale(bray_curtis, k=k, eig=T)
points <- pcoa$points
eig <- pcoa$eig
points <- as.data.frame(points)
colnames(points) <- c("x", "y")

points <- cbind(points, design[match(rownames(points), design$SampleID), ])

points$Condition <- factor(points$Condition, levels=shapes$group)
points$Compartment <- factor(points$Compartment, levels=colors$group)

# plot PCo 1 and 2

p <- ggplot(points, aes(x=x, y=y, color=Compartment, shape=Condition)) +
     geom_point(alpha=.5, size=5) +
     scale_colour_manual(values=as.character(colors$color)) +
     scale_shape_manual(values=shapes$shape) +
     labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
     y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) + 
     main_theme +
     theme(legend.position="top")+ theme(
       legend.title = element_text(color = "black", size = 5),
       legend.text = element_text(color = "black", size = 5)
     )

p
ggsave(paste("PCoA_BC_bacteria.pdf", sep=""), p)

######## fungi #####################
# load data

design <- read.table("fun_design.txt", header=T, sep="\t")
bray_curtis <- read.table("fun_bray_curtis_otu_table_norm.txt", sep="\t", header=T, check.names=F)

# re-order data matrices

idx <- match(design$SampleID, colnames(bray_curtis))
bray_curtis <- bray_curtis[idx, idx]


# subset samples of interest from distance matrices

excluded_samples <- design$SampleID[design$not_include=="yes"]
idx <- which(!design$SampleID %in% excluded_samples)
design <- design[idx, ]
bray_curtis <- bray_curtis[idx, idx]

### beta diversity
colors <- data.frame(group=c("matrix", "root","input"), 
                     color=c("orange", "gray","black"))

shapes <- data.frame(group=c("LLL", "LLH", "LHH", "HHH", "HLL", "HHL", "HLH", "LHL", "input"),
                     shape=c(0,1,2,3,4,5,6,7,19))


# PCoA Bray-Curtis

k <- 2
pcoa <- cmdscale(bray_curtis, k=k, eig=T)
points <- pcoa$points
eig <- pcoa$eig
points <- as.data.frame(points)
colnames(points) <- c("x", "y")

points <- cbind(points, design[match(rownames(points), design$SampleID), ])

points$Condition <- factor(points$Condition, levels=shapes$group)
points$Compartment <- factor(points$Compartment, levels=colors$group)

# plot PCo 1 and 2

p <- ggplot(points, aes(x=x, y=y, color=Compartment, shape=Condition)) +
  geom_point(alpha=.5, size=5) +
  scale_colour_manual(values=as.character(colors$color)) +
  scale_shape_manual(values=shapes$shape) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) + 
  main_theme +
  theme(legend.position="top")+ theme(
    legend.title = element_text(color = "black", size = 5),
    legend.text = element_text(color = "black", size = 5)
  )

p
ggsave(paste("PCoA_BC_fungi.pdf", sep=""), p)

######## oomycetes #####################
# load data

design <- read.table("oom_design.txt", header=T, sep="\t")
bray_curtis <- read.table("oom_bray_curtis.txt", sep="\t", header=T, check.names=F)

# re-order data matrices

idx <- match(design$SampleID, colnames(bray_curtis))
bray_curtis <- bray_curtis[idx, idx]


# subset samples of interest from distance matrices

excluded_samples <- design$SampleID[design$not_include=="yes"]
idx <- which(!design$SampleID %in% excluded_samples)
design <- design[idx, ]
bray_curtis <- bray_curtis[idx, idx]

### beta diversity
colors <- data.frame(group=c("matrix", "root","input"), 
                     color=c("orange", "gray","black"))

shapes <- data.frame(group=c("LLL", "LLH", "LHH", "HHH", "HLL", "HHL", "HLH", "LHL", "input"),
                     shape=c(0,1,2,3,4,5,6,7,19))


# PCoA Bray-Curtis

k <- 2
pcoa <- cmdscale(bray_curtis, k=k, eig=T)
points <- pcoa$points
eig <- pcoa$eig
points <- as.data.frame(points)
colnames(points) <- c("x", "y")

points <- cbind(points, design[match(rownames(points), design$SampleID), ])

points$Condition <- factor(points$Condition, levels=shapes$group)
points$Compartment <- factor(points$Compartment, levels=colors$group)

# plot PCo 1 and 2

p <- ggplot(points, aes(x=x, y=y, color=Compartment, shape=Condition)) +
  geom_point(alpha=.5, size=5) +
  scale_colour_manual(values=as.character(colors$color)) +
  scale_shape_manual(values=shapes$shape) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) + 
  main_theme +
  theme(legend.position="top")+ theme(
    legend.title = element_text(color = "black", size = 5),
    legend.text = element_text(color = "black", size = 5)
  )

p
ggsave(paste("PCoA_BC_oomycetes.pdf", sep=""), p)


