library("gplots")
library("ggplot2")

cs <- read.table("CS_mutants_heatmap.txt", sep="\t",header=T)
bc <- read.table("BC_mutants_heatmap.txt", sep="\t",header=T)
pto <- read.table("pto_mutants_heatmap.txt", sep="\t",header=T)

cs_col0 <- subset(cs, geno=="col0")

cs_col0_nc <- subset(cs_col0, lab=="NC")
cs_col0_ncm <- subset(cs_col0, lab=="NC+M")
cs_col0_lp <- subset(cs_col0, lab=="LP")
cs_col0_lpm <- subset(cs_col0, lab=="LP+M")

cs_sid2 <- subset(cs, geno=="sid2")

cs_sid2_nc <- subset(cs_sid2, lab=="NC")
cs_sid2_ncm <- subset(cs_sid2, lab=="NC+M")
cs_sid2_lp <- subset(cs_sid2, lab=="LP")
cs_sid2_lpm <- subset(cs_sid2, lab=="LP+M")

cs_dde2 <- subset(cs, geno=="dde2")

cs_dde2_nc <- subset(cs_dde2, lab=="NC")
cs_dde2_ncm <- subset(cs_dde2, lab=="NC+M")
cs_dde2_lp <- subset(cs_dde2, lab=="LP")
cs_dde2_lpm <- subset(cs_dde2, lab=="LP+M")

cs_myc2 <- subset(cs, geno=="myc2")

cs_myc2_nc <- subset(cs_myc2, lab=="NC")
cs_myc2_ncm <- subset(cs_myc2, lab=="NC+M")
cs_myc2_lp <- subset(cs_myc2, lab=="LP")
cs_myc2_lpm <- subset(cs_myc2, lab=="LP+M")

cs_jazq <- subset(cs, geno=="jazq")

cs_jazq_nc <- subset(cs_jazq, lab=="NC")
cs_jazq_ncm <- subset(cs_jazq, lab=="NC+M")
cs_jazq_lp <- subset(cs_jazq, lab=="LP")
cs_jazq_lpm <- subset(cs_jazq, lab=="LP+M")

cs_della <- subset(cs, geno=="della")

cs_della_nc <- subset(cs_della, lab=="NC")
cs_della_ncm <- subset(cs_della, lab=="NC+M")
cs_della_lp <- subset(cs_della, lab=="LP")
cs_della_lpm <- subset(cs_della, lab=="LP+M")

cs_bri1 <- subset(cs, geno=="bri1")

cs_bri1_nc <- subset(cs_bri1, lab=="NC")
cs_bri1_ncm <- subset(cs_bri1, lab=="NC+M")
cs_bri1_lp <- subset(cs_bri1, lab=="LP")
cs_bri1_lpm <- subset(cs_bri1, lab=="LP+M")

cs_cry1cry2 <- subset(cs, geno=="cry1cry2")

cs_cry1cry2_nc <- subset(cs_cry1cry2, lab=="NC")
cs_cry1cry2_ncm <- subset(cs_cry1cry2, lab=="NC+M")
cs_cry1cry2_lp <- subset(cs_cry1cry2, lab=="LP")
cs_cry1cry2_lpm <- subset(cs_cry1cry2, lab=="LP+M")

cs_sav <- subset(cs, geno=="sav")

cs_sav_nc <- subset(cs_sav, lab=="NC")
cs_sav_ncm <- subset(cs_sav, lab=="NC+M")
cs_sav_lp <- subset(cs_sav, lab=="LP")
cs_sav_lpm <- subset(cs_sav, lab=="LP+M")

col0 <- c(mean(cs_col0_nc$canopysize),mean(cs_col0_ncm$canopysize),mean(cs_col0_lp$canopysize),mean(cs_col0_lpm$canopysize))
sid2 <- c(mean(cs_sid2_nc$canopysize),mean(cs_sid2_ncm$canopysize),mean(cs_sid2_lp$canopysize),mean(cs_sid2_lpm$canopysize))
dde2 <- c(mean(cs_dde2_nc$canopysize),mean(cs_dde2_ncm$canopysize),mean(cs_dde2_lp$canopysize),mean(cs_dde2_lpm$canopysize))
myc2 <- c(mean(cs_myc2_nc$canopysize),mean(cs_myc2_ncm$canopysize),mean(cs_myc2_lp$canopysize),mean(cs_myc2_lpm$canopysize))
jazq <- c(mean(cs_jazq_nc$canopysize),mean(cs_jazq_ncm$canopysize),mean(cs_jazq_lp$canopysize),mean(cs_jazq_lpm$canopysize))
della <- c(mean(cs_della_nc$canopysize),mean(cs_della_ncm$canopysize),mean(cs_della_lp$canopysize),mean(cs_della_lpm$canopysize))
bri1 <- c(mean(cs_bri1_nc$canopysize),mean(cs_bri1_ncm$canopysize),mean(cs_bri1_lp$canopysize),mean(cs_bri1_lpm$canopysize))
cry1cry2 <- c(mean(cs_cry1cry2_nc$canopysize),mean(cs_cry1cry2_ncm$canopysize),mean(cs_cry1cry2_lp$canopysize),mean(cs_cry1cry2_lpm$canopysize))
sav <- c(mean(cs_sav_nc$canopysize),mean(cs_sav_ncm$canopysize),mean(cs_sav_lp$canopysize),mean(cs_sav_lpm$canopysize))

cs1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
colnames(cs1) <- c("NC","NCM","LP","LPM")

pdf("canopysize_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "black"))(n = 50)
heatmap.2(cs1, Colv=FALSE, col=my_palette, key.title = "Canopy size",trace = "none")
dev.off()


bc_col0 <- subset(bc, geno=="col0")

bc_col0_nc <- subset(bc_col0, lab=="NC")
bc_col0_ncm <- subset(bc_col0, lab=="NC+M")
bc_col0_lp <- subset(bc_col0, lab=="LP")
bc_col0_lpm <- subset(bc_col0, lab=="LP+M")

bc_sid2 <- subset(bc, geno=="sid2")

bc_sid2_nc <- subset(bc_sid2, lab=="NC")
bc_sid2_ncm <- subset(bc_sid2, lab=="NC+M")
bc_sid2_lp <- subset(bc_sid2, lab=="LP")
bc_sid2_lpm <- subset(bc_sid2, lab=="LP+M")

bc_dde2 <- subset(bc, geno=="dde2")

bc_dde2_nc <- subset(bc_dde2, lab=="NC")
bc_dde2_ncm <- subset(bc_dde2, lab=="NC+M")
bc_dde2_lp <- subset(bc_dde2, lab=="LP")
bc_dde2_lpm <- subset(bc_dde2, lab=="LP+M")

bc_myc2 <- subset(bc, geno=="myc2")

bc_myc2_nc <- subset(bc_myc2, lab=="NC")
bc_myc2_ncm <- subset(bc_myc2, lab=="NC+M")
bc_myc2_lp <- subset(bc_myc2, lab=="LP")
bc_myc2_lpm <- subset(bc_myc2, lab=="LP+M")

bc_jazq <- subset(bc, geno=="jazq")

bc_jazq_nc <- subset(bc_jazq, lab=="NC")
bc_jazq_ncm <- subset(bc_jazq, lab=="NC+M")
bc_jazq_lp <- subset(bc_jazq, lab=="LP")
bc_jazq_lpm <- subset(bc_jazq, lab=="LP+M")

bc_della <- subset(bc, geno=="della")

bc_della_nc <- subset(bc_della, lab=="NC")
bc_della_ncm <- subset(bc_della, lab=="NC+M")
bc_della_lp <- subset(bc_della, lab=="LP")
bc_della_lpm <- subset(bc_della, lab=="LP+M")

bc_bri1 <- subset(bc, geno=="bri1")

bc_bri1_nc <- subset(bc_bri1, lab=="NC")
bc_bri1_ncm <- subset(bc_bri1, lab=="NC+M")
bc_bri1_lp <- subset(bc_bri1, lab=="LP")
bc_bri1_lpm <- subset(bc_bri1, lab=="LP+M")

bc_cry1cry2 <- subset(bc, geno=="cry1cry2")

bc_cry1cry2_nc <- subset(bc_cry1cry2, lab=="NC")
bc_cry1cry2_ncm <- subset(bc_cry1cry2, lab=="NC+M")
bc_cry1cry2_lp <- subset(bc_cry1cry2, lab=="LP")
bc_cry1cry2_lpm <- subset(bc_cry1cry2, lab=="LP+M")

bc_sav <- subset(bc, geno=="sav")

bc_sav_nc <- subset(bc_sav, lab=="NC")
bc_sav_ncm <- subset(bc_sav, lab=="NC+M")
bc_sav_lp <- subset(bc_sav, lab=="LP")
bc_sav_lpm <- subset(bc_sav, lab=="LP+M")

col0 <- c(mean(bc_col0_nc$y),mean(bc_col0_ncm$y),mean(bc_col0_lp$y),mean(bc_col0_lpm$y))
sid2 <- c(mean(bc_sid2_nc$y),mean(bc_sid2_ncm$y),mean(bc_sid2_lp$y),mean(bc_sid2_lpm$y))
dde2 <- c(mean(bc_dde2_nc$y),mean(bc_dde2_ncm$y),mean(bc_dde2_lp$y),mean(bc_dde2_lpm$y))
myc2 <- c(mean(bc_myc2_nc$y),mean(bc_myc2_ncm$y),mean(bc_myc2_lp$y),mean(bc_myc2_lpm$y))
jazq <- c(mean(bc_jazq_nc$y),mean(bc_jazq_ncm$y),mean(bc_jazq_lp$y),mean(bc_jazq_lpm$y))
della <- c(mean(bc_della_nc$y),mean(bc_della_ncm$y),mean(bc_della_lp$y),mean(bc_della_lpm$y))
bri1 <- c(mean(bc_bri1_nc$y),mean(bc_bri1_ncm$y),mean(bc_bri1_lp$y),mean(bc_bri1_lpm$y))
cry1cry2 <- c(mean(bc_cry1cry2_nc$y),mean(bc_cry1cry2_ncm$y),mean(bc_cry1cry2_lp$y),mean(bc_cry1cry2_lpm$y))
sav <- c(mean(bc_sav_nc$y),mean(bc_sav_ncm$y),mean(bc_sav_lp$y),mean(bc_sav_lpm$y))

bc1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
colnames(bc1) <- c("NC","NCM","LP","LPM")

pdf("bc_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "red"))(n = 50)
heatmap.2(bc1, Colv=FALSE, col=my_palette, trace = "none")
dev.off()

pto_col0 <- subset(pto, geno=="col0")

pto_col0_nc <- subset(pto_col0, lab=="NC")
pto_col0_ncm <- subset(pto_col0, lab=="NC+M")
pto_col0_lp <- subset(pto_col0, lab=="LP")
pto_col0_lpm <- subset(pto_col0, lab=="LP+M")

pto_sid2 <- subset(pto, geno=="sid2")

pto_sid2_nc <- subset(pto_sid2, lab=="NC")
pto_sid2_ncm <- subset(pto_sid2, lab=="NC+M")
pto_sid2_lp <- subset(pto_sid2, lab=="LP")
pto_sid2_lpm <- subset(pto_sid2, lab=="LP+M")

pto_dde2 <- subset(pto, geno=="dde2")

pto_dde2_nc <- subset(pto_dde2, lab=="NC")
pto_dde2_ncm <- subset(pto_dde2, lab=="NC+M")
pto_dde2_lp <- subset(pto_dde2, lab=="LP")
pto_dde2_lpm <- subset(pto_dde2, lab=="LP+M")

pto_myc2 <- subset(pto, geno=="myc2")

pto_myc2_nc <- subset(pto_myc2, lab=="NC")
pto_myc2_ncm <- subset(pto_myc2, lab=="NC+M")
pto_myc2_lp <- subset(pto_myc2, lab=="LP")
pto_myc2_lpm <- subset(pto_myc2, lab=="LP+M")

pto_jazq <- subset(pto, geno=="jazq")

pto_jazq_nc <- subset(pto_jazq, lab=="NC")
pto_jazq_ncm <- subset(pto_jazq, lab=="NC+M")
pto_jazq_lp <- subset(pto_jazq, lab=="LP")
pto_jazq_lpm <- subset(pto_jazq, lab=="LP+M")

pto_della <- subset(pto, geno=="della")

pto_della_nc <- subset(pto_della, lab=="NC")
pto_della_ncm <- subset(pto_della, lab=="NC+M")
pto_della_lp <- subset(pto_della, lab=="LP")
pto_della_lpm <- subset(pto_della, lab=="LP+M")

pto_bri1 <- subset(pto, geno=="bri1")

pto_bri1_nc <- subset(pto_bri1, lab=="NC")
pto_bri1_ncm <- subset(pto_bri1, lab=="NC+M")
pto_bri1_lp <- subset(pto_bri1, lab=="LP")
pto_bri1_lpm <- subset(pto_bri1, lab=="LP+M")

pto_cry1cry2 <- subset(pto, geno=="cry1cry2")

pto_cry1cry2_nc <- subset(pto_cry1cry2, lab=="NC")
pto_cry1cry2_ncm <- subset(pto_cry1cry2, lab=="NC+M")
pto_cry1cry2_lp <- subset(pto_cry1cry2, lab=="LP")
pto_cry1cry2_lpm <- subset(pto_cry1cry2, lab=="LP+M")

pto_sav <- subset(pto, geno=="sav")

pto_sav_nc <- subset(pto_sav, lab=="NC")
pto_sav_ncm <- subset(pto_sav, lab=="NC+M")
pto_sav_lp <- subset(pto_sav, lab=="LP")
pto_sav_lpm <- subset(pto_sav, lab=="LP+M")

col0 <- c(mean(pto_col0_nc$y),mean(pto_col0_ncm$y),mean(pto_col0_lp$y),mean(pto_col0_lpm$y))
sid2 <- c(mean(pto_sid2_nc$y),mean(pto_sid2_ncm$y),mean(pto_sid2_lp$y),mean(pto_sid2_lpm$y))
dde2 <- c(mean(pto_dde2_nc$y),mean(pto_dde2_ncm$y),mean(pto_dde2_lp$y),mean(pto_dde2_lpm$y))
myc2 <- c(mean(pto_myc2_nc$y),mean(pto_myc2_ncm$y),mean(pto_myc2_lp$y),mean(pto_myc2_lpm$y))
jazq <- c(mean(pto_jazq_nc$y),mean(pto_jazq_ncm$y),mean(pto_jazq_lp$y),mean(pto_jazq_lpm$y))
della <- c(mean(pto_della_nc$y),mean(pto_della_ncm$y),mean(pto_della_lp$y),mean(pto_della_lpm$y))
bri1 <- c(mean(pto_bri1_nc$y),mean(pto_bri1_ncm$y),mean(pto_bri1_lp$y),mean(pto_bri1_lpm$y))
cry1cry2 <- c(mean(pto_cry1cry2_nc$y),mean(pto_cry1cry2_ncm$y),mean(pto_cry1cry2_lp$y),mean(pto_cry1cry2_lpm$y))
sav <- c(mean(pto_sav_nc$y),mean(pto_sav_ncm$y),mean(pto_sav_lp$y),mean(pto_sav_lpm$y))

pto1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
colnames(pto1) <- c("NC","NCM","LP","LPM")

pdf("pto_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "red"))(n = 50)
heatmap.2(pto1, Colv=FALSE, col=my_palette, trace = "none")
dev.off()

sfw <- read.table("mutants8_sfw.txt", sep="\t",header=T)

col0 <- c(mean(subset(sfw, lab1=="col0_NC")$shootfreshweight),mean(subset(sfw, lab1=="col0_NC_M")$shootfreshweight),
          mean(subset(sfw, lab1=="col0_SC")$shootfreshweight),mean(subset(sfw, lab1=="col0_SC_M")$shootfreshweight))
bri1 <- c(mean(subset(sfw, lab1=="bri1_NC")$shootfreshweight),mean(subset(sfw, lab1=="bri1_NC_M")$shootfreshweight),
         mean(subset(sfw, lab1=="bri1_SC")$shootfreshweight),mean(subset(sfw, lab1=="bri1_SC_M")$shootfreshweight))
cry1cry2 <- c(mean(subset(sfw, lab1=="cry1cry2_NC")$shootfreshweight),mean(subset(sfw, lab1=="cry1cry2_NC_M")$shootfreshweight),
              mean(subset(sfw, lab1=="cry1cry2_SC")$shootfreshweight),mean(subset(sfw, lab1=="cry1cry2_SC_M")$shootfreshweight))
dde2 <- c(mean(subset(sfw, lab1=="dde2_NC")$shootfreshweight),mean(subset(sfw, lab1=="dde2_NC_M")$shootfreshweight),
          mean(subset(sfw, lab1=="dde2_SC")$shootfreshweight),mean(subset(sfw, lab1=="dde2_SC_M")$shootfreshweight))
della <- c(mean(subset(sfw, lab1=="della_NC")$shootfreshweight),mean(subset(sfw, lab1=="della_NC_M")$shootfreshweight),
           mean(subset(sfw, lab1=="della_SC")$shootfreshweight),mean(subset(sfw, lab1=="della_SC_M")$shootfreshweight))
jazq <- c(mean(subset(sfw, lab1=="jazq_NC")$shootfreshweight),mean(subset(sfw, lab1=="jazq_NC_M")$shootfreshweight),
          mean(subset(sfw, lab1=="jazq_SC")$shootfreshweight),mean(subset(sfw, lab1=="jazq_SC_M")$shootfreshweight))
myc2 <- c(mean(subset(sfw, lab1=="myc2_NC")$shootfreshweight),mean(subset(sfw, lab1=="myc2_NC_M")$shootfreshweight),
          mean(subset(sfw, lab1=="myc2_SC")$shootfreshweight),mean(subset(sfw, lab1=="myc2_SC_M")$shootfreshweight))
sav <- c(mean(subset(sfw, lab1=="sav_NC")$shootfreshweight),mean(subset(sfw, lab1=="sav_NC_M")$shootfreshweight),
         mean(subset(sfw, lab1=="sav_SC")$shootfreshweight),mean(subset(sfw, lab1=="sav_SC_M")$shootfreshweight)) 
sid2 <- c(mean(subset(sfw, lab1=="sid2_NC")$shootfreshweight),mean(subset(sfw, lab1=="sid2_NC_M")$shootfreshweight),
          mean(subset(sfw, lab1=="sid2_SC")$shootfreshweight),mean(subset(sfw, lab1=="sid2_SC_M")$shootfreshweight))

sfw1 <- rbind(col0,sid2,dde2,myc2,jazq,della,bri1,cry1cry2,sav)
colnames(sfw1) <- c("NC","NCM","LP","LPM")

pdf("sfw_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "black"))(n = 50)
heatmap.2(sfw1, Colv=FALSE,  col=my_palette, trace = "none")
dev.off()

pl <- read.table("mutants8_petiole_length.txt", sep="\t",header=T)

col0 <- c(mean(subset(pl, lab=="col0_NC")$petiole_length),mean(subset(pl, lab=="col0_NCM")$petiole_length),
          mean(subset(pl, lab=="col0_LP")$petiole_length),mean(subset(pl, lab=="col0_LPM")$petiole_length))
bri1 <- c(mean(subset(pl, lab=="bri1_NC")$petiole_length),mean(subset(pl, lab=="bri1_NCM")$petiole_length),
          mean(subset(pl, lab=="bri1_LP")$petiole_length),mean(subset(pl, lab=="bri1_LPM")$petiole_length))
cry1cry2 <- c(mean(subset(pl, lab=="cry1cry2_NC")$petiole_length),mean(subset(pl, lab=="cry1cry2_NCM")$petiole_length),
              mean(subset(pl, lab=="cry1cry2_LP")$petiole_length),mean(subset(pl, lab=="cry1cry2_LPM")$petiole_length))
dde2 <- c(mean(subset(pl, lab=="dde2_NC")$petiole_length),mean(subset(pl, lab=="dde2_NCM")$petiole_length),
          mean(subset(pl, lab=="dde2_LP")$petiole_length),mean(subset(pl, lab=="dde2_LPM")$petiole_length))
dellap <- c(mean(subset(pl, lab=="dellap_NC")$petiole_length),mean(subset(pl, lab=="dellap_NCM")$petiole_length),
           mean(subset(pl, lab=="dellap_LP")$petiole_length),mean(subset(pl, lab=="dellap_LPM")$petiole_length))
jazq <- c(mean(subset(pl, lab=="jazq_NC")$petiole_length),mean(subset(pl, lab=="jazq_NCM")$petiole_length),
          mean(subset(pl, lab=="jazq_LP")$petiole_length),mean(subset(pl, lab=="jazq_LPM")$petiole_length))
myc2 <- c(mean(subset(pl, lab=="myc2_NC")$petiole_length),mean(subset(pl, lab=="myc2_NCM")$petiole_length),
          mean(subset(pl, lab=="myc2_LP")$petiole_length),mean(subset(pl, lab=="myc2_LPM")$petiole_length))
sav <- c(mean(subset(pl, lab=="sav_NC")$petiole_length),mean(subset(pl, lab=="sav_NCM")$petiole_length),
         mean(subset(pl, lab=="sav_LP")$petiole_length),mean(subset(pl, lab=="sav_LPM")$petiole_length)) 
sid2 <- c(mean(subset(pl, lab=="sid2_NC")$petiole_length),mean(subset(pl, lab=="sid2_NCM")$petiole_length),
          mean(subset(pl, lab=="sid2_LP")$petiole_length),mean(subset(pl, lab=="sid2_LPM")$petiole_length))

pl1 <- rbind(col0,sid2,dde2,myc2,jazq,dellap,bri1,cry1cry2,sav)
colnames(pl1) <- c("NC","NCM","LP","LPM")

pdf("pl_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "black"))(n = 50)
heatmap.2(pl1, Colv=FALSE,  col=my_palette, trace = "none")
dev.off()

ln <- read.table("mutants8_leaf_number.txt", sep="\t",header=T)

col0 <- c(mean(subset(ln, lab=="col0_NC")$total.number.of.leaves),mean(subset(ln, lab=="col0_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="col0_LP")$total.number.of.leaves),mean(subset(ln, lab=="col0_LPM")$total.number.of.leaves))
bri1 <- c(mean(subset(ln, lab=="bri1_NC")$total.number.of.leaves),mean(subset(ln, lab=="bri1_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="bri1_LP")$total.number.of.leaves),mean(subset(ln, lab=="bri1_LPM")$total.number.of.leaves))
cry1cry2 <- c(mean(subset(ln, lab=="cry1cry2_NC")$total.number.of.leaves),mean(subset(ln, lab=="cry1cry2_NCM")$total.number.of.leaves),
              mean(subset(ln, lab=="cry1cry2_LP")$total.number.of.leaves),mean(subset(ln, lab=="cry1cry2_LPM")$total.number.of.leaves))
dde2 <- c(mean(subset(ln, lab=="dde2_NC")$total.number.of.leaves),mean(subset(ln, lab=="dde2_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="dde2_LP")$total.number.of.leaves),mean(subset(ln, lab=="dde2_LPM")$total.number.of.leaves))
dellap <- c(mean(subset(ln, lab=="dellap_NC")$total.number.of.leaves),mean(subset(ln, lab=="dellap_NCM")$total.number.of.leaves),
            mean(subset(ln, lab=="dellap_LP")$total.number.of.leaves),mean(subset(ln, lab=="dellap_LPM")$total.number.of.leaves))
jazq <- c(mean(subset(ln, lab=="jazq_NC")$total.number.of.leaves),mean(subset(ln, lab=="jazq_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="jazq_LP")$total.number.of.leaves),mean(subset(ln, lab=="jazq_LPM")$total.number.of.leaves))
myc2 <- c(mean(subset(ln, lab=="myc2_NC")$total.number.of.leaves),mean(subset(ln, lab=="myc2_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="myc2_LP")$total.number.of.leaves),mean(subset(ln, lab=="myc2_LPM")$total.number.of.leaves))
sav <- c(mean(subset(ln, lab=="sav_NC")$total.number.of.leaves),mean(subset(ln, lab=="sav_NCM")$total.number.of.leaves),
         mean(subset(ln, lab=="sav_LP")$total.number.of.leaves),mean(subset(ln, lab=="sav_LPM")$total.number.of.leaves)) 
sid2 <- c(mean(subset(ln, lab=="sid2_NC")$total.number.of.leaves),mean(subset(ln, lab=="sid2_NCM")$total.number.of.leaves),
          mean(subset(ln, lab=="sid2_LP")$total.number.of.leaves),mean(subset(ln, lab=="sid2_LPM")$total.number.of.leaves))

ln1 <- rbind(col0,sid2,dde2,myc2,jazq,dellap,bri1,cry1cry2,sav)
colnames(ln1) <- c("NC","NCM","LP","LPM")

pdf("ln_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "black"))(n = 50)
heatmap.2(ln1, Colv=FALSE,  col=my_palette, trace = "none")
dev.off()

ls <- read.table("mutants8_leaf_shape.txt", sep="\t",header=T)

col0 <- c(mean(subset(ls, lab=="col0_NC")$y),mean(subset(ls, lab=="col0_NCM")$y),
          mean(subset(ls, lab=="col0_LP")$y),mean(subset(ls, lab=="col0_LPM")$y))
bri1 <- c(mean(subset(ls, lab=="bri1_NC")$y),mean(subset(ls, lab=="bri1_NCM")$y),
          mean(subset(ls, lab=="bri1_LP")$y),mean(subset(ls, lab=="bri1_LPM")$y))
cry1cry2 <- c(mean(subset(ls, lab=="cry1cry2_NC")$y),mean(subset(ls, lab=="cry1cry2_NCM")$y),
              mean(subset(ls, lab=="cry1cry2_LP")$y),mean(subset(ls, lab=="cry1cry2_LPM")$y))
dde2 <- c(mean(subset(ls, lab=="dde2_NC")$y),mean(subset(ls, lab=="dde2_NCM")$y),
          mean(subset(ls, lab=="dde2_LP")$y),mean(subset(ls, lab=="dde2_LPM")$y))
dellap <- c(mean(subset(ls, lab=="dellap_NC")$y),mean(subset(ls, lab=="dellap_NCM")$y),
            mean(subset(ls, lab=="dellap_LP")$y),mean(subset(ls, lab=="dellap_LPM")$y))
jazq <- c(mean(subset(ls, lab=="jazq_NC")$y),mean(subset(ls, lab=="jazq_NCM")$y),
          mean(subset(ls, lab=="jazq_LP")$y),mean(subset(ls, lab=="jazq_LPM")$y))
myc2 <- c(mean(subset(ls, lab=="myc2_NC")$y),mean(subset(ls, lab=="myc2_NCM")$y),
          mean(subset(ls, lab=="myc2_LP")$y),mean(subset(ls, lab=="myc2_LPM")$y))
sav <- c(mean(subset(ls, lab=="sav_NC")$y),mean(subset(ls, lab=="sav_NCM")$y),
         mean(subset(ls, lab=="sav_LP")$y),mean(subset(ls, lab=="sav_LPM")$y)) 
sid2 <- c(mean(subset(ls, lab=="sid2_NC")$y),mean(subset(ls, lab=="sid2_NCM")$y),
          mean(subset(ls, lab=="sid2_LP")$y),mean(subset(ls, lab=="sid2_LPM")$y))

ls1 <- rbind(col0,sid2,dde2,myc2,jazq,dellap,bri1,cry1cry2,sav)
colnames(ls1) <- c("NC","NCM","LP","LPM")

pdf("ls_heatmap.pdf", width=5, height=5,useDingbats = F)
my_palette <- colorRampPalette(c("white", "black"))(n = 50)
heatmap.2(ls1, Colv=FALSE,  col=my_palette, trace = "none")
dev.off()






