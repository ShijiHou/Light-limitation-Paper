library(gplots)
library(ggplot2)
library(dplyr)

chip2019 <- read.table("MYC2_JA_2019.txt", header=F, sep="\t")
chip2019 <- unique(chip2019)
colnames(chip2019) <- c("x")

chip2020 <- read.table("MYC2_JA_2020.txt", header=F, sep="\t")
chip2020 <- unique(chip2020)
colnames(chip2020) <- c("x")

chip2019PNAS <- read.table("MYC2_JA_2019_PNAS.txt", header=F, sep="\t")
chip2019PNAS <- unique(chip2019PNAS)
colnames(chip2019PNAS) <- c("x")

chip <- rbind(chip2019PNAS,chip2019,chip2020)
chip <- unique(chip)

s1 <- read.table("S1.txt", header=T, sep="\t")
s2 <- read.table("S2.txt", header=T, sep="\t")
s3 <- read.table("S3.txt", header=T, sep="\t")
s4 <- read.table("S4.txt", header=T, sep="\t")
s5 <- read.table("S5.txt", header=T, sep="\t")
s6 <- read.table("S6.txt", header=T, sep="\t")
s7 <- read.table("S7.txt", header=T, sep="\t")
s8 <- read.table("S8.txt", header=T, sep="\t")

r1 <- read.table("R1.txt", header=T, sep="\t")
r2 <- read.table("R2.txt", header=T, sep="\t")
r3 <- read.table("R3.txt", header=T, sep="\t")
r4 <- read.table("R4.txt", header=T, sep="\t")
r5 <- read.table("R5.txt", header=T, sep="\t")
r6 <- read.table("R6.txt", header=T, sep="\t")
r7 <- read.table("R7.txt", header=T, sep="\t")
r8 <- read.table("R8.txt", header=T, sep="\t")

shoot <- rbind(s1,s2,s3,s4,s5,s6,s7,s8)
s <- nrow(shoot)
root <- rbind(r1,r2,r3,r4,r5,r6,r7,r8)
r <- nrow(root)

shootchip <- inner_join(shoot, chip, by="x")
shootchip <- nrow(shootchip)
rootchip  <- inner_join(root, chip, by="x")
rootchip  <- nrow(rootchip)



library(Vennerable)

Vcombo <- Venn(SetNames = c("A.thaliana", "MYC2-binding","Shoot"),
               Weight = c(0,27655,0,7973,0,2139,0,2899))

plot(Vcombo)

pdf("Figure_6d.pdf",useDingbats = F)
plot(Vcombo)
dev.off()

