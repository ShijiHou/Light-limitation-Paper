library(ggplot2)

shoot_nc_up <- 1496
shoot_nc_down <- 1274
shoot_nc <- rbind(shoot_nc_up,shoot_nc_down)

shoot_ncm_up <- 326
shoot_ncm_down <- 191
shoot_ncm <- rbind(shoot_ncm_up,shoot_ncm_down)

shoot_lp_up <- 171
shoot_lp_down <- 246
shoot_lp <- rbind(shoot_lp_up,shoot_lp_down)

shoot_lpm_up <- 1514
shoot_lpm_down <- 1831
shoot_lpm <- rbind(shoot_lpm_up,shoot_lpm_down)

root_nc_up <- 728
root_nc_down <- 1180
root_nc <- rbind(root_nc_up,root_nc_down)

root_ncm_up <- 1169
root_ncm_down <- 881
root_ncm <- rbind(root_ncm_up,root_ncm_down)

root_lp_up <- 1709
root_lp_down <- 1453
root_lp <- rbind(root_lp_up,root_lp_down)

root_lpm_up <- 1018
root_lpm_down <- 1484
root_lpm <- rbind(root_lpm_up,root_lpm_down)

all <- cbind(shoot_nc,shoot_ncm,shoot_lp,shoot_lpm,root_nc,root_ncm,root_lp,root_lpm)
rownames(all) <- c("UP", "DOWN")
colnames(all) <- c("Shoot_NC","Shoot_NCM","Shoot_LP","Shoot_LPM","Root_NC","Root_NCM","Root_LP","Root_LPM")

library("reshape2")

all1 <- melt(all)

library("ggplot2")

p <- ggplot(data=all1, mapping=aes(x=Var2, y=value, fill=Var1))+
  geom_bar(stat="identity",width=0.5,position="stack")+
  scale_fill_manual(values=c("red","skyblue"))+
  geom_text(aes(label=value), vjust=1, color="black", size=3.5)+
  scale_x_discrete(breaks=c("Shoot_NC","Shoot_NCM","Shoot_LP","Shoot_LPM","Root_NC","Root_NCM","Root_LP","Root_LPM"),
                   labels=c("NC","NC+M","LP","LP+M","NC","NC+M","LP","LP+M"))+
  theme_minimal()+
  theme_bw()+theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

pdf("Figure_8e.pdf", width=5, height=3, useDingbats = F)
p
dev.off()

