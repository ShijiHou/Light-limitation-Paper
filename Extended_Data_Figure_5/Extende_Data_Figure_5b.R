nc_shoot_up <- 955
nc_shoot_down <- 133

nc_shoot <- rbind(nc_shoot_up,nc_shoot_down)

lp_shoot_up <- 86
lp_shoot_down <- 79

lp_shoot <- rbind(lp_shoot_up,lp_shoot_down)

shoot_n_up <- 428
shoot_n_down <- 401

shoot_n <- rbind(shoot_n_up,shoot_n_down)

shoot_y_up <- 673
shoot_y_down <- 1456

shoot_y <- rbind(shoot_y_up,shoot_n_down)

nc_root_up <- 768
nc_root_down <- 401

nc_root <- rbind(nc_root_up,nc_root_down)

lp_root_up <- 973
lp_root_down <- 579

lp_root <- rbind(lp_root_up,lp_root_down)

root_n_up <- 299
root_n_down <- 511

root_n <- rbind(root_n_up,root_n_down)

root_y_up <- 463
root_y_down <- 880

root_y <- rbind(root_y_up,root_n_down)

all <- cbind(nc_shoot,lp_shoot,shoot_n,shoot_y,nc_root,lp_root,root_n,root_y)
rownames(all) <- c("UP", "DOWN")
colnames(all) <- c("NC_shoot","LP_shoot","shoot_N","shoot_Y","NC_root","LP_root","root_N","root_Y")

library("reshape2")

all1 <- melt(all)

library("ggplot2")

all1$Var2 <- factor(all1$Var2, levels = levels(all1$Var2)[c(5:8,1:4)], ordered = T)

p <- ggplot(data=all1, mapping=aes(x=Var2, y=value, fill=Var1))+
  geom_bar(stat="identity",width=0.5,position="stack")+
  scale_fill_manual(values=c("red","skyblue"))+
  geom_text(aes(label=value), vjust=1, color="black", size=3.5)+
  scale_x_discrete(breaks=c("NC_root","LP_root","root_N","root_Y","NC_shoot","LP_shoot","shoot_N","shoot_Y"),
                   labels=c("NC","LP","-BFO","+BFO","NC","LP","-BFO","+BFO"))+
  theme_minimal()+
  theme_bw()+theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

pdf("Extended_Data_Figure_5b.pdf", width=5, height=3, useDingbats = F)
p
dev.off()

