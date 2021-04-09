library("ggplot2")

opda_shoot_all <- read.table("opda_shoot_all_scaled.txt", sep="\t",header=T)
ja_shoot_all <- read.table("ja_shoot_all_scaled.txt", sep="\t",header=T)
jaile_shoot_all <- read.table("jaile_shoot_all_scaled.txt", sep="\t",header=T)
sa_shoot_all <- read.table("sa_shoot_all_scaled.txt", sep="\t",header=T)

opda_root_all <- read.table("opda_root_all_scaled.txt", sep="\t",header=T)
ja_root_all <- read.table("ja_root_all_scaled.txt", sep="\t",header=T)
jaile_root_all <- read.table("jaile_root_all_scaled.txt", sep="\t",header=T)
sa_root_all <- read.table("sa_root_all_scaled.txt", sep="\t",header=T)


opda_shoot_all$label <- factor(opda_shoot_all$label, levels = levels(opda_shoot_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

opda_shoot <- ggplot(opda_shoot_all,aes(x=label, y=opda_shoot_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("OPDA")+
  scale_x_discrete(breaks=c("col0_NC_Shoot","col0_NC_M_Shoot","col0_LP_Shoot","col0_LP_M_Shoot",
                            "myc2_NC_Shoot","myc2_NC_M_Shoot","myc2_LP_Shoot","myc2_LP_M_Shoot"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(opda_shoot)


ja_shoot_all$label <- factor(ja_shoot_all$label, levels = levels(ja_shoot_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

ja_shoot <- ggplot(ja_shoot_all,aes(x=label, y=ja_shoot_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("JA")+
  scale_x_discrete(breaks=c("col0_NC_Shoot","col0_NC_M_Shoot","col0_LP_Shoot","col0_LP_M_Shoot",
                            "myc2_NC_Shoot","myc2_NC_M_Shoot","myc2_LP_Shoot","myc2_LP_M_Shoot"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(ja_shoot)


jaile_shoot_all$label <- factor(jaile_shoot_all$label, levels = levels(jaile_shoot_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

jaile_shoot <- ggplot(jaile_shoot_all,aes(x=label, y=jaile_shoot_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("JAIle")+
  scale_x_discrete(breaks=c("col0_NC_Shoot","col0_NC_M_Shoot","col0_LP_Shoot","col0_LP_M_Shoot",
                            "myc2_NC_Shoot","myc2_NC_M_Shoot","myc2_LP_Shoot","myc2_LP_M_Shoot"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(jaile_shoot)

sa_shoot_all$label <- factor(sa_shoot_all$label, levels = levels(sa_shoot_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

sa_shoot <- ggplot(sa_shoot_all,aes(x=label, y=sa_shoot_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("SA")+
  scale_x_discrete(breaks=c("col0_NC_Shoot","col0_NC_M_Shoot","col0_LP_Shoot","col0_LP_M_Shoot",
                            "myc2_NC_Shoot","myc2_NC_M_Shoot","myc2_LP_Shoot","myc2_LP_M_Shoot"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(sa_shoot)



opda_root_all$label <- factor(opda_root_all$label, levels = levels(opda_root_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

opda_root <- ggplot(opda_root_all,aes(x=label, y=opda_root_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("OPDA")+
  scale_x_discrete(breaks=c("col0_NC_Root","col0_NC_M_Root","col0_LP_Root","col0_LP_M_Root",
                            "myc2_NC_Root","myc2_NC_M_Root","myc2_LP_Root","myc2_LP_M_Root"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(opda_root)


ja_root_all$label <- factor(ja_root_all$label, levels = levels(ja_root_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

ja_root <- ggplot(ja_root_all,aes(x=label, y=ja_root_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("JA")+
  scale_x_discrete(breaks=c("col0_NC_Root","col0_NC_M_Root","col0_LP_Root","col0_LP_M_Root",
                            "myc2_NC_Root","myc2_NC_M_Root","myc2_LP_Root","myc2_LP_M_Root"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(ja_root)


jaile_root_all$label <- factor(jaile_root_all$label, levels = levels(jaile_root_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

jaile_root <- ggplot(jaile_root_all,aes(x=label, y=jaile_root_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("JAIle")+
  scale_x_discrete(breaks=c("col0_NC_Root","col0_NC_M_Root","col0_LP_Root","col0_LP_M_Root",
                            "myc2_NC_Root","myc2_NC_M_Root","myc2_LP_Root","myc2_LP_M_Root"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(jaile_root)

sa_root_all$label <- factor(sa_root_all$label, levels = levels(sa_root_all$label)[c(4,3,2,1,8,7,6,5)], ordered = T)

sa_root <- ggplot(sa_root_all,aes(x=label, y=sa_root_scaled, group=label))+
  geom_boxplot(outlier.shape=NA, aes(fill=label))+
  scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  geom_jitter(position=position_jitter(width=0.1), size=1)+
  xlab("")+ylab("SA")+
  scale_x_discrete(breaks=c("col0_NC_Root","col0_NC_M_Root","col0_LP_Root","col0_LP_M_Root",
                            "myc2_NC_Root","myc2_NC_M_Root","myc2_LP_Root","myc2_LP_M_Root"),
                   labels=c("-BFO","+BFO","-BFO","+BFO","-BFO","+BFO","-BFO","+BFO"))+
  #scale_y_continuous(breaks=seq(0, 4, by=1),limits=c(0,4))+
  theme_bw()+
  theme(axis.text.x=element_text(size=10,angle=45,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(legend.position = "none")

print(sa_root)

library(ggpubr)

p <- ggarrange(opda_shoot, ja_shoot, jaile_shoot, sa_shoot, 
                opda_root, ja_root, jaile_root, sa_root, 
               ncol=4, nrow = 2)

pdf("Extended_Data_Figure_10.pdf", width=12, height=6,useDingbats = F)
p
dev.off()


###################statistics########################################################
#Test for nomal distribution using Shapiro-Wilk test
for (g in unique(ja_root_all$label)){
  x <- subset(ja_root_all, label == g, sel = c("ja_root_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Root : 0.366663234470412"
#[1] "p-value for col0_NC_M_Root : 0.448234633653659"
#[1] "p-value for col0_LP_Root : 0.347992617508266"
#[1] "p-value for col0_LP_M_Root : 0.300684089017163"
#[1] "p-value for myc2_NC_Root : 0.190740699737254"
#[1] "p-value for myc2_NC_M_Root : 0.273042321057853"
#[1] "p-value for myc2_LP_Root : 0.858020733021795"
#[1] "p-value for myc2_LP_M_Root : 0.31352105553945"

library(multcomp)
aov <- aov(ja_root_scaled ~ label,ja_root_all)
tukey <- glht(aov, linfct=mcp(label="Tukey"))
cld(tukey)
# "ac", "ab",  "bc",  "a", "d", "cd", "d", "ac"


for (g in unique(ja_shoot_all$label)){
  x <- subset(ja_shoot_all, label == g, sel = c("ja_shoot_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Shoot : 0.00442897262451328"
#[1] "p-value for col0_NC_M_Shoot : 8.85413553503328e-05"
#[1] "p-value for col0_LP_Shoot : 0.621511716515593"
#[1] "p-value for col0_LP_M_Shoot : 2.11906294726158e-05"
#[1] "p-value for myc2_NC_Shoot : 0.0736146137671346"
#[1] "p-value for myc2_NC_M_Shoot : 0.220414179884204"
#[1] "p-value for myc2_LP_Shoot : 0.177361175481859"
#[1] "p-value for myc2_LP_M_Shoot : 0.015862796805822"

library(pgirmess)
kmc <- kruskalmc(ja_shoot_scaled ~ label, data=ja_shoot_all)
kmc
library("multcompView")
tt <- kmc$dif.com$difference
names(tt) <- rownames(kmc$dif.com)
multcompLetters(tt)
# "a", "a", "a", "ab",  "b", "b", "ab", "ab" 


for (g in unique(jaile_root_all$label)){
  x <- subset(jaile_root_all, label == g, sel = c("jaile_root_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Root : 0.0121863274542654"
#[1] "p-value for col0_NC_M_Root : 0.250411693480695"
#[1] "p-value for col0_LP_Root : 0.175618864241985"
#[1] "p-value for col0_LP_M_Root : 0.00448417746045224"
#[1] "p-value for myc2_NC_Root : 0.504798503232933"
#[1] "p-value for myc2_NC_M_Root : 0.492036482937663"
#[1] "p-value for myc2_LP_Root : 0.86869405516932"
#[1] "p-value for myc2_LP_M_Root : 0.649112739853022"

library(pgirmess)
kmc <- kruskalmc(jaile_root_scaled ~ label, data=jaile_root_all)
kmc
library("multcompView")
tt <- kmc$dif.com$difference
names(tt) <- rownames(kmc$dif.com)
multcompLetters(tt)
#"bc","ab", "abc","a", "c","c","c","abc"


for (g in unique(jaile_shoot_all$label)){
  x <- subset(jaile_shoot_all, label == g, sel = c("jaile_shoot_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Shoot : 0.000168308810350504"
#[1] "p-value for col0_NC_M_Shoot : 3.39557863129803e-06"
#[1] "p-value for col0_LP_Shoot : 0.400142479388522"
#[1] "p-value for col0_LP_M_Shoot : 1.23070264531553e-05"
#[1] "p-value for myc2_NC_Shoot : 0.00396444598290596"
#[1] "p-value for myc2_NC_M_Shoot : 0.0362327643450907"
#[1] "p-value for myc2_LP_Shoot : 0.71627933070437"
#[1] "p-value for myc2_LP_M_Shoot : 0.0213441234444745"
library(pgirmess)
kmc <- kruskalmc(jaile_shoot_scaled ~ label, data=jaile_shoot_all)
kmc
library("multcompView")
tt <- kmc$dif.com$difference
names(tt) <- rownames(kmc$dif.com)
multcompLetters(tt)
#"ab","a","a","a", "b","b","ab","ab"


for (g in unique(opda_root_all$label)){
  x <- subset(opda_root_all, label == g, sel = c("opda_root_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Root : 0.220780991124228"
#[1] "p-value for col0_NC_M_Root : 0.153988891977932"
#[1] "p-value for col0_LP_Root : 0.363848531878642"
#[1] "p-value for col0_LP_M_Root : 0.281584908934856"
#[1] "p-value for myc2_NC_Root : 0.52795929886879"
#[1] "p-value for myc2_NC_M_Root : 0.610033741426817"
#[1] "p-value for myc2_LP_Root : 0.81642379228026"
#[1] "p-value for myc2_LP_M_Root : 0.969148962762531"
library(multcomp)
aov <- aov(opda_root_scaled ~ label,opda_root_all)
tukey <- glht(aov, linfct=mcp(label="Tukey"))
cld(tukey)
#"a","a","a","a","a","a","a","a"



for (g in unique(opda_shoot_all$label)){
  x <- subset(opda_shoot_all, label == g, sel = c("opda_shoot_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Shoot : 0.0201503289240554"
#[1] "p-value for col0_NC_M_Shoot : 0.0250242963573185"
#[1] "p-value for col0_LP_Shoot : 0.522371195140447"
#[1] "p-value for col0_LP_M_Shoot : 0.00432599566016421"
#[1] "p-value for myc2_NC_Shoot : 0.00235270026015867"
#[1] "p-value for myc2_NC_M_Shoot : 0.0181904657724389"
#[1] "p-value for myc2_LP_Shoot : 0.0159425967409631"
#[1] "p-value for myc2_LP_M_Shoot : 0.00102432204158094"
library(pgirmess)
kmc <- kruskalmc(opda_shoot_scaled ~ label, data=opda_shoot_all)
kmc
library("multcompView")
tt <- kmc$dif.com$difference
names(tt) <- rownames(kmc$dif.com)
multcompLetters(tt)
#"a","abc","b","abc" ,   "ac","abc","abc" , "bc" 



for (g in unique(sa_root_all$label)){
  x <- subset(sa_root_all, label == g, sel = c("sa_root_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Root : 0.344183483947116"
#[1] "p-value for col0_NC_M_Root : 0.286345829496313"
#[1] "p-value for col0_LP_Root : 0.719435265465322"
#[1] "p-value for col0_LP_M_Root : 0.0916034592332198"
#[1] "p-value for myc2_NC_Root : 0.193258115460239"
#[1] "p-value for myc2_NC_M_Root : 0.484545315004737"
#[1] "p-value for myc2_LP_Root : 0.695023452087733"
#[1] "p-value for myc2_LP_M_Root : 0.493857031342537"
library(multcomp)
aov <- aov(sa_root_scaled ~ label,sa_root_all)
tukey <- glht(aov, linfct=mcp(label="Tukey"))
cld(tukey)
#"ab" ,"ab","b", "ab", "ab" , "a", "ab", "b" 


for (g in unique(sa_shoot_all$label)){
  x <- subset(sa_shoot_all, label == g, sel = c("sa_shoot_scaled"))
  y <- shapiro.test(x[,1])
  print(paste("p-value for", g, ":", y[2]))
}
#[1] "p-value for col0_NC_Shoot : 0.00869533370911869"
#[1] "p-value for col0_NC_M_Shoot : 0.315076418028452"
#[1] "p-value for col0_LP_Shoot : 0.145662272068947"
#[1] "p-value for col0_LP_M_Shoot : 0.192732882518751"
#[1] "p-value for myc2_NC_Shoot : 0.0240595143290392"
#[1] "p-value for myc2_NC_M_Shoot : 0.830333759533193"
#[1] "p-value for myc2_LP_Shoot : 0.705912641591067"
#[1] "p-value for myc2_LP_M_Shoot : 0.771235513588866"
library(pgirmess)
kmc <- kruskalmc(sa_shoot_scaled ~ label, data=sa_shoot_all)
kmc
library("multcompView")
tt <- kmc$dif.com$difference
names(tt) <- rownames(kmc$dif.com)
multcompLetters(tt)
#"abc","ab","ac","ac",  "b",  "ab", "ab" , "c"


######JA-Ile/SA######################################################
col0_NC_jaile_shoot <- subset(jaile_shoot_all,label=="col0_NC_Shoot")
col0_NCM_jaile_shoot<- subset(jaile_shoot_all,label=="col0_NC_M_Shoot")
col0_LP_jaile_shoot <- subset(jaile_shoot_all,label=="col0_LP_Shoot")
col0_LPM_jaile_shoot<- subset(jaile_shoot_all,label=="col0_LP_M_Shoot")

col0_NC_jaile_root <- subset(jaile_root_all,label=="col0_NC_Root")
col0_NCM_jaile_root<- subset(jaile_root_all,label=="col0_NC_M_Root")
col0_LP_jaile_root <- subset(jaile_root_all,label=="col0_LP_Root")
col0_LPM_jaile_root<- subset(jaile_root_all,label=="col0_LP_M_Root")

col0_NC_sa_shoot <- subset(sa_shoot_all,label=="col0_NC_Shoot")
col0_NCM_sa_shoot<- subset(sa_shoot_all,label=="col0_NC_M_Shoot")
col0_LP_sa_shoot <- subset(sa_shoot_all,label=="col0_LP_Shoot")
col0_LPM_sa_shoot<- subset(sa_shoot_all,label=="col0_LP_M_Shoot")

col0_NC_sa_root <- subset(sa_root_all,label=="col0_NC_Root")
col0_NCM_sa_root<- subset(sa_root_all,label=="col0_NC_M_Root")
col0_LP_sa_root <- subset(sa_root_all,label=="col0_LP_Root")
col0_LPM_sa_root<- subset(sa_root_all,label=="col0_LP_M_Root")

myc2_NC_jaile_shoot <- subset(jaile_shoot_all,label=="myc2_NC_Shoot")
myc2_NCM_jaile_shoot<- subset(jaile_shoot_all,label=="myc2_NC_M_Shoot")
myc2_LP_jaile_shoot <- subset(jaile_shoot_all,label=="myc2_LP_Shoot")
myc2_LPM_jaile_shoot<- subset(jaile_shoot_all,label=="myc2_LP_M_Shoot")

myc2_NC_jaile_root <- subset(jaile_root_all,label=="myc2_NC_Root")
myc2_NCM_jaile_root<- subset(jaile_root_all,label=="myc2_NC_M_Root")
myc2_LP_jaile_root <- subset(jaile_root_all,label=="myc2_LP_Root")
myc2_LPM_jaile_root<- subset(jaile_root_all,label=="myc2_LP_M_Root")

myc2_NC_sa_shoot <- subset(sa_shoot_all,label=="myc2_NC_Shoot")
myc2_NCM_sa_shoot<- subset(sa_shoot_all,label=="myc2_NC_M_Shoot")
myc2_LP_sa_shoot <- subset(sa_shoot_all,label=="myc2_LP_Shoot")
myc2_LPM_sa_shoot<- subset(sa_shoot_all,label=="myc2_LP_M_Shoot")

myc2_NC_sa_root <- subset(sa_root_all,label=="myc2_NC_Root")
myc2_NCM_sa_root<- subset(sa_root_all,label=="myc2_NC_M_Root")
myc2_LP_sa_root <- subset(sa_root_all,label=="myc2_LP_Root")
myc2_LPM_sa_root<- subset(sa_root_all,label=="myc2_LP_M_Root")

col0_NC_shoot  <- mean(col0_NC_jaile_shoot[,2])/mean(col0_NC_sa_shoot[,2])
col0_NCM_shoot <- mean(col0_NCM_jaile_shoot[,2])/mean(col0_NCM_sa_shoot[,2])
col0_LP_shoot  <- mean(col0_LP_jaile_shoot[,2])/mean(col0_LP_sa_shoot[,2])
col0_LPM_shoot <- mean(col0_LPM_jaile_shoot[,2])/mean(col0_LPM_sa_shoot[,2])

col0_NC_root  <- mean(col0_NC_jaile_root[,2])/mean(col0_NC_sa_root[,2])
col0_NCM_root <- mean(col0_NCM_jaile_root[,2])/mean(col0_NCM_sa_root[,2])
col0_LP_root  <- mean(col0_LP_jaile_root[,2])/mean(col0_LP_sa_root[,2])
col0_LPM_root <- mean(col0_LPM_jaile_root[,2])/mean(col0_LPM_sa_root[,2])

myc2_NC_shoot  <- mean(myc2_NC_jaile_shoot[,2])/mean(myc2_NC_sa_shoot[,2])
myc2_NCM_shoot <- mean(myc2_NCM_jaile_shoot[,2])/mean(myc2_NCM_sa_shoot[,2])
myc2_LP_shoot  <- mean(myc2_LP_jaile_shoot[,2])/mean(myc2_LP_sa_shoot[,2])
myc2_LPM_shoot <- mean(myc2_LPM_jaile_shoot[,2])/mean(myc2_LPM_sa_shoot[,2])

myc2_NC_root  <- mean(myc2_NC_jaile_root[,2])/mean(myc2_NC_sa_root[,2])
myc2_NCM_root <- mean(myc2_NCM_jaile_root[,2])/mean(myc2_NCM_sa_root[,2])
myc2_LP_root  <- mean(myc2_LP_jaile_root[,2])/mean(myc2_LP_sa_root[,2])
myc2_LPM_root <- mean(myc2_LPM_jaile_root[,2])/mean(myc2_LPM_sa_root[,2])

ratio_jaile_sa_shoot <- rbind(col0_NC_shoot,col0_NCM_shoot,col0_LP_shoot,col0_LPM_shoot,
                          myc2_NC_shoot,myc2_NCM_shoot,myc2_LP_shoot,myc2_LPM_shoot)
label1 <- rownames(ratio_jaile_sa_shoot)
ratio_jaile_sa_shoot <- cbind(label1,ratio_jaile_sa_shoot)
ratio_jaile_sa_shoot <- as.data.frame(ratio_jaile_sa_shoot)
colnames(ratio_jaile_sa_shoot) <- c("label","ratio")
ratio_jaile_sa_shoot$ratio <- as.numeric(levels(ratio_jaile_sa_shoot$ratio))[ratio_jaile_sa_shoot$ratio]
light <- c("NC","NC","LP","LP","NC","NC","LP","LP")
ratio_jaile_sa_shoot <- cbind(ratio_jaile_sa_shoot,light)

ratio_jaile_sa_root <- rbind(col0_NC_root,col0_NCM_root,col0_LP_root,col0_LPM_root,
                         myc2_NC_root,myc2_NCM_root,myc2_LP_root,myc2_LPM_root)
label2 <- rownames(ratio_jaile_sa_root)
ratio_jaile_sa_root <- cbind(label2,ratio_jaile_sa_root)
ratio_jaile_sa_root <- as.data.frame(ratio_jaile_sa_root)
colnames(ratio_jaile_sa_root) <- c("label","ratio")
ratio_jaile_sa_root$ratio <- as.numeric(levels(ratio_jaile_sa_root$ratio))[ratio_jaile_sa_root$ratio]
ratio_jaile_sa_root <- cbind(ratio_jaile_sa_root,light)

ratio_jaile_sa_shoot$label <- factor(ratio_jaile_sa_shoot$label, 
                                     levels = levels(ratio_jaile_sa_shoot$label)[c(3,4,1,2,7,8,5,6)], ordered = T)

p1 <- ggplot(ratio_jaile_sa_shoot, aes(x=label, y=ratio, group=label)) +
  geom_bar(stat="identity", position="stack",aes(fill=light)) +
  #scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  xlab("")+ylab("Ratio (JA-Ile/SA)")+
  #geom_text(aes(label=rate))+
  scale_x_discrete(breaks=c("col0_NC_shoot","col0_NCM_shoot","col0_LP_shoot","col0_LPM_shoot","myc2_NC_shoot","myc2_NCM_shoot","myc2_LP_shoot","myc2_LPM_shoot"))+
  scale_y_continuous(breaks=seq(0, 5, by=1),limits=c(0,5.5))+
  theme_bw()+
  theme(axis.text.x=element_text(size=8,angle=30,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+ 
  theme(legend.position = "none")


p1

ratio_jaile_sa_root$label <- factor(ratio_jaile_sa_root$label, 
                                     levels = levels(ratio_jaile_sa_root$label)[c(3,4,1,2,7,8,5,6)], ordered = T)

p2 <- ggplot(ratio_jaile_sa_root, aes(x=label, y=ratio, group=label)) +
  geom_bar(stat="identity", position="stack",aes(fill=light)) +
  #scale_fill_manual(values=c("white","white","gray48","gray48","white","white","gray48","gray48"))+
  xlab("")+ylab("Ratio (JA-Ile/SA)")+
  #geom_text(aes(label=rate))+
  scale_x_discrete(breaks=c("col0_NC_root","col0_NCM_root","col0_LP_root","col0_LPM_root","myc2_NC_root","myc2_NCM_root","myc2_LP_root","myc2_LPM_root"))+
  scale_y_continuous(breaks=seq(0, 5, by=1),limits=c(0,5.5))+
  theme_bw()+
  theme(axis.text.x=element_text(size=8,angle=30,hjust=1))+
  theme(axis.text.y = element_text(size=8,angle=90,hjust=0.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+ 
  theme(axis.text.y=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+ 
  theme(legend.position = "none")


p2

library(ggpubr)

p3 <- ggarrange(p1, p2,
               ncol=2, nrow = 1)

pdf("Ratio_JAIle_SA_.pdf", width=14, height=5,useDingbats = F)
p3
dev.off()

