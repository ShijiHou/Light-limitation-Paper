tab=read.table("observed_species.txt",header=T)

xx=grep("SF|CAS",rownames(tab),invert=F)
tab=tab[xx,]

boxplot(tab[,4]~droplevels(tab[,"soil"])+droplevels(tab[,"comp"])+droplevels(tab[,"cond"]),las=2)

stripchart(tab[,4]~droplevels(tab[,"soil"])+droplevels(tab[,"comp"])+droplevels(tab[,"cond"]),add=T,vertical=T,pch=19,cex=0.5,method="jitter",col="grey")


dev.new( width=11.601810, height=4.606335)

par(mfrow=c(1,2))

xx=grep("CAS_",rownames(tab))
aa_lm=lm(tab[xx,4]~tab[xx,5])
aa_ano=aov(aa_lm)
aa_tuk=TukeyHSD(aa_ano)
par(mar=c(5,12,4,3))
plot(aa_tuk,las=2)

xx=grep("SF_",rownames(tab))
aa_lm=lm(tab[xx,4]~tab[xx,5])
aa_ano=aov(aa_lm)
aa_tuk=TukeyHSD(aa_ano)
par(mar=c(5,12,4,3))
plot(aa_tuk,las=2)



