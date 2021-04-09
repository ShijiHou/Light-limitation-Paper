################### Fig. 2a Observed seq. variants (B) ################################# 
tab=read.table("obs_tab_bac.txt",header=T)

dev.new(width=4.489583,height=8.590617)
boxplot(tab[,1]~tab[,2],ylim=c(0,80),las=2)

for(i in 1:nrow(tab)){
  ran_add=runif(1,-0.2,0.2)
  if(tab[i,2]=="M_6"){xpos=1}
  if(tab[i,2]=="M_9"){xpos=2}
  if(tab[i,2]=="R_6"){xpos=3}
  if(tab[i,2]=="R_9"){xpos=4}
  
  points(xpos+ran_add,tab[i,1],col="grey",cex=0.75,pch=19)
  
}

#stop()

dev.new()

aa_lm=lm(tab[,1]~tab[,2])
aa_ano=aov(aa_lm)
aa_tuk=TukeyHSD(aa_ano)
par(mar=c(5,12,4,3))
plot(aa_tuk,las=2)


################### Fig. 2a Observed seq. variants (F) ################################# 

tab=read.table("obs_tab_fun.txt",header=T)

dev.new(width=4.489583,height=8.590617)
boxplot(tab[,1]~tab[,2],ylim=c(0,20),las=2)

for(i in 1:nrow(tab)){
	ran_add=runif(1,-0.2,0.2)
	if(tab[i,2]=="M_6"){xpos=1}
	if(tab[i,2]=="M_9"){xpos=2}
	if(tab[i,2]=="R_6"){xpos=3}
	if(tab[i,2]=="R_9"){xpos=4}
	
	points(xpos+ran_add,tab[i,1],col="grey",cex=0.75,pch=19)

}

#stop()

dev.new()

aa_lm=lm(tab[,1]~tab[,2])
aa_ano=aov(aa_lm)
aa_tuk=TukeyHSD(aa_ano)
par(mar=c(5,12,4,3))
plot(aa_tuk,las=2)
