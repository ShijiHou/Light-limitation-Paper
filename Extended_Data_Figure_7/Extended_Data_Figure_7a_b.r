tab=read.table("tab_constrain_vals_fungi.txt",header=F)

par(mfrow=c(2,2))

use_lev=unique(paste(tab[,2]))


for(i in 1:length(use_lev)){
	
	for(ii in 1:2 ){
		
		if(ii==1){pat="matrix"; use_col="#f7931e"}
		if(ii==2){pat="root"; use_col="#c7b299"}		
		
		use_row=which(tab[,2]==use_lev[i] & tab[,3]==pat)
		
		col_rep=rep(use_col,length(use_row))
		
		no_sig=which(tab[use_row,5]>=0.05)
		
		col_rep[no_sig]="white"
		
		barplot(as.matrix(tab[use_row,4]),beside=T,names=tab[use_row,1],las=2,ylim=c(0,12),col=c(col_rep),space=0.25,cex.names=0.75)
		
				
	}
	
}


