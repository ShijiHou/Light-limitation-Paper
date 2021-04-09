tab=read.table("otu_table_bacteria.txt",header=T)



colnames(tab)=gsub("dellap","della",colnames(tab))

tab_ra=sweep(tab,2,colSums(tab),"/")





col_ro_con=grep("NC.(col0).R.(4|5|7)",colnames(tab_ra))
col_ro_shade=grep("SC.(col0).R.(4|5|7)",colnames(tab_ra))
ra_ord=rev(order(rowSums(tab_ra[,c(col_ro_con,col_ro_shade)])))
best_xx=rownames(tab_ra[ra_ord[1:80],])


###### normalization


sig_tab=matrix(data=0,nrow=nrow(tab_ra),ncol=16,dimnames=list(c(rownames(tab_ra)),c("myc2_NC","sid2_NC","dde2_NC","cry_NC","bri1_NC","del_NC","sav_NC","jazq_NC","myc2_SC","sid2_SC","dde2_SC","cry_SC","bri1_SC","del_SC","sav_SC","jazq_SC")) )

sig_tab_shade=matrix(data=0,nrow=nrow(tab_ra),ncol=9,dimnames=list(c(rownames(tab_ra)),c("myc2_SC","sid2_SC","dde2_SC","cry_SC","bri1_SC","del_SC","sav_SC","jazq_SC","col0_SC")) )
sig_tab_shade_pval=matrix(data=NA,nrow=nrow(tab_ra),ncol=9,dimnames=list(c(rownames(tab_ra)),c("myc2_SC","sid2_SC","dde2_SC","cry_SC","bri1_SC","del_SC","sav_SC","jazq_SC","col0_SC")) )

mean_tab_diff=matrix(data=0,nrow=nrow(tab_ra),ncol=9,dimnames=list(c(rownames(tab_ra)),c("myc2","sid2","dde2","cry1cry2","bri1","dellap","sav","jazq","col0")) )

mean_tab_sc=matrix(data=0,nrow=nrow(tab_ra),ncol=9,dimnames=list(c(rownames(tab_ra)),c("myc2","sid2","dde2","cry1cry2","bri1","dellap","sav","jazq","col0")) )
mean_tab_nc=matrix(data=0,nrow=nrow(tab_ra),ncol=9,dimnames=list(c(rownames(tab_ra)),c("myc2","sid2","dde2","cry1cry2","bri1","dellap","sav","jazq","col0")) )

for(ii in 1:9){

	if(ii==1){
		col_use_a1=grep("SC.(myc2).R.4",colnames(tab_ra));
		col_use_a2=grep("NC.(myc2).R.4",colnames(tab_ra));

		col_use_b1=grep("SC.(myc2).R.5",colnames(tab_ra));
		col_use_b2=grep("NC.(myc2).R.5",colnames(tab_ra));

		col_use_c1=grep("SC.(myc2).R.7",colnames(tab_ra));
		col_use_c2=grep("NC.(myc2).R.7",colnames(tab_ra));
	}

	if(ii==2){
		col_use_a1=grep("SC.(sid2).R.4",colnames(tab_ra));
		col_use_a2=grep("NC.(sid2).R.4",colnames(tab_ra));

		col_use_b1=grep("SC.(sid2).R.1",colnames(tab_ra));
		col_use_b2=grep("NC.(sid2).R.1",colnames(tab_ra));

		col_use_c1=grep("SC.(sid2).R.7",colnames(tab_ra));
		col_use_c2=grep("NC.(sid2).R.7",colnames(tab_ra));
	}

	if(ii==3){
		col_use_a1=grep("SC.(dde2).R.1",colnames(tab_ra));
		col_use_a2=grep("NC.(dde2).R.1",colnames(tab_ra));

		col_use_b1=grep("SC.(dde2).R.2",colnames(tab_ra));
		col_use_b2=grep("NC.(dde2).R.2",colnames(tab_ra));

		col_use_c1=grep("SC.(dde2).R.3",colnames(tab_ra));
		col_use_c2=grep("NC.(dde2).R.3",colnames(tab_ra));
	}

	if(ii==4){
		col_use_a1=grep("SC.(cry1cry2).R.2",colnames(tab_ra));
		col_use_a2=grep("NC.(cry1cry2).R.2",colnames(tab_ra));

		col_use_b1=grep("SC.(cry1cry2).R.4",colnames(tab_ra));
		col_use_b2=grep("NC.(cry1cry2).R.4",colnames(tab_ra));

		col_use_c1=grep("SC.(cry1cry2).R.5",colnames(tab_ra));
		col_use_c2=grep("NC.(cry1cry2).R.5",colnames(tab_ra));
	}

	if(ii==5){
		col_use_a1=grep("SC.(bri1).R.3",colnames(tab_ra));
		col_use_a2=grep("NC.(bri1).R.3",colnames(tab_ra));

		col_use_b1=grep("SC.(bri1).R.4",colnames(tab_ra));
		col_use_b2=grep("NC.(bri1).R.4",colnames(tab_ra));

		col_use_c1=grep("SC.(bri1).R.6",colnames(tab_ra));
		col_use_c2=grep("NC.(bri1).R.6",colnames(tab_ra));
	}

	if(ii==6){
		col_use_a1=grep("SC.(della).R.1",colnames(tab_ra));
		col_use_a2=grep("NC.(della).R.1",colnames(tab_ra));

		col_use_b1=grep("SC.(della).R.4",colnames(tab_ra));
		col_use_b2=grep("NC.(della).R.4",colnames(tab_ra));

		col_use_c1=grep("SC.(della).R.5",colnames(tab_ra));
		col_use_c2=grep("NC.(della).R.5",colnames(tab_ra));
	}
	
	if(ii==7){
		col_use_a1=grep("SC.(sav).R.3",colnames(tab_ra));
		col_use_a2=grep("NC.(sav).R.3",colnames(tab_ra));

		col_use_b1=grep("SC.(sav).R.5",colnames(tab_ra));
		col_use_b2=grep("NC.(sav).R.5",colnames(tab_ra));

		col_use_c1=grep("SC.(sav).R.6",colnames(tab_ra));
		col_use_c2=grep("NC.(sav).R.6",colnames(tab_ra));
	}

	if(ii==8){
		col_use_a1=grep("SC.(jazq).R.5",colnames(tab_ra));
		col_use_a2=grep("NC.(jazq).R.5",colnames(tab_ra));

		col_use_b1=grep("SC.(jazq).R.6",colnames(tab_ra));
		col_use_b2=grep("NC.(jazq).R.6",colnames(tab_ra));

	}

	if(ii==9){
		col_use_a1=grep("SC.(col0).R.1",colnames(tab_ra));
		col_use_a2=grep("NC.(col0).R.1",colnames(tab_ra));

		col_use_b1=grep("SC.(col0).R.2",colnames(tab_ra));
		col_use_b2=grep("NC.(col0).R.2",colnames(tab_ra));

		col_use_c1=grep("SC.(col0).R.3",colnames(tab_ra));
		col_use_c2=grep("NC.(col0).R.3",colnames(tab_ra));

		col_use_d1=grep("SC.(col0).R.4",colnames(tab_ra));
		col_use_d2=grep("NC.(col0).R.4",colnames(tab_ra));

		col_use_e1=grep("SC.(col0).R.5",colnames(tab_ra));
		col_use_e2=grep("NC.(col0).R.5",colnames(tab_ra));

		col_use_f1=grep("SC.(col0).R.6",colnames(tab_ra));
		col_use_f2=grep("NC.(col0).R.6",colnames(tab_ra));

		col_use_g1=grep("SC.(col0).R.7",colnames(tab_ra));
		col_use_g2=grep("NC.(col0).R.7",colnames(tab_ra));

	}


	for(i in 1:nrow(tab_ra)){
	
		x=tab_ra[i,c(col_use_a1,col_use_a2)]
		norm_xa = (x-min(x))/(max(x)-min(x))
		#norm_xa=norm_xa+0.05
	
		x=tab_ra[i,c(col_use_b1,col_use_b2)]
		norm_xb = (x-min(x))/(max(x)-min(x))
		#norm_xb=norm_xb+0.05
	
		x=tab_ra[i,c(col_use_c1,col_use_c2)]
		norm_xc = (x-min(x))/(max(x)-min(x))
		#norm_xc=norm_xc+0.05

		if(ii==9){
		
			x=tab_ra[i,c(col_use_d1,col_use_d2)]
			norm_xd = (x-min(x))/(max(x)-min(x))

			x=tab_ra[i,c(col_use_e1,col_use_e2)]
			norm_xe = (x-min(x))/(max(x)-min(x))

			x=tab_ra[i,c(col_use_f1,col_use_f2)]
			norm_xf = (x-min(x))/(max(x)-min(x))

			x=tab_ra[i,c(col_use_g1,col_use_g2)]
			norm_xg = (x-min(x))/(max(x)-min(x))

			all_norm=cbind(norm_xa,norm_xb,norm_xc,norm_xd,norm_xe,norm_xf,norm_xg)


		}else if(ii==8){	

			all_norm=cbind(norm_xa,norm_xb)
		}else{
			all_norm=cbind(norm_xa,norm_xb,norm_xc)
		}
	
		if(i==1){ fin_norm=all_norm }
		else{ fin_norm=rbind(fin_norm,all_norm) }	
	
	}	

	use_row=!is.na(rowSums(fin_norm))
	fin_norm=fin_norm[use_row,]

	if(ii==1){myc2_norm=fin_norm}
	if(ii==2){sid2_norm=fin_norm}
	if(ii==3){dde2_norm=fin_norm}
	if(ii==4){cry_norm=fin_norm}
	if(ii==5){bri1_norm=fin_norm}
	if(ii==6){della_norm=fin_norm}
	if(ii==7){sav_norm=fin_norm}
	if(ii==8){jazq_norm=fin_norm}
	if(ii==9){col0_norm=fin_norm}



	### find sig ones, only in mutant between NC and SC condition

	col_ro_con=grep("(NC).(myc2|sid2|dde2|cry1cry2|bri1|della|dellap|sav|jazq|col0).R.",colnames(fin_norm))
	col_ro_shade=grep("(SC).(myc2|sid2|dde2|cry1cry2|bri1|della|dellap|sav|jazq|col0).R.",colnames(fin_norm))

	for(i in 1:nrow(fin_norm)){

		if(sum(fin_norm[i,col_ro_con])==0 | sum(fin_norm[i,col_ro_shade])==0){next}		
	


		t_res=wilcox.test(as.numeric(fin_norm[i,col_ro_con]),as.numeric(fin_norm[i,col_ro_shade]))

		if(t_res$p.value<=0.05){
			#diff_val=rowMeans(fin_norm[i,col_ro_shade])/rowMeans(fin_norm[i,col_ro_con])
			#sig_tab_shade[rownames(fin_norm)[i],ii]=log2(diff_val)

			diff_val=rowMeans(fin_norm[i,col_ro_shade])-rowMeans(fin_norm[i,col_ro_con])
			sig_tab_shade[rownames(fin_norm)[i],ii]=diff_val
			
		}
		sig_tab_shade_pval[rownames(fin_norm)[i],ii]=t_res$p.value

		mean_tab_diff[rownames(fin_norm[i,]),ii]=rowMeans(fin_norm[i,col_ro_shade])-rowMeans(fin_norm[i,col_ro_con])
		

		mean_tab_sc[rownames(fin_norm[i,]),ii]=rowMeans(fin_norm[i,col_ro_shade])

		mean_tab_nc[rownames(fin_norm[i,]),ii]=rowMeans(fin_norm[i,col_ro_con])

	}

	
	

}

mean_tab_diff2=mean_tab_diff[rowMeans(mean_tab_diff)!=0,]


###

all_common=Reduce(intersect,list(rownames(myc2_norm),rownames(sid2_norm),rownames(dde2_norm),rownames(cry_norm),rownames(bri1_norm),rownames(della_norm),rownames(sav_norm),rownames(jazq_norm),rownames(col0_norm)))

all_norm=cbind(myc2_norm[all_common,],sid2_norm[all_common,],dde2_norm[all_common,],cry_norm[all_common,],bri1_norm[all_common,],della_norm[all_common,],sav_norm[all_common,],jazq_norm[all_common,],col0_norm[all_common,])

rem_col0=grep("NC",colnames(all_norm),invert=T)

#heatmap.2(as.matrix(all_norm[,rem_col0]),tracecol=F,margin=c(8,12),Colv=T,col=use_col2)

keep_row=rowSums(mean_tab_diff)!=0
mean_tab_sc2=mean_tab_sc[keep_row,]
mean_tab_nc2=mean_tab_nc[keep_row,]

mean_tab_sc2[mean_tab_sc2==0]<-NA
mean_tab_nc2[mean_tab_nc2==0]<-NA


tax=read.table("taxonomy_bacteria.txt",header=F,fill=T)
t_match=match(rownames(mean_tab_sc2[,]),tax[,1])

par(mar=c(12,4,4,4))



 
aa=by(as.matrix(mean_tab_nc2[,]),droplevels(tax[t_match,5]),colMeans,na.rm=T)
bb=by(as.matrix(mean_tab_sc2[,]),droplevels(tax[t_match,5]),colMeans,na.rm=T)

## all values , use mean_tab_xx[,9] when only use col0 values !
boxplot(mean_tab_nc2[,9]~droplevels(tax[t_match,5]),las=2,at=seq(1.1,18,2),col="white",ylim=c(0,1),xlim=c(0,19))
boxplot(mean_tab_sc2[,9]~droplevels(tax[t_match,5]),add=T,las=2,at=seq(1.9,18,2),col="grey")
 



pos_num1=seq(1.1,18,2)
pos_num2=seq(1.9,18,2)


my_col=c("#f15a24","#8d753b","#f7931e","#29abe2","#93278f","#e2dd1a","#0071bc","#c1272d","black")

tax_group=sort(unique(droplevels(tax[t_match,5])))

for(i in 1 : length(tax_group)){



	use_row=grep(tax_group[i],tax[t_match,5])
	# skip small groups
	if(i==5 | i==4){next}
	
	for(ii in 1:9){

		#is normal distributed !		
	

		t_res=wilcox.test(as.numeric(mean_tab_nc2[use_row,ii]),as.numeric(mean_tab_sc2[use_row,ii]),na.action = "na.exclude",alternative="two.sided",exact=F)
		if(t_res$p.value <=0.05){
			points(pos_num1[i],mean(mean_tab_nc2[use_row,ii],na.rm=T),col=my_col[ii],pch=19)
			points(pos_num2[i],mean(mean_tab_sc2[use_row,ii],na.rm=T),col=my_col[ii],pch=19)
			segments(pos_num1[i],mean(mean_tab_nc2[use_row,ii],na.rm=T),pos_num2[i],mean(mean_tab_sc2[use_row,ii],na.rm=T),col=my_col[ii])
		}else{
			#segments(pos_num1[i],mean(mean_tab_nc2[use_row,ii],na.rm=T),pos_num2[i],mean(mean_tab_sc2[use_row,ii],na.rm=T),col=my_col[ii], lty="dotted")
		}

	}

	### use [use_row,9] for col0 samples 


	t_res_all=wilcox.test(as.numeric(mean_tab_nc2[use_row,9]),as.numeric(mean_tab_sc2[use_row,9]),na.action = "na.exclude",alternative="two.sided",exact=F)
	if(t_res_all$p.value <=0.05){	
		message(paste(tax_group[i],"_",t_res_all$p.value))
		points(pos_num1[i]+0.5,1,col="black",pch=8)
	}
	
}







 

