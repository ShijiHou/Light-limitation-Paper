tab=read.table("asv_table_clean.txt",header=T)

tax_tab=read.table("taxonomy_asv.txt",header=F,fill=T)

tab_ra=sweep(tab,2,colSums(tab),"/")
tab_ra_orig=tab_ra

dev.new(height=5,width=8.35)
par(mfrow=c(2,1),cex.axis=0.5,mar=c(5,4,1,1))

for(i in 1:2){

	if(i ==1){
		col_use=grep("(NC|LP)_CAS_R",colnames(tab_ra_orig))
		tab_nam="CAS Root"
	}else{
		col_use=grep("(NC|LP)_SF_R",colnames(tab_ra_orig))	
		tab_nam="SF Root"	
	}
	ra_mean=rowMeans(tab_ra_orig[,col_use])

	use_rows_ra=names(which(ra_mean>=0.001))

	tab_ra=tab_ra_orig[use_rows_ra,col_use]

	#### normalize across replicates (#3)
	col_use_a1=grep("R1|R2|R3",colnames(tab_ra));
	col_use_a2=grep("R4|R5|R6",colnames(tab_ra));
	col_use_a3=grep("R7|R8|R9",colnames(tab_ra));



	for(i in 1:nrow(tab_ra)){
	
		x=tab_ra[i,col_use_a1]
		norm_xa = (x-min(x))/(max(x)-min(x))
	
		x=tab_ra[i,col_use_a2]
		norm_xb = (x-min(x))/(max(x)-min(x))
	
		x=tab_ra[i,col_use_a3]
		norm_xc = (x-min(x))/(max(x)-min(x))	

		all_norm=cbind(norm_xa,norm_xb,norm_xc)
	
		if(i==1){ fin_norm=all_norm }
		else{ fin_norm=rbind(fin_norm,all_norm) }	
	
	}

	use_row=!is.na(rowSums(fin_norm))
	fin_norm=fin_norm[use_row,]

	tax_m=match(rownames(fin_norm),tax_tab[,1])
	col_1_n=grep("NC",colnames(fin_norm));
	col_2_n=grep("LP",colnames(fin_norm));
	
	use_tax=names(which(table(droplevels(tax_tab[tax_m,5]))>3))
	zz=!is.na(match(tax_tab[tax_m,5],use_tax))
	t_num=length(use_tax)*2
	
	boxplot(rowMeans(fin_norm[zz,col_1_n])~droplevels(tax_tab[tax_m[zz],5]),las=2,at=seq(1,t_num,2),col="white",ylab=tab_nam)
	boxplot(rowMeans(fin_norm[zz,col_2_n])~droplevels(tax_tab[tax_m[zz],5]),las=2,at=seq(2,t_num,2),col="grey",add=T)

}













