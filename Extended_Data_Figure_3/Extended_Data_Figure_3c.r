# need fin_norm from boxplot script

fin_norm=read.table("norm_values.txt",header=T)
tax=read.table("taxonomy.txt",header=F,fill=T)
t_match=match(rownames(fin_norm[,]),tax[,1])

col_ro_con=grep("(1|2|3)R.6.(1|2|3|4)",colnames(fin_norm))
col_ro_shade=grep("(1|2|3)R.9.(1|2|3|4)",colnames(fin_norm))

mean_nc=rowMeans(fin_norm[,col_ro_con])
mean_sc=rowMeans(fin_norm[,col_ro_shade])

par(mar=c(22,4,4,4))

# 5=18,6=44

#cat_tax=paste(tax[t_match,4],tax[t_match,5],paste(tax[t_match,6]))
cat_tax=paste(tax[t_match,3],paste(tax[t_match,5]))

#cat_tax=cat_tax[keep_tax]
#mean_sc=mean_sc[keep_tax]
#mean_nc=mean_sc[keep_tax]


boxplot(as.matrix(mean_nc)~cat_tax,las=2,at=seq(1,18,2),xlim=c(0,20),ylim=c(0,1))
boxplot(as.matrix(mean_sc)~cat_tax,las=2,at=seq(2,18,2),col="grey",add=T)

tax_group=sort(unique(droplevels(tax[t_match,5])))

for(i in 1 : length(tax_group)){

	use_row=grep(tax_group[i],tax[t_match,5])	

	#if(i==4 | i==5){next}

	t_res_all=wilcox.test(as.numeric(mean_nc[use_row]),as.numeric(mean_sc[use_row]),na.action = "na.exclude")
	if(t_res_all$p.value <=0.05){	
		message(paste(tax_group[i],"_",t_res_all$p.value))
	}
	

}
