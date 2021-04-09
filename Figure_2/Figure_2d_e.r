#### need two libraries

library("gplots")
library("edgeR")

# read otu-table 
tab=read.table("otu_table_bacteria.txt",header=T)


###### get RA ### for each sample, RA of each strain within each sample
tab_ra=sweep(tab,2,colSums(tab),"/")


##### get column number of ROOT samples
col_ro_con=grep("(1|2|3)R.6.(1|2|3|4)",colnames(tab_ra))
col_ro_shade=grep("(1|2|3)R.9.(1|2|3|4)",colnames(tab_ra))



##### order from smallest to biggest of each strains RA among all root samples
ra_ord=rev(order(rowSums(tab_ra[,c(col_ro_con,col_ro_shade)])))

##### select the biggest 80th
best_xx=rownames(tab_ra[ra_ord[1:80],])


###### from fig2 0.001 above usinf ALL samples !!!

ra_mean=rowMeans(tab_ra[,])

use_rows_ra=names(which(ra_mean>=0.001))

tab_ra=tab_ra[use_rows_ra,]



###### normalization for Root Samples, Matrix samples would be M instead of R

## grab the column number of the samples
col_use_a1=grep("1R.9.(1|2|3|4)",colnames(tab_ra));
col_use_a2=grep("1R.6.(1|2|3|4)",colnames(tab_ra));

col_use_b1=grep("2R.9.(1|2|3|4)",colnames(tab_ra));
col_use_b2=grep("2R.6.(1|2|3|4)",colnames(tab_ra));

col_use_c1=grep("3R.9.(1|2|3|4)",colnames(tab_ra));
col_use_c2=grep("3R.6.(1|2|3|4)",colnames(tab_ra));


###### normalization for each strain within the same independent biological replicate
for(i in 1:nrow(tab_ra)){
	
	x=tab_ra[i,c(col_use_a1,col_use_a2)]
	norm_xa = (x-min(x))/(max(x)-min(x))
	
	x=tab_ra[i,c(col_use_b1,col_use_b2)]
	norm_xb = (x-min(x))/(max(x)-min(x))
	
	x=tab_ra[i,c(col_use_c1,col_use_c2)]
	norm_xc = (x-min(x))/(max(x)-min(x))
	
	all_norm=cbind(norm_xa,norm_xb,norm_xc)
	
	if(i==1){ fin_norm=all_norm }
	else{ fin_norm=rbind(fin_norm,all_norm) }	
	
}

use_row=!is.na(rowSums(fin_norm))
fin_norm=fin_norm[use_row,]

fin_norm_orig=fin_norm


col_ord=order(colnames(fin_norm))

tree_ord=read.table("tree_order_bacteria.txt",header=F)
zz=c(as.matrix(tree_ord))
xx=intersect(zz,rownames(fin_norm))
my_palette <- colorRampPalette(c("white","black"))(n = 100)
heatmap.2(as.matrix(fin_norm[xx,]),trace="none", tracecol=F,Rowv=F,col=my_palette,density.info = "none")



###### for GLM #####

col_ro_con=grep("(1|2|3)R.6.(1|2|3|4)",colnames(tab_ra))
col_ro_shade=grep("(1|2|3)R.9.(1|2|3|4)",colnames(tab_ra))

# change for matrix/root, less samples in Matrix !
gen_list=c(rep(1,12),rep(2,12))

tab_test=tab[xx,c(col_ro_con,col_ro_shade)]

new_obj=DGEList(counts=tab_test,lib.size=colSums(tab_test),group=gen_list,remove.zeros=T)	 
new_obj <- calcNormFactors(new_obj)

exp_num=factor(substring(colnames(tab_test),2,3))
cond_num=factor(substring(colnames(tab_test),5,5))

# order is important !
design=model.matrix(~exp_num+cond_num)

rownames(design)=colnames(tab_test)

new_obj_disp=estimateGLMCommonDisp(new_obj,design)
new_obj_disp=estimateGLMTrendedDisp(new_obj_disp,design)
new_obj_disp=estimateGLMTagwiseDisp(new_obj_disp,design)

# likelihood ratio test 
fit=glmFit(new_obj_disp,design)
lrt=glmLRT(fit)




dev.new()

# make dot plot
# first sig ones
aa=rownames(lrt$table[lrt$table$PValue<=0.05,])
aa_num=match(aa,rownames(lrt$table))
plot(aa_num,lrt$table[lrt$table$PValue<=0.05,1],xlim=c(1,51),pch=19,cex=1.5,ylim=c(-1.5,1.5))

bb=rownames(lrt$table[lrt$table$PValue>0.05,])
bb_num=match(bb,rownames(lrt$table))
points(bb_num,lrt$table[lrt$table$PValue>0.05,1],pch=19,cex=0.5,col="grey")

segments(1:51,rep(0,51),1:51,lrt$table$logFC)




