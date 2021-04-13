library("gplots")
library("edgeR")

tab=read.table("otu_table_bac_leaf.txt",header=T)
tab_ra=sweep(tab,2,colSums(tab),"/")
tab_l=log2(tab_ra)
tab_l[is.infinite(as.matrix(tab_l))]<- log2(0.00001)


col_1=grep("NC",colnames(tab_ra));
col_2=grep("LP",colnames(tab_ra));

zz=rev(order(rowMeans(tab_ra[,col_1])))



tax_tab=read.table("taxonomy.txt",header=F,fill=T)
tax_m=match(rownames(tab_ra),tax_tab[,1])



#### norm part

ra_mean=rowMeans(tab_ra[,])

use_rows_ra=names(which(ra_mean>=0.001))

tab_ra=tab_ra[use_rows_ra,]

col_use_a1=grep("R1",colnames(tab_ra));
col_use_a2=grep("R2",colnames(tab_ra));
col_use_a3=grep("R3",colnames(tab_ra));

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


use_col2=colorRampPalette(c("white","black"))(100)

tree_ord=read.table("tree_bac_leaf.txt",header=F)
zz=c(as.matrix(tree_ord))
xx=intersect(zz,rownames(fin_norm))
heatmap.2(as.matrix(fin_norm[xx,]),tracecol=F,col=use_col2,Rowv=F)

tax_m=match(rownames(fin_norm),tax_tab[,1])
col_1_n=grep("NC",colnames(fin_norm));
col_2_n=grep("LP",colnames(fin_norm));



mean_sc_ra=rowMeans(tab_ra[,col_2])
mean_nc_ra=rowMeans(tab_ra[,col_1])

mean_nc=rowMeans(fin_norm[,col_1_n])
mean_sc=rowMeans(fin_norm[,col_2_n])

tax_group=sort(unique(droplevels(tax_tab[tax_m,5])))




###################
### with GLM
###################

gen_list=c(rep(1,9),rep(2,9))

# xx comes from overlap with tree

tab_test=tab[xx,c(col_1,col_2)]

new_obj=DGEList(counts=tab_test,lib.size=colSums(tab_test),group=gen_list,remove.zeros=T)	 
new_obj <- calcNormFactors(new_obj)

exp_num=factor(substring(colnames(tab_test),4,5))
cond_num=factor(substring(colnames(tab_test),1,2))

design=model.matrix(~exp_num+cond_num)
rownames(design)=colnames(tab_test)

new_obj_disp=estimateGLMCommonDisp(new_obj,design,verbose=T)
new_obj_disp=estimateGLMTrendedDisp(new_obj_disp,design)
new_obj_disp=estimateGLMTagwiseDisp(new_obj_disp,design)

fit=glmFit(new_obj_disp,design)
lrt=glmLRT(fit)


dev.new()

aa=rownames(lrt$table[lrt$table$PValue<=0.05,])
aa_num=match(aa,rownames(lrt$table))
plot(aa_num,lrt$table[lrt$table$PValue<=0.05,1],xlim=c(1,30),pch=19,cex=1.5,ylim=c(-1.5,1.5))

bb=rownames(lrt$table[lrt$table$PValue>0.05,])
bb_num=match(bb,rownames(lrt$table))
points(bb_num,lrt$table[lrt$table$PValue>0.05,1],pch=19,cex=0.5,col="grey")

segments(1:30,rep(0,30),1:30,lrt$table$logFC)

