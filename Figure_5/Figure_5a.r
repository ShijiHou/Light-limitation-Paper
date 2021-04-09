### needs two libraries

library("gplots")
library("edgeR")

tab=read.table("otu_table_bacteria.txt",header=T)

colnames(tab)=gsub("dellap","della",colnames(tab))

tree_ord=read.table("tree_ord_fig5.txt")
xx=intersect(as.matrix(tree_ord),rownames(tab))

sig_tab_glm=matrix(data=0,nrow=nrow(tab),ncol=9,dimnames=list(c(rownames(tab)),c("myc2_SC","sid2_SC","dde2_SC","cry_SC","bri1_SC","del_SC","sav_SC","jazq_SC","col0_SC")) )

fc_tab_glm=matrix(data=0,nrow=nrow(tab),ncol=9,dimnames=list(c(rownames(tab)),c("myc2_SC","sid2_SC","dde2_SC","cry_SC","bri1_SC","del_SC","sav_SC","jazq_SC","col0_SC")) )

for(i in 1:9){

	if(i ==1){
		col_test=grep("SC.myc2.R.(4|5|7)",colnames(tab))
		col_test2=grep("NC.myc2.R.(4|5|7)",colnames(tab))
		pos_x=11
	}

	if(i ==2){
		col_test=grep("SC.sid2.R.(1|4|7)",colnames(tab))
		col_test2=grep("NC.sid2.R.(1|4|7)",colnames(tab))
		pos_x=11
	}
	
	if(i ==3){
		col_test=grep("SC.dde2.R.(1|2|3)",colnames(tab))
		col_test2=grep("NC.dde2.R.(1|2|3)",colnames(tab))
		pos_x=11
	}

	if(i ==4){
		col_test=grep("SC.cry1cry2.R.(2|4|5)",colnames(tab))
		col_test2=grep("NC.cry1cry2.R.(2|4|5)",colnames(tab))
		pos_x=15
	}

	if(i ==5){
		col_test=grep("SC.bri1.R.(3|4|6)",colnames(tab))
		col_test2=grep("NC.bri1.R.(3|4|6)",colnames(tab))
		pos_x=11
	}
	
	if(i ==6){ ##### see dellap / della
		col_test=grep("SC.(della).R.(1|4|5)",colnames(tab))
		col_test2=grep("NC.(della).R.(1|4|5)",colnames(tab))
		pos_x=12
	}

	if(i ==7){
		col_test=grep("SC.sav.R.(3|5|6)",colnames(tab))
		col_test2=grep("NC.sav.R.(3|5|6)",colnames(tab))
		pos_x=10
	}

	if(i ==8){
		col_test=grep("SC.jazq.R.(5|6)",colnames(tab))
		col_test2=grep("NC.jazq.R.(5|6)",colnames(tab))
		pos_x=11
	}
	
	if(i ==9){
		col_test=grep("SC.col0.R.",colnames(tab))
		col_test2=grep("NC.col0.R.",colnames(tab))
		pos_x=11
	}

	gen_list=c(rep(1,length(col_test)),rep(2,length(col_test2)))

	# xx comes from overlap with tree

	tab_test=tab[xx,c(col_test,col_test2)]

	new_obj=DGEList(counts=tab_test,lib.size=colSums(tab_test),group=gen_list,remove.zeros=T)	 
	new_obj <- calcNormFactors(new_obj)

	exp_num=factor(substring(colnames(tab_test),pos_x,pos_x))
	cond_num=factor(substring(colnames(tab_test),1,2))
	design=model.matrix(~exp_num+cond_num)
	rownames(design)=colnames(tab_test)

	new_obj_disp=estimateGLMCommonDisp(new_obj,design,verbose=F)
	new_obj_disp=estimateGLMTrendedDisp(new_obj_disp,design)
	new_obj_disp=estimateGLMTagwiseDisp(new_obj_disp,design)

	fit=glmFit(new_obj_disp,design)
	lrt=glmLRT(fit)


	sig_ones=rownames(lrt$table[lrt$table$PValue <=0.05,])
	

	sig_tab_glm[sig_ones,i]=1

	fc_tab_glm[rownames(lrt$table),i]=lrt$table[,1]

}




#### make heatmap

use_col=colorRampPalette(c("dark blue","blue","blue","white","red","red","brown"))(100)

bb=sig_tab_glm[xx,]
bb[bb==1]="*"
bb[bb==0]=NA
heatmap.2(as.matrix(fc_tab_glm[xx,]),tracecol=F,col=use_col,Rowv=F,Colv=T,cellnote=bb[,],notecol="black")

dev.new()

barplot(rowMeans(fc_tab_glm[rev(xx),]),las=2,horiz=F,cex.names=0.5)


