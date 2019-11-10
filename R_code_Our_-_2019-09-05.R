rm(list=ls())

#library(car)
library(xlsx)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
### Declarations ###

n = 100
#n = 200
#n = 500

p = 5
#p = 15

delta = 5
#delta = 10

MC = 1000000
alpha = 0.05



### Calculate cut-off values - Old and New Methods ###

Cut_offs = function(data, alpha){

	dat = data.frame(data)
	p = ncol(data)-1
	n = nrow(data)


	### New cut_offs for Cooks_D ###

	MC = 1000000
	v = n-p

	D2 = qchisq(1-alpha, p)

	tt = rt(MC,v-1)
	T = (tt*sqrt((v)/((v-1)+tt^2)))^2
	T = sort(T)
	r2 = T[(1-alpha)*MC]

	ha = D2/(n-1) + 1/n

	ours = r2*(ha/(1-ha))/p


	### Traditional cut-offs for Cooks_D ###

	theirs = qf(0.5,p,n-p)


	### Output ###
	
	out = list(ours, theirs)
	return(out)

}



### Function for obtaining a confusion matrix ###

Conf_Mat = function(data, cut_offs, inf_obs){

	data_df = data.frame(data)
	fit = lm(Y ~ . - 1, data = data_df)
	Cooks_d = cooks.distance(fit)

	outl = which(Cooks_d > cut_offs)

	conf = matrix(c(seq(1:n)), nrow = n)
	colnames(conf) = c("n")
	conf_df = data.frame(conf)

	conf_df$pred = ""
	conf_df$pred[outl] = "*"
	
	conf_df$actual = ""
	conf_df$actual[inf_obs] = "*"

	TN = 0
	FP = 0
	FN = 0
	TP = 0

	for(i in 1:n){

		if((conf_df[i,2] == "") && (conf_df[i,3] == "")){

			TN = TN + 1
			FP = FP
			FN = FN
			TP = TP

		}else if((conf_df[i,2] == "*") && (conf_df[i,3] == "")){
		
			TN = TN
			FP = FP + 1
			FN = FN
			TP = TP

		}else if((conf_df[i,2] == "") && (conf_df[i,3] == "*")){

			TN = TN
			FP = FP
			FN = FN + 1
			TP = TP

		}else if((conf_df[i,2] == "*") && (conf_df[i,3] == "*")){

			TN = TN
			FP = FP
			FN = FN
			TP = TP + 1

		}

	}

	conf_mat = matrix(c(TN, FP, FN, TP), nrow = 2, byrow = T)
	colnames(conf_mat) = c("Pred_NO", "Pred_YES") 
	rownames(conf_mat) = c("Act_NO", "Act_YES")

	return(conf_mat)

}



### Combinded function ###

Our_method = function(data, alpha, inf_obs){
	
	cut_offs = Cut_offs(data, alpha)

	out = matrix(c(unlist(cut_offs)), ncol = 2)
	colnames(out) = c("Ours", "Theirs")

	CM = Conf_Mat(data, out[,1], inf_obs)
	CM_tr = Conf_Mat(data, out[,2], inf_obs)

	ans = list(cut_offs, CM, CM_tr)
	return(ans)

}



ptm <- proc.time()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
fi = list.files("E:/M/Projek/R Output - Ordinary/Datasets/Normal (0,1)/n=100/p=5/delta=5", full.names = T)
dataset = lapply(fi, read.csv)

n_datasets = length(dataset)

n_Out_M = matrix(c(rep(0, 2*n_datasets)), ncol=2)
co = matrix(c(rep(0, 2*n_datasets)), ncol=2)
cm = matrix(c(rep(0, 4*n_datasets)), ncol=4)
cm_tr = matrix(c(rep(0, 4*n_datasets)), ncol=4)
conclusions = matrix(c(rep(0, 4*n_datasets)), ncol=4)
conclusions_tr = matrix(c(rep(0, 4*n_datasets)), ncol=4)

for(i in 1:n_datasets){

	mydata_new = matrix(c(unlist(dataset[i])), ncol = p+2+delta)
	nc = ncol(mydata_new)

	inf_obs = matrix(c(mydata_new[1,(nc-delta+1):nc]), ncol = delta)
	colnames(inf_obs) = c(paste0("outl",1:delta))

	mydata_new = mydata_new[,-c(1,(nc-delta+1):nc)]
	colnames(mydata_new) <- c("Y",paste0("X",0:(p-1)))

	our = Our_method(mydata_new, alpha, inf_obs)
	cut_offs = matrix(c(unlist(our[1])), ncol = 2)
	colnames(cut_offs) = c("Ours", "Theirs")

	cut_offs = data.frame(cut_offs)
	co[i,] = c(cut_offs[,1], cut_offs[,2])

	CM = data.frame(our[2])
	rownames(CM) = c("Act_NO", "Act_YES")

	CM_tr = data.frame(our[3])
	rownames(CM_tr) = c("Act_NO", "Act_YES")

	TN = CM[1,1]
	FP = CM[1,2]
	FN = CM[2,1]
	TP = CM[2,2]

	cm[i,] = c(TN, FP, FN, TP)

	ERR = (FP + FN) / n
	ACC = 1 - ERR
	SN = TP / (TP + FN)
	SP = TN / (TN + FP)

	Conclusions = matrix(c(ERR, ACC, SN, SP), ncol = 4)
	colnames(Conclusions) = c("ERR", "ACC", "SN", "SP")

	conclusions[i,] = c(ERR, ACC, SN, SP)

	TN_tr = CM_tr[1,1]
	FP_tr = CM_tr[1,2]
	FN_tr = CM_tr[2,1]
	TP_tr = CM_tr[2,2]

	cm_tr[i,] = c(TN_tr, FP_tr, FN_tr, TP_tr)

	ERR_tr = (FP_tr + FN_tr) / n
	ACC_tr = 1 - ERR_tr
	SN_tr = TP_tr / (TP_tr + FN_tr)
	SP_tr = TN_tr / (TN_tr + FP_tr)

	Conclusions_tr = matrix(c(ERR_tr, ACC_tr, SN_tr, SP_tr), ncol = 4)
	colnames(Conclusions_tr) = c("ERR_tr", "ACC_tr", "SN_tr", "SP_tr")

	conclusions_tr[i,] = c(ERR_tr, ACC_tr, SN_tr, SP_tr)
	

	Our_n_inf = FP + TP
	Tr_n_inf = FP_tr + TP_tr

	n_Out = matrix(c(Our_n_inf, Tr_n_inf), ncol=2)
	colnames(n_Out) = c("Our_n_inf", "Tr_n_inf")

	n_Out_M[i,] = c(Our_n_inf, Tr_n_inf)


	Settings = matrix(c(i, MC, n, p, delta), ncol=5)
	colnames(Settings) = c("dataset", "MC", "n", "p", "delta")


	wb = createWorkbook()
	sheet = createSheet(wb, "Sheet 1")


	addDataFrame(n_Out, sheet=sheet, startRow=1, row.names=FALSE)
	addDataFrame(CM, sheet=sheet, startRow = 5, row.names=TRUE)
	addDataFrame(CM_tr, sheet=sheet, startRow = 5, startColumn = 8, row.names=TRUE)
	addDataFrame(Conclusions, sheet=sheet, startRow = 10, row.names=FALSE)
	addDataFrame(Conclusions_tr, sheet=sheet, startRow = 10, startColumn = 8, row.names=FALSE)
	addDataFrame(cut_offs, sheet=sheet, startColumn=14, row.names=FALSE)
	addDataFrame(Settings, sheet=sheet, startRow = 18, row.names=FALSE)

	saveWorkbook(wb, paste("E:/M/Projek/R Output - Ordinary/New Method/Normal (0,1)/n=100/p=5/delta=5/output", i, format(Sys.time(),"%d-%b-%Y %H.%M"), ".xlsx"))

}

n_out_Our = mean(n_Out_M[,1])
n_out_Tr = mean(n_Out_M[,2])

n_out_Our_var = var(n_Out_M[,1])
n_out_Tr_var = var(n_Out_M[,2])

n_Out = matrix(c(n_out_Our, n_out_Tr), ncol = 2)
colnames(n_Out) = c("Our_n_inf", "Tr_n_inf") 
rownames(n_Out) = c("mean") 

n_Out_var = matrix(c(n_out_Our_var, n_out_Tr_var), ncol = 2)
colnames(n_Out_var) = c("Our_n_inf", "Tr_n_inf") 
rownames(n_Out_var) = c("var") 


TN_ave = mean(cm[,1])
FP_ave = mean(cm[,2])
FN_ave = mean(cm[,3])
TP_ave = mean(cm[,4])

TN_var = var(cm[,1])
FP_var = var(cm[,2])
FN_var = var(cm[,3])
TP_var = var(cm[,4])

CM_ave = matrix(c(TN_ave, FP_ave, FN_ave, TP_ave), ncol = 2, byrow = T)
colnames(CM_ave) = c("Pred_NO", "Pred_YES") 
rownames(CM_ave) = c("Act_NO", "Act_YES")

CM_var = matrix(c(TN_var, FP_var, FN_var, TP_var), ncol = 4)
colnames(CM_var) = c("TN", "FP", "FN", "TP")
rownames(CM_var) = c("var")


ERR_ave = mean(conclusions[,1])
ACC_ave = mean(conclusions[,2])
SN_ave = mean(conclusions[,3])
SP_ave = mean(conclusions[,4])

ERR_var = var(conclusions[,1])
ACC_var = var(conclusions[,2])
SN_var = var(conclusions[,3])
SP_var = var(conclusions[,4])

Conclusions_ave = matrix(c(ERR_ave, ACC_ave, SN_ave, SP_ave), ncol = 4)
colnames(Conclusions_ave) = c("ERR", "ACC", "SN", "SP")
rownames(Conclusions_ave) = c("ave")

Conclusions_var = matrix(c(ERR_var, ACC_var, SN_var, SP_var), ncol = 4)
colnames(Conclusions_var) = c("ERR", "ACC", "SN", "SP")
rownames(Conclusions_var) = c("var")


TN_tr_ave = mean(cm_tr[,1])
FP_tr_ave = mean(cm_tr[,2])
FN_tr_ave = mean(cm_tr[,3])
TP_tr_ave = mean(cm_tr[,4])

TN_tr_var = var(cm_tr[,1])
FP_tr_var = var(cm_tr[,2])
FN_tr_var = var(cm_tr[,3])
TP_tr_var = var(cm_tr[,4])

CM_tr_ave = matrix(c(TN_tr_ave, FP_tr_ave, FN_tr_ave, TP_tr_ave), ncol = 2, byrow = T)
colnames(CM_tr_ave) = c("Pred_NO", "Pred_YES") 
rownames(CM_tr_ave) = c("Act_NO", "Act_YES")

CM_tr_var = matrix(c(TN_tr_var, FP_tr_var, FN_tr_var, TP_tr_var), ncol = 4)
colnames(CM_tr_var) = c("TN_tr", "FP_tr", "FN_tr", "TP_tr")
rownames(CM_tr_var) = c("var")

ERR_tr_ave = mean(conclusions_tr[,1])
ACC_tr_ave = mean(conclusions_tr[,2])
SN_tr_ave = mean(conclusions_tr[,3])
SP_tr_ave = mean(conclusions_tr[,4])

ERR_tr_var = var(conclusions_tr[,1])
ACC_tr_var = var(conclusions_tr[,2])
SN_tr_var = var(conclusions_tr[,3])
SP_tr_var = var(conclusions_tr[,4])

Conclusions_tr_ave = matrix(c(ERR_tr_ave, ACC_tr_ave, SN_tr_ave, SP_tr_ave), ncol = 4)
colnames(Conclusions_tr_ave) = c("ERR", "ACC", "SN", "SP")
rownames(Conclusions_tr_ave) = c("ave")

Conclusions_tr_var = matrix(c(ERR_tr_var, ACC_tr_var, SN_tr_var, SP_tr_var), ncol = 4)
colnames(Conclusions_tr_var) = c("ERR", "ACC", "SN", "SP")
rownames(Conclusions_tr_var) = c("var")


Our_cut_offs_ave = mean(co[,1])
Their_cut_offs_ave = mean(co[,2])

co_ave = matrix(c(Our_cut_offs_ave, Their_cut_offs_ave), ncol = 2)
colnames(co_ave) = c("Our_CO", "Their_CO")
rownames(co_ave) = c("ave")

Our_cut_offs_var = var(co[,1])
Their_cut_offs_var = var(co[,2])

co_var = matrix(c(Our_cut_offs_var, Their_cut_offs_var), ncol = 2)
colnames(co_var) = c("Our_CO", "Their_CO")
rownames(co_var) = c("var")


t = matrix(c(print(proc.time() - ptm)))
t = matrix(c(t[3]))
colnames(t) = c("Time")


Settings = matrix(c(n_datasets, MC, n, p, delta, t), ncol=6)
colnames(Settings) = c("n_Datasets", "MC", "n", "p", "delta", "Time")

Our = c("Our M")
Traditional = c("Traditional M")


wb = createWorkbook()
sheet = createSheet(wb, "Sheet 1")

addDataFrame(n_Out, sheet=sheet, startRow = 1, row.names=TRUE)
addDataFrame(n_Out_var, sheet=sheet, startRow = 1, startCol=8, row.names=TRUE)
addDataFrame(Our, sheet=sheet, startRow = 5, row.names=FALSE, col.names=FALSE)
addDataFrame(CM_ave, sheet=sheet, startRow=7, row.names=TRUE)
addDataFrame(CM_var, sheet=sheet, startRow=7, startCol=8, row.names=TRUE)
addDataFrame(Conclusions_ave, sheet=sheet, startRow=11, row.names=TRUE)
addDataFrame(Conclusions_var, sheet=sheet, startRow=11, startCol=8, row.names=TRUE)
addDataFrame(Traditional, sheet=sheet, startRow = 15, row.names=FALSE, col.names=FALSE)
addDataFrame(CM_tr_ave, sheet=sheet, startRow=17, row.names=TRUE)
addDataFrame(CM_tr_var, sheet=sheet, startRow=17, startCol=8, row.names=TRUE)
addDataFrame(Conclusions_tr_ave, sheet=sheet, startRow=21, row.names=TRUE)
addDataFrame(Conclusions_tr_var, sheet=sheet, startRow=21, startCol=8, row.names=TRUE)
addDataFrame(co_ave, sheet=sheet, startRow=25, row.names=TRUE)
addDataFrame(co_var, sheet=sheet, startRow=25, startCol=8, row.names=TRUE)
addDataFrame(Settings, sheet=sheet, startRow=29, row.names=FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
saveWorkbook(wb, paste("E:/M/Projek/R Output - Ordinary/New Method/Normal (0,1)/n=100/p=5/delta=5/output_ave", i, format(Sys.time(),"%d-%b-%Y %H.%M"), ".xlsx"))



