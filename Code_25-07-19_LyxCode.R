#______________________________________________________________#

#-----------------------Define n and m-------------------------#


Packages = c("csv")

for(a in Packages){

	if(!require(a,character.only = TRUE)){

		install.packages(a)
	}
	library(a,character.only = TRUE)
}

seq1 = seq(0, 10, 0.5)
lambda1 = c(0.1, seq1[-1])
lambda2 = c(0.1, seq1[-1])

n = 20
m = 20
num = 1000
iter = 1000





#______________________________________________________________#

#--------------------------Jeffreys----------------------------#


theta = rep(0, num)
ave_int_len = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
Cov_rate = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
thet = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb1 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb2 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
theta0 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
count = 0

for(i in 1:21){

	for(j in 1:21){
	
		theta0 = lambda1[i]/lambda2[j]
		int_len = rep(0, iter)
		count = 0

		for(k in 1:iter){

			x = rexp(n, lambda1[i])
			y = rexp(m, lambda2[j])
	
			lambda_1 = rgamma(num, shape = n, 
				          rate = sum(x))
			lambda_2 = rgamma(num, shape = m, 
				          rate = sum(y))

			theta = lambda_1/lambda_2
			values = sort(theta)
			low_lim = values[0.025*num]
			upp_lim = values[0.975*num]
			int_len[k] = upp_lim - low_lim

			if((low_lim < theta0) && (upp_lim > 
			theta0)){

				count = count + 1
			}else{
				count = count
			}
		}

		ave_int_len[j,i] = mean(int_len[k])
		Cov_rate[j,i] = count/iter
		thet[j,i] = theta0
		lamb1[,i] = lambda1[i]
		lamb2[j,] = lambda2[j]
	}
}

ave_int_len = round(as.vector(ave_int_len), 3)
Cov_rate = as.vector(Cov_rate)
thet = round(as.vector(thet), 3)
lamb1 = as.vector(lamb1)
lamb2 = as.vector(lamb2)

results = cbind(ave_int_len, Cov_rate, thet, lamb1, lamb2)
colnames(results) = c("Average interval length", "Coverage rate", 
"Theta", "Lambda 1", "Lambda 2")
write.csv(results, paste("C:/Users/user/Desktop/Codes/Results/
Results_Jeffreys", "n =", n, "m =", m, ".csv"))



#______________________________________________________________#

#-----------------------------GML------------------------------#


theta = rep(0, num)
ave_int_len = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
Cov_rate = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
thet = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb1 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb2 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
theta0 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
count = 0

for(i in 1:21){

	for(j in 1:21){
	
		theta0 = lambda1[i]/lambda2[j]
		int_len = rep(0, iter)
		count = 0

		for(k in 1:iter){

			x = rexp(n, lambda1[i])
			y = rexp(m, lambda2[j])
	
			lambda_1 = rgamma(num, shape = (n + 1/2), 
			rate = sum(x))
			lambda_2 = rgamma(num, shape = (m + 1/2), 
			rate = sum(y))
			theta = lambda_1/lambda_2
			values = sort(theta)
			low_lim = values[0.025*num]
			upp_lim = values[0.975*num]
			int_len[k] = upp_lim - low_lim

			if((low_lim < theta0) && (upp_lim > 
			theta0)){

				count = count + 1
			}else{
				count = count
			}
		}

		ave_int_len[j,i] = mean(int_len[k])
		Cov_rate[j,i] = count/iter
		thet[j,i] = theta0
		lamb1[,i] = lambda1[i]
		lamb2[j,] = lambda2[j]
	}
}

ave_int_len = round(as.vector(ave_int_len), 3)
Cov_rate = as.vector(Cov_rate)
thet = round(as.vector(thet), 3)
lamb1 = as.vector(lamb1)
lamb2 = as.vector(lamb2)

results = cbind(ave_int_len, Cov_rate, thet, lamb1, lamb2)
colnames(results) = c("Average interval length", "Coverage rate", 
"Theta", "Lambda 1", "Lambda 2")
write.csv(results, paste("C:/Users/user/Desktop/Codes/Results/
Results_GML", "n =", n, "m =", m, ".csv"))



#______________________________________________________________#

#---------------------------Uniform----------------------------#


theta = rep(0, num)
ave_int_len = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
Cov_rate = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
thet = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb1 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb2 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
theta0 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
count = 0

for(i in 1:21){

	for(j in 1:21){
	
		theta0 = lambda1[i]/lambda2[j]
		int_len = rep(0, iter)
		count = 0

		for(k in 1:iter){

			x = rexp(n, lambda1[i])
			y = rexp(m, lambda2[j])
	
			lambda_1 = rgamma(num, shape = (n + 1), 
			rate = sum(x))
			lambda_2 = rgamma(num, shape = (m + 1), 
			rate = sum(y))
			theta = lambda_1/lambda_2
			values = sort(theta)
			low_lim = values[0.025*num]
			upp_lim = values[0.975*num]
			int_len[k] = upp_lim - low_lim

			if((low_lim < theta0) && (upp_lim > 
			theta0)){

				count = count + 1
			}else{
				count = count
			}
		}

		ave_int_len[j,i] = mean(int_len[k])
		Cov_rate[j,i] = count/iter
		thet[j,i] = theta0
		lamb1[,i] = lambda1[i]
		lamb2[j,] = lambda2[j]
	}
}

ave_int_len = round(as.vector(ave_int_len), 3)
Cov_rate = as.vector(Cov_rate)
thet = round(as.vector(thet), 3)
lamb1 = as.vector(lamb1)
lamb2 = as.vector(lamb2)

results = cbind(ave_int_len, Cov_rate, thet, lamb1, lamb2)
colnames(results) = c("Average interval length", "Coverage rate", 
"Theta", "Lambda 1", "Lambda 2")
write.csv(results, paste("C:/Users/user/Desktop/Codes/Results/
Results_Uniform", "n =", n, "m =", m, ".csv"))



#______________________________________________________________#

#-----------------------------MDI------------------------------#


theta = rep(0, num)
ave_int_len = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
Cov_rate = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
thet = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb1 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
lamb2 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
theta0 = matrix(c(rep(0, 21*21)), ncol=21, nrow=21)
count = 0

for(i in 1:21){

	for(j in 1:21){
	
		theta0 = lambda1[i]/lambda2[j]
		int_len = rep(0, iter)
		count = 0

		for(k in 1:iter){

			x = rexp(n, lambda1[i])
			y = rexp(m, lambda2[j])
	
			lambda_1 = rgamma(num, shape = (2*n + 1), 
			rate = sum(x))
			lambda_2 = rgamma(num, shape = (2*m + 1), 
			rate = sum(y))
			theta = lambda_1/lambda_2
			values = sort(theta)
			low_lim = values[0.025*num]
			upp_lim = values[0.975*num]
			int_len[k] = upp_lim - low_lim

			if((low_lim < theta0) && (upp_lim > 
			theta0)){

				count = count + 1
			}else{
				count = count
			}
		}

		ave_int_len[j,i] = mean(int_len[k])
		Cov_rate[j,i] = count/iter
		thet[j,i] = theta0
		lamb1[,i] = lambda1[i]
		lamb2[j,] = lambda2[j]
	}
}

ave_int_len = round(as.vector(ave_int_len), 3)
Cov_rate = as.vector(Cov_rate)
thet = round(as.vector(thet), 3)
lamb1 = as.vector(lamb1)
lamb2 = as.vector(lamb2)

results = cbind(ave_int_len, Cov_rate, thet, lamb1, lamb2)
colnames(results) = c("Average interval length", "Coverage rate", 
"Theta", "Lambda 1", "Lambda 2")
write.csv(results, paste("C:/Users/user/Desktop/Codes/Results/
Results_MDI", "n =", n, "m =", m, ".csv"))


#--------------------------------------------------------------#
#______________________________________________________________#
