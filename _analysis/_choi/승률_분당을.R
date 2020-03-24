# 승률파트만 떼어놓은 것 같음. 


SAE_vics1 = matrix(nrow=100,ncol=length(SAE_results_1[,1])-2)
MIN_vics1 = matrix(nrow=100,ncol=length(MIN_results_1[,1])-2)
UNK1_vics1 = matrix(nrow=100,ncol=length(UNK1_results_1[,1])-2)

SAE_vics2 = matrix(nrow=100,ncol=length(SAE_results_1[,1])-2)
MIN_vics2 = matrix(nrow=100,ncol=length(MIN_results_1[,1])-2)
UNK1_vics2 = matrix(nrow=100,ncol=length(UNK1_results_1[,1])-2)


for(j in 1:100){
for(i in 1:nrow(SAE_posterior)){
    
  SAE_posterior[i,] = rnorm(10000,SAE_results_1[i,1],SAE_results_1[i,2])
  MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])
  UNK1_posterior[i,] = rnorm(10000,UNK1_results_1[i,1],UNK1_results_1[i,2])
  
  maxs = c(max(SAE_posterior[i,]),max(MIN_posterior[i,]),max(UNK1_posterior[i,]))
  mins = c(min(SAE_posterior[i,]),min(MIN_posterior[i,]),min(UNK1_posterior[i,]))
  
  maxs_SAE = maxs[-1]
  mins_SAE = mins[-1]
  
  maxs_MIN = maxs[-2]
  mins_MIN = mins[-2]
  
  maxs_UNK1 = maxs[-3]
  mins_UNK1 = mins[-3]
  
  a = mean(SAE_posterior[i,]>=max(maxs_SAE))
  b = mean(SAE_posterior[i,]>=max(mins_SAE))
  SAE_vics1[j,i] = a
  SAE_vics2[j,i] = b
  
  a = mean(MIN_posterior[i,]>=max(maxs_MIN))
  b = mean(MIN_posterior[i,]>=max(mins_MIN))
  MIN_vics1[j,i] = a
  MIN_vics2[j,i] = b
  
  a = mean(UNK1_posterior[i,]>=max(maxs_UNK1))
  b = mean(UNK1_posterior[i,]>=max(mins_UNK1))
  UNK1_vics1[j,i] = a
  UNK1_vics2[j,i] = b
  
  }
}

A = cbind(colMeans(SAE_vics1),apply(SAE_vics1,2,quantile,0.025),apply(SAE_vics1,2,quantile,1-0.025))
B = cbind(colMeans(MIN_vics2),apply(MIN_vics2,2,quantile,0.025),apply(MIN_vics2,2,quantile,1-0.025))
C = cbind(colMeans(UNK1_vics2),apply(UNK1_vics2,2,quantile,0.025),apply(UNK1_vics2,2,quantile,1-0.025))

