library(dplyr)
setwd('C:\\Users\\environ\\Dropbox\\DOC\\여론조사')
DATA <- read.csv("20대data.csv")
colnames(DATA) = c('startdate','durringdate','enddate','area1','area2','area3','SAE_raw','MIN_raw','PEOPLE_raw',
                   'JUS_raw','UNK1_raw','UNK2_raw','UNK3_raw','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','non_res',
                   'house','press','method','ARS','N','rate','weight','age')

DATA[,which(colnames(DATA)=='SAE_raw'):which(colnames(DATA)=='UNK3')] = 
  DATA[,which(colnames(DATA)=='SAE_raw'):which(colnames(DATA)=='UNK3')] * 100

region = DATA %>% group_by(area3) %>%count %>% filter(n>=2) %>%data.frame
region = region[,1]
a = region[1]
datas = data.frame()
setwd(paste0(paste0('C:\\Users\\environ\\Dropbox\\DOC\\여론조사\\',a),'\\prob'))



names = c()
for (file in list.files()){
 temp_1 = substr(file,1,4)
 temp = read.csv(file)
 datas = rbind(datas,temp)
 names = c(names,temp_1)
}

N = length(list.files())
i=1;p=1
j=i+nrow(datas)/N-1

for (p in 1:N) {
  j=i+nrow(datas)/N-1
  assign(names[p],datas[i:j,])
  i = 1+j
}

for (p in 1:N){
  assign(paste0(names[p],'vic_1'),matrix(nrow=100,ncol=nrow(datas)/N-2))
  assign(paste0(names[p],'vic_2'),matrix(nrow=100,ncol=nrow(datas)/N-2))
}

As = c()
Bs = c()
i=1;p=1
j=i+nrow(datas)/N-1
A1 = assign(names[1],datas[i:j,])

j = 1
for(k in 1:100){
for(i in 1:(nrow(A1)-2)){
  maxs = c()
  mins = c()
  
  for(p in 1:length(names)){
  assign(paste0(names[p],'posterior'),rnorm(10000,  eval(parse(text = names[p]))[i,2],
                                            eval(parse(text = names[p]))[i,3]))
  
  maxs = c(maxs,assign(paste0(names[p],'min_'),max(eval(parse(text=paste0(names[p],'posterior'))))))
  mins = c(mins,assign(paste0(names[p],'max_'),min(eval(parse(text=paste0(names[p],'posterior'))))))
  

  }
  
  as = c()
  bs = c()

  for(p in 1:length(names)){
  
    as = c(as, mean(eval(parse(text=paste0(names[p],'posterior'))) >= max(maxs[-p])))
    bs = c(bs, mean(eval(parse(text=paste0(names[p],'posterior'))) >= min(maxs[-p])))
  
  }
  As = rbind(As,as)
  Bs = rbind(Bs,bs)
  
}
}

As[,1]