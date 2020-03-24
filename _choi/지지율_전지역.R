setwd('/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_choi')

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
# ggplot ?ѱ۱??? ????
library(extrafont)
# font_import()
font_name = "NanumGothic"
par(family="NanumGothic")


## Data import

for(j in 1:2){
  setwd('/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_choi')
  
DATA <- read.csv("20대data.csv")
colnames(DATA) = c('startdate','durringdate','enddate','area1','area2','area3','SAE_raw','MIN_raw','PEOPLE_raw',
                   'JUS_raw','UNK1_raw','UNK2_raw','UNK3_raw','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','non_res',
                   'house','press','method','ARS','N','rate','weight','age')

DATA[,which(colnames(DATA)=='SAE_raw'):which(colnames(DATA)=='UNK3')] = 
  DATA[,which(colnames(DATA)=='SAE_raw'):which(colnames(DATA)=='UNK3')] * 100

region = DATA %>% group_by(area3) %>%count %>% filter(n>=2) %>%data.frame


a = region$area3[j]
dir.create(paste0(a))

data = DATA %>% filter(area3==a) ## ???? ????

data$startdate <- as.Date(as.character(data$startdate))
data$durringdate <- as.Date(as.character(data$durringdate))
data$enddate <- as.Date(as.character(data$enddate))
data = arrange(data,data$startdate)

head(data)

## Initialization

START_DATE <- as.Date(as.character("2016-03-15"))
END_DATE <- as.Date(as.character("2016-04-07"))

MU_START_SAE = data %>% group_by(enddate) %>% summarise(a = mean(SAE_raw))
MU_START_SAE <- MU_START_SAE$a[1] # ???????? ?ʱⰪ

MU_START_MIN = data %>% group_by(enddate) %>% summarise(a = mean(MIN_raw))
MU_START_MIN <- MU_START_MIN$a[1] # ???ִ? ?ʱⰪ

MU_START_PEOPLE = data %>% group_by(enddate) %>% summarise(a = mean(PEOPLE_raw))
MU_START_PEOPLE <- MU_START_PEOPLE$a[1] # ?????Ǵ? ?ʱⰪ

MU_START_JUS = data %>% group_by(enddate) %>% summarise(a = mean(JUS_raw))
MU_START_JUS <- MU_START_JUS$a[1] # ��?Ǵ? ?ʱⰪ

MU_START_UNK1 = data %>% group_by(enddate) %>% summarise(a = mean(UNK1_raw))
MU_START_UNK1 <- MU_START_UNK1$a[1] # ???Ҽ? 1 ?ʱⰪ

MU_START_UNK2 = data %>% group_by(enddate) %>% summarise(a = mean(UNK2_raw))
MU_START_UNK2 <- MU_START_UNK2$a[1] # ???Ҽ? 2 ?ʱⰪ

MU_START_UNK3 = data %>% group_by(enddate) %>% summarise(a = mean(UNK3_raw))
MU_START_UNK3 <- MU_START_UNK3$a[1] # ???Ҽ? 3 ?ʱⰪ


MU_FINISH_SAE = data %>% group_by(enddate) %>% summarise(a = mean(SAE_raw))
MU_FINISH_SAE <- MU_FINISH_SAE$a[length(MU_FINISH_SAE$a)] # ???ִ? ????????

MU_FINISH_MIN = data %>% group_by(enddate) %>% summarise(a = mean(MIN_raw))
MU_FINISH_MIN <- MU_FINISH_MIN$a[length(MU_FINISH_MIN$a)] # ???ִ? ?????? ??

MU_FINISH_PEOPLE = data %>% group_by(enddate) %>% summarise(a = mean(PEOPLE_raw))
MU_FINISH_PEOPLE <- MU_FINISH_PEOPLE$a[length(MU_FINISH_PEOPLE$a)] # ?????Ǵ? ????????

MU_FINISH_JUS = data %>% group_by(enddate) %>% summarise(a = mean(JUS_raw))
MU_FINISH_JUS <- MU_FINISH_JUS$a[length(MU_FINISH_JUS$a)] # ��?Ǵ? ????????

MU_FINISH_UNK1 = data %>% group_by(enddate) %>% summarise(a = mean(UNK1_raw))
MU_FINISH_UNK1 <- MU_FINISH_UNK1$a[length(MU_FINISH_UNK1$a)] # ???Ҽ? 1 ????????

MU_FINISH_UNK2 = data %>% group_by(enddate) %>% summarise(a = mean(UNK2_raw))
MU_FINISH_UNK2 <- MU_FINISH_UNK2$a[length(MU_FINISH_UNK2$a)] # ???Ҽ? 2 ????????

MU_FINISH_UNK3 = data %>% group_by(enddate) %>% summarise(a = mean(UNK3_raw))
MU_FINISH_UNK3 <- MU_FINISH_UNK3$a[length(MU_FINISH_UNK3$a)] # ???Ҽ? 3 ????????

Y_MIN <- 30 ##
Y_MAX <- 60 ## 
days_between_elections <- as.integer(diff(as.Date(c(START_DATE, END_DATE))))  + 1

# d1_SAE <- list(mu_start = MU_START_SAE, mu_finish = MU_FINISH_SAE, n_days = days_between_elections)   
# d1_MIN <- list(mu_start = MU_START_MIN, mu_finish = MU_FINISH_MIN, n_days = days_between_elections)  
# d1_JUS <- list(mu_start = MU_START_JUS, mu_finish = MU_FINISH_JUS, n_days = days_between_elections)   
# d1_PEOPLE <- list(mu_start = MU_START_PEOPLE, mu_finish = MU_FINISH_PEOPLE, n_days = days_between_elections)  
# d1_UNK1 <- list(mu_start = MU_START_UNK1, mu_finish = MU_FINISH_UNK1, n_days = days_between_elections)   
# d1_UNK2 <- list(mu_start = MU_START_UNK2, mu_finish = MU_FINISH_UNK2, n_days = days_between_elections)  
# d1_UNK3  <- list(mu_start = MU_START_UNK3, mu_finish = MU_FINISH_UNK3, n_days = days_between_elections)   


# plot_results ?? ?????? ??���� ?״??? ????

# plot_results <- function(stan_m){
#   if(class(stan_m) != "stanfit"){
#     stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")}
#   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
#   names(ex) <- 1:d1_SAE$n_days
#   
#   p <- ex %>%
#     gather(day, value) %>%
#     mutate(day = as.numeric(day),
#            day = as.Date(day, origin = START_DATE)) %>%
#     group_by(day) %>%
#     summarise(mean = mean(value),
#               upper = quantile(value, 0.975),
#               lower = quantile(value, 0.025)) %>%
#     ggplot(aes(x = day)) +
#     labs(x = "Shaded region shows a pointwise 95% credible interval.",
#          y = "Voting intention for the MJD(%)",
#          caption = "Source: Jackman's pscl R package") + 
#     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#     geom_line(aes(y = mean)) + 
#     scale_y_continuous(breaks = Y_MIN:Y_MAX, sec.axis = dup_axis(name="")) + # 30:60�� ?????? ?׷????? y???? ?? y ??????. 
#     theme(panel.grid.minor = element_blank())
#   
#   return(p)
# } 
## Method L1: House Effect in Local Election


## Method L2: No House Effect in Local Election

ac_SAE <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = SAE_raw/100,
         se_SAE_raw = sqrt(p*(1-p)/N)*100)



ac_MIN <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = MIN_raw/100,
         se_MIN_raw = sqrt(p*(1-p)/N)*100)
ac_PEOPLE <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = PEOPLE_raw/100,
         se_PEOPLE_raw = sqrt(p*(1-p)/N)*100)

ac_JUS <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = JUS_raw/100,
         se_JUS_raw = sqrt(p*(1-p)/N)*100)

ac_UNK1 <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = UNK1_raw/100,
         se_UNK1_raw = sqrt(p*(1-p)/N)*100)



ac_UNK2 <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = UNK2_raw/100,
         se_UNK2_raw = sqrt(p*(1-p)/N)*100)

ac_UNK3 <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = UNK3_raw/100,
         se_UNK3_raw = sqrt(p*(1-p)/N)*100)


d2_SAE <- list(
  mu_start = MU_START_SAE,
  mu_finish = MU_FINISH_SAE,
  n_days = days_between_elections,
  y_values = ac_SAE$SAE_raw,
  y_days = ac_SAE$MidDateNum,
  y_n = nrow(ac_SAE),
  y_se = ac_SAE$se_SAE_raw
)

d2_MIN <- list(
  mu_start = MU_START_MIN,
  mu_finish = MU_FINISH_MIN,
  n_days = days_between_elections,
  y_values = ac_MIN$MIN_raw,
  y_days = ac_MIN$MidDateNum,
  y_n = nrow(ac_MIN),
  y_se = ac_MIN$se_MIN_raw
)

d2_JUS<- list(
  mu_start = MU_START_JUS,
  mu_finish = MU_FINISH_JUS,
  n_days = days_between_elections,
  y_values = ac_JUS$JUS_raw,
  y_days = ac_JUS$MidDateNum,
  y_n = nrow(ac_JUS),
  y_se = ac_JUS$se_JUS_raw
)

d2_PEOPLE <- list(
  mu_start = MU_START_PEOPLE,
  mu_finish = MU_FINISH_PEOPLE,
  n_days = days_between_elections,
  y_values = ac_PEOPLE$PEOPLE_raw,
  y_days = ac_PEOPLE$MidDateNum,
  y_n = nrow(ac_PEOPLE),
  y_se = ac_PEOPLE$se_PEOPLE_raw
) 

d2_UNK1 <- list(
  mu_start = MU_START_UNK1,
  mu_finish = MU_FINISH_UNK1,
  n_days = days_between_elections,
  y_values = ac_UNK1$UNK1_raw,
  y_days = ac_UNK1$MidDateNum,
  y_n = nrow(ac_UNK1),
  y_se = ac_UNK1$se_UNK1_raw
)

d2_UNK2 <- list(
  mu_start = MU_START_UNK2,
  mu_finish = MU_FINISH_UNK2,
  n_days = days_between_elections,
  y_values = ac_UNK2$UNK2_raw,
  y_days = ac_UNK2$MidDateNum,
  y_n = nrow(ac_UNK2),
  y_se = ac_UNK2$se_UNK2_raw
) 

d2_UNK3 <- list(
  mu_start = MU_START_UNK3,
  mu_finish = MU_FINISH_UNK3,
  n_days = days_between_elections,
  y_values = ac_UNK3$UNK3_raw,
  y_days = ac_UNK3$MidDateNum,
  y_n = nrow(ac_UNK3),
  y_se = ac_UNK3$se_UNK3_raw
) 


  tryCatch({stan_mod2_SAE <- stan(file = 'oz-polls-2.stan', data = d2_SAE,
                        control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_MIN <- stan(file = 'oz-polls-2.stan', data = d2_MIN,
                        control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_PEOPLE <- stan(file = 'oz-polls-2.stan', data = d2_PEOPLE,
                           control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_JUS <- stan(file = 'oz-polls-2.stan', data = d2_JUS,
                        control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_UNK1 <- stan(file = 'oz-polls-2.stan', data = d2_UNK1,
                         control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_UNK2 <- stan(file = 'oz-polls-2.stan', data = d2_UNK2,
                         control = list(max_treedepth = 20))})
  
  tryCatch({stan_mod2_UNK3 <- stan(file = 'oz-polls-2.stan', data = d2_UNK3,
                         control = list(max_treedepth = 20))})
   
## Prob. of Victory

setwd(paste0('/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_choi/',a))
dir.create('prob')
DATE = seq(START_DATE,END_DATE,1)
DATE = DATE[-length(DATE)]

if(length(dim(stan_mod2_SAE)>0)){
  SAE_results = summary(stan_mod2_SAE)
  SAE_chains = dim(SAE_results$c_summary)[3]
  SAE_results_1 = SAE_results$c_summary[,,SAE_chains]
  SAE_vics = matrix(nrow=100,ncol=length(SAE_results_1[,1])-2)
  SAE_posterior = matrix(nrow=length(SAE_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(SAE_posterior)){
    SAE_posterior[i,] = rnorm(10000,SAE_results_1[i,1],SAE_results_1[i,2])}
  SAE_Final_Results = cbind(rowMeans(SAE_posterior),apply(SAE_posterior,1,min),apply(SAE_posterior,1,max))
  SAE_Final_Results = data.frame(DATE,SAE_Final_Results)
  colnames(SAE_Final_Results) = c('??¥','????��','????','????')
  write.csv(SAE_Final_Results,file=paste0(paste0('SAE_final_Results_',a),'.csv'))
  write.csv(SAE_results_1,file=paste0('prob/',paste0(paste0('SAE_results_1_',a),'.csv'))) }
  
  
  ## ?????Ǵ? ????�� ?ð迭
  
if(length(dim(stan_mod2_MIN)>0)){
  MIN_results = summary(stan_mod2_MIN)
  MIN_chains = dim(MIN_results$c_summary)[3]
  MIN_results_1 = MIN_results$c_summary[,,MIN_chains]
  MIN_vics = matrix(nrow=100,ncol=length(MIN_results_1[,1])-2)
  MIN_posterior = matrix(nrow=length(MIN_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(MIN_posterior)){
    MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])}
  MIN_Final_Results = cbind(rowMeans(MIN_posterior),apply(MIN_posterior,1,min),apply(MIN_posterior,1,max))
  MIN_Final_Results = data.frame(DATE,MIN_Final_Results)
  colnames(MIN_Final_Results) = c('??¥','????��','????','????')
  write.csv(MIN_Final_Results,file=paste0(paste0('MIN_final_Results_',a),'.csv'))
  write.csv(MIN_results_1,file=paste0('prob/',paste0(paste0('MIN_results_1_',a),'.csv'))) }
  
if(length(dim(stan_mod2_PEOPLE)>0)){
  PEOPLE_results = summary(stan_mod2_PEOPLE)
  PEOPLE_chains = dim(PEOPLE_results$c_summary)[3]
  PEOPLE_results_1 = PEOPLE_results$c_summary[,,PEOPLE_chains]
  PEOPLE_vics = matrix(nrow=100,ncol=length(PEOPLE_results_1[,1])-2)
  PEOPLE_posterior = matrix(nrow=length(PEOPLE_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(PEOPLE_posterior)){
    PEOPLE_posterior[i,] = rnorm(10000,PEOPLE_results_1[i,1],PEOPLE_results_1[i,2])}
  PEOPLE_Final_Results = cbind(rowMeans(PEOPLE_posterior),apply(PEOPLE_posterior,1,min),apply(PEOPLE_posterior,1,max))
  PEOPLE_Final_Results = data.frame(DATE,PEOPLE_Final_Results)
  colnames(PEOPLE_Final_Results) = c('??¥','????��','????','????')
  write.csv(PEOPLE_Final_Results,file=paste0(paste0('PEOPLE_final_Results_',a),'.csv'))
  write.csv(PEOPLE_results_1,file=paste0('prob/',paste0(paste0('PEOPLE_results_1_',a),'.csv'))) }
if(length(dim(stan_mod2_JUS)>0)){
  JUS_results = summary(stan_mod2_JUS)
  JUS_chains = dim(JUS_results$c_summary)[3]
  JUS_results_1 = JUS_results$c_summary[,,JUS_chains]
  JUS_vics = matrix(nrow=100,ncol=length(JUS_results_1[,1])-2)
  JUS_posterior = matrix(nrow=length(JUS_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(JUS_posterior)){
    JUS_posterior[i,] = rnorm(10000,JUS_results_1[i,1],JUS_results_1[i,2])}
  JUS_Final_Results = cbind(rowMeans(JUS_posterior),apply(JUS_posterior,1,min),apply(JUS_posterior,1,max))
  JUS_Final_Results = data.frame(DATE,JUS_Final_Results)
  colnames(JUS_Final_Results) = c('??¥','????��','????','????')
  write.csv(JUS_Final_Results,file=paste0(paste0('JUS_final_Results_',a),'.csv'))
  write.csv(JUS_results_1,file=paste0('prob/',paste0(paste0('JUS_results_1_',a),'.csv'))) }

if(length(dim(stan_mod2_UNK1)>0)){
  UNK1_results = summary(stan_mod2_UNK1)
  UNK1_chains = dim(UNK1_results$c_summary)[3]
  UNK1_results_1 = UNK1_results$c_summary[,,UNK1_chains]
  UNK1_vics = matrix(nrow=100,ncol=length(UNK1_results_1[,1])-2)
  UNK1_posterior = matrix(nrow=length(UNK1_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(UNK1_posterior)){
    UNK1_posterior[i,] = rnorm(10000,UNK1_results_1[i,1],UNK1_results_1[i,2])}
  UNK1_Final_Results = cbind(rowMeans(UNK1_posterior),apply(UNK1_posterior,1,min),apply(UNK1_posterior,1,max))
  UNK1_Final_Results = data.frame(DATE,UNK1_Final_Results)
  colnames(UNK1_Final_Results) = c('??¥','????��','????','????')
  write.csv(UNK1_Final_Results,file=paste0(paste0('UNK1_final_Results_',a),'.csv'))
  write.csv(UNK1_results_1,file=paste0('prob/',paste0(paste0('UNK1_results_1_',a),'.csv'))) }

if(length(dim(stan_mod2_UNK2)>0)){
  UNK2_results = summary(stan_mod2_UNK2)
  UNK2_chains = dim(UNK2_results$c_summary)[3]
  UNK2_results_1 = UNK2_results$c_summary[,,UNK2_chains]
  UNK2_vics = matrix(nrow=100,ncol=length(UNK2_results_1[,1])-2)
  UNK2_posterior = matrix(nrow=length(UNK2_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(UNK2_posterior)){
    UNK2_posterior[i,] = rnorm(10000,UNK2_results_1[i,1],UNK2_results_1[i,2])}
  UNK2_Final_Results = cbind(rowMeans(UNK2_posterior),apply(UNK2_posterior,1,min),apply(UNK2_posterior,1,max))
  UNK2_Final_Results = data.frame(DATE,UNK2_Final_Results)
  colnames(UNK2_Final_Results) = c('??¥','????��','????','????')
  write.csv(UNK2_Final_Results,file=paste0(paste0('UNK2_final_Results_',a),'.csv'))
  write.csv(UNK2_results_1,file=paste0('prob/',paste0(paste0('UNK2_results_1_',a),'.csv'))) }

if(length(dim(stan_mod2_UNK3)>0)){
  UNK3_results = summary(stan_mod2_UNK3)
  UNK3_chains = dim(UNK3_results$c_summary)[3]
  UNK3_results_1 = UNK3_results$c_summary[,,UNK3_chains]
  UNK3_vics = matrix(nrow=100,ncol=length(UNK3_results_1[,1])-2)
  UNK3_posterior = matrix(nrow=length(UNK3_results_1[,1])-2,ncol=10000)
  for(i in 1:nrow(UNK3_posterior)){
    UNK3_posterior[i,] = rnorm(10000,UNK3_results_1[i,1],UNK3_results_1[i,2])}
  UNK3_Final_Results = cbind(rowMeans(UNK3_posterior),apply(UNK3_posterior,1,min),apply(UNK3_posterior,1,max))
  UNK3_Final_Results = data.frame(DATE,UNK3_Final_Results)
  colnames(UNK3_Final_Results) = c('??¥','????��','????','????')
  write.csv(UNK3_Final_Results,file=paste0(paste0('UNK3_final_Results_',a),'.csv'))
  write.csv(UNK3_results_1,file=paste0('prob/',paste0(paste0('UNK3_results_1_',a),'.csv'))) }

    
}  
