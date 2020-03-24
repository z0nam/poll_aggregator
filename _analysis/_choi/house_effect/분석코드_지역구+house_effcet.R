library(dplyr)
house =c('엠브레인퍼블릭','한국리서치','입소스','리얼미터','조원씨앤아이')
MI = c(-7.9074974, -7.0848577, -0.2675706,  1.9111784,  3.4546718)

MI = data.frame(house,MI)

house = c('리서치앤리서치','리서치뷰','조원씨앤아이','한국리서치','리얼미터','입소스','엠브레인퍼블릭')
MIN=c(-0.208405268,	6.507029785,	3.78837334,	-6.728616294,	4.746998932	,1.786660104,	-1.269675488)

MIN = data.frame(house,MIN)

house = c('한국리서치','입소스','엠브레인퍼블릭','리얼미터')
PEOPLE = c(-3.733755685,	1.868955147,	-3.299383935,	0.208236677)

PEOPLE = data.frame(house,PEOPLE)

house = c('리서치앤리서치','리서치뷰','조원씨앤아이','한국리서치','리얼미터','입소스','엠브레인퍼블릭')
JUS = c(-0.067789562,	-0.875630502,	-1.538705953,	-2.687824775,	-4.935710075,	0.237830497,	-5.776298431)

JUS = data.frame(house,JUS)

house = c('한국리서치',	'리얼미터',	'조원씨앤아이')
MINSAE = c(-0.345068231,	1.573750031,	-1.013632127)

MINSAE = data.frame(house,MINSAE)

house_effect = full_join(MI,MIN,by='house')

house_effect = full_join(house_effect,PEOPLE,by='house')
house_effect = full_join(house_effect,JUS,by='house')
house_effect = full_join(house_effect,MINSAE,by='house')




setwd('/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_choi/house_effect')

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
# ggplot 한글깨짐 대비
library(extrafont)
library(dplyr)
font_import()
font_name = "NanumGothic"
par(family="NanumGothic")


## Data import

DATA <- read.csv("2020_여론조사.csv")
colnames(DATA) = c('startdate','durringdate','enddate','area1','area2','area3',
                   'MIN','MI','press','house','method','ARS',
                   'N','rate')


DATA = left_join(DATA,house_effect,by='house')


DATA$startdate <- as.Date(as.character(DATA$startdate))
DATA$durringdate <- as.Date(as.character(DATA$durringdate))
DATA$enddate <- as.Date(as.character(DATA$enddate))
DATA = arrange(DATA,DATA$startdate)


DATA$MI.x = DATA$MI.x - DATA$MI.y
DATA$MIN.x = DATA$MIN.x - DATA$MIN.y

colnames(DATA)[which(colnames(DATA) == 'MI.x')] = 'MI'
colnames(DATA)[which(colnames(DATA) == 'MIN.x')] = 'MIN'

DATA = DATA %>% filter(!is.na(MI))

DATA = DATA %>% filter(area3 == '서울광진구을') %>% data.frame
head(DATA)

## Initialization

START_DATE <- as.Date(as.character("2020-01-25"))
END_DATE <- as.Date(as.character("2020-03-13"))

MU_START_MI = DATA %>% group_by(enddate) %>% summarise(a = mean(MI))
MU_START_MI <- MU_START_MI$a[1] # 새누리당 초기값

MU_START_MIN = DATA %>% group_by(enddate) %>% summarise(a = mean(MIN))
MU_START_MIN <- MU_START_MIN$a[1] # 민주당 초기값

MU_FINISH_MI = DATA %>% group_by(enddate) %>% summarise(a = mean(MI))
MU_FINISH_MI <- MU_FINISH_MI$a[length(MU_FINISH_MI$a)] # 민주당 마지막값

MU_FINISH_MIN = DATA %>% group_by(enddate) %>% summarise(a = mean(MIN))
MU_FINISH_MIN <- MU_FINISH_MIN$a[length(MU_FINISH_MIN$a)] # 민주당 마지막 값

Y_MIN <- 30 ##
Y_MAX <- 80 ## 
days_between_elections <- as.integer(diff(as.Date(c(START_DATE, END_DATE))))  + 1

d1_MI <- list(mu_start = MU_START_MI, mu_finish = MU_FINISH_MI, n_days = days_between_elections)   
d2_MIN <- list(mu_start = MU_START_MIN, mu_finish = MU_FINISH_MIN, n_days = days_between_elections)  

# plot_results 는 공개된 버젼을 그대로 사용

plot_results <- function(stan_m){
  if(class(stan_m) != "stanfit"){
    stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")}
  ex <- as.data.frame(rstan::extract(stan_m, "mu"))
  names(ex) <- 1:d1_MI$n_days
  
  p <- ex %>%
    gather(day, value) %>%
    mutate(day = as.numeric(day),
           day = as.Date(day, origin = START_DATE)) %>%
    group_by(day) %>%
    summarise(mean = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ggplot(aes(x = day)) +
    labs(x = "Shaded region shows a pointwise 95% credible interval.",
         y = "Voting intention for the MJD(%)",
         caption = "Source: Jackman's pscl R package") + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) + 
    scale_y_continuous(breaks = Y_MIN:Y_MAX, sec.axis = dup_axis(name="")) + # 30:60은 보여줄 그래프의 y하한 과 y 상한임. 
    theme(panel.grid.minor = element_blank())
  
  return(p)
} 
## Method L1: House Effect in Local Election


## Method L2: No House Effect in Local Election

ac_MIN <- DATA %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = MIN/100,
         se_MIN= p*(1-p)/N*100)

ac_MI <- DATA %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - START_DATE),
         p = MI/100,
         se_MI = p*(1-p)/N*100)



d2_MI <- list(
  mu_start = MU_START_MI,
  mu_finish = MU_FINISH_MI,
  n_days = days_between_elections,
  y_values = ac_MI$MI,
  y_days = ac_MI$MidDateNum,
  y_n = nrow(ac_MI),
  y_se = ac_MI$se_MI
)

d2_MIN <- list(
  mu_start = MU_START_MIN,
  mu_finish = MU_FINISH_MIN,
  n_days = days_between_elections,
  y_values = ac_MIN$MIN,
  y_days = ac_MIN$MidDateNum,
  y_n = nrow(ac_MIN),
  y_se = ac_MIN$se_MIN
)



system.time({
  stan_mod2_MI <- stan(file = 'oz-polls-2.stan', data = d2_MI,
                       control = list(max_treedepth = 20))
  
  stan_mod2_MIN <- stan(file = 'oz-polls-2.stan', data = d2_MIN,
                        control = list(max_treedepth = 20))
}) # 510 seconds// me: 473


MIN_results = summary(stan_mod2_MIN)
MIN_chains = dim(MIN_results$c_summary)[3]
MIN_results_1 = MIN_results$c_summary[,,MIN_chains]

MIN_posterior = matrix(nrow=days_between_elections,ncol=10000)
for(i in 1:nrow(MIN_posterior)){
  MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])}
MIN_Final_Results = cbind(rowMeans(MIN_posterior),apply(MIN_posterior,1,quantile,0.05)
                          ,apply(MIN_posterior,1,quantile,0.95))

DATE = seq( START_DATE,END_DATE,1)
MIN_Final_Results = data.frame(DATE,MIN_Final_Results)
colnames(MIN_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열

SAE_results = summary(stan_mod2_MI)
SAE_chains = dim(SAE_results$c_summary)[3]
SAE_results_1 = SAE_results$c_summary[,,SAE_chains]

SAE_posterior = matrix(nrow=days_between_elections,ncol=10000)
for(i in 1:nrow(SAE_posterior)){
  SAE_posterior[i,] = rnorm(10000,SAE_results_1[i,1],SAE_results_1[i,2])}
SAE_Final_Results = cbind(rowMeans(SAE_posterior),apply(SAE_posterior,1,quantile,0.05)
                          ,apply(SAE_posterior,1,quantile,0.95))

DATE = seq( START_DATE,END_DATE,1)
SAE_Final_Results = data.frame(DATE,SAE_Final_Results)
colnames(SAE_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열


A = data.frame(rowMeans(MIN_posterior - SAE_posterior),
               apply(MIN_posterior - SAE_posterior,1,min),
               apply(MIN_posterior - SAE_posterior,1,max))


MIN_vics = SAE_vics = matrix(nrow=nrow(MIN_posterior),ncol=100)

for(j in 1:100){
  for(i in 1:nrow(MIN_posterior)){
    MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])
    SAE_posterior[i,] = rnorm(10000,SAE_results_1[i,1],SAE_results_1[i,2])
    MIN_vics[i,j] = mean(MIN_posterior[i,]>SAE_posterior[i,])
    SAE_vics[i,j] = mean(SAE_posterior[i,]>MIN_posterior[i,])
  }}

MIN_VICS = cbind(rowMeans(MIN_vics),apply(MIN_vics,1,quantile,0.025),apply(MIN_vics,1,quantile,0.975))
SAE_VICS = cbind(rowMeans(SAE_vics),apply(SAE_vics,1,quantile,0.025),apply(SAE_vics,1,quantile,0.975))
write.csv(MIN_VICS,'민주당_광진구_승률.csv')
write.csv(SAE_VICS,'미래당_광진구_승률.csv')
write.csv(MIN_Final_Results,'민주당_광진구.csv')
write.csv(SAE_Final_Results,'미래당_광진구.csv')
write.csv(A,'차이_광진구.csv')
