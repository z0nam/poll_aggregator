# setwd('C:\\Users\\environ\\Dropbox\\DOC\\여론조사')
setwd("_choi/")

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
# ggplot 한글깨짐 대비
library(extrafont)
# font_import()
# font_name = "NanumGothic"
# par(family="NanumGothic")
theme_update(text=element_text(family="NanumGothic"))


## Data import

data <- read.csv("20대data.csv")
colnames(data) = c('startdate','durringdate','enddate','area1','area2','area3','SAE_raw','MIN_raw','PEOPLE_raw',
                   'JUS_raw','UNK1_raw','UNK2_raw','UNK3_raw','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','non_res',
                   'house','press','method','ARS','N','rate','weight','age')

data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] = 
  data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] * 100

data = data %>% filter(area3=='서울종로구') ## 지역 선택

data$startdate <- as.Date(as.character(data$startdate))
data$durringdate <- as.Date(as.character(data$durringdate))
data$enddate <- as.Date(as.character(data$enddate))
data = arrange(data,data$startdate)

head(data)

## Initialization

START_DATE <- as.Date(as.character("2016-03-15"))
END_DATE <- as.Date(as.character("2016-04-07"))

MU_START_SAE = data %>% group_by(enddate) %>% summarise(a = mean(SAE_raw))
MU_START_SAE <- MU_START_SAE$a[1] # 새누리당 초기값

MU_START_MIN = data %>% group_by(enddate) %>% summarise(a = mean(MIN_raw))
MU_START_MIN <- MU_START_MIN$a[1] # 민주당 초기값

MU_FINISH_SAE = data %>% group_by(enddate) %>% summarise(a = mean(SAE_raw))
MU_FINISH_SAE <- MU_FINISH_SAE$a[length(MU_FINISH_SAE$a)] # 민주당 마지막값

MU_FINISH_MIN = data %>% group_by(enddate) %>% summarise(a = mean(MIN_raw))
MU_FINISH_MIN <- MU_FINISH_MIN$a[length(MU_FINISH_MIN$a)] # 민주당 마지막 값

Y_MIN <- 30 ##
Y_MAX <- 60 ## 
days_between_elections <- as.integer(diff(as.Date(c(START_DATE, END_DATE))))  + 1

d1_SAE <- list(mu_start = MU_START_SAE, mu_finish = MU_FINISH_SAE, n_days = days_between_elections)   
d2_MIN <- list(mu_start = MU_START_MIN, mu_finish = MU_FINISH_MIN, n_days = days_between_elections)  

# plot_results 는 공개된 버젼을 그대로 사용

plot_results <- function(stan_m){
  if(class(stan_m) != "stanfit"){
    stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")}
  ex <- as.data.frame(rstan::extract(stan_m, "mu"))
  names(ex) <- 1:d1_SAE$n_days
  
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



system.time({
  stan_mod2_SAE <- stan(file = 'oz-polls-2.stan', data = d2_SAE,
                    control = list(max_treedepth = 20))
  
  stan_mod2_MIN <- stan(file = 'oz-polls-2.stan', data = d2_MIN,
                    control = list(max_treedepth = 20))
}) # 510 seconds// me: 473

plot_results(stan_mod2_SAE) +
  geom_point(data = ac_SAE, aes(x = MidDate, y = SAE_raw)) +
  ggtitle("Voting intention for the Minju Party between the 2016 Boondang-eul elections",
          "Latent variable estimated with use of all firm's data") 

plot_results(stan_mod2_MIN) +
  geom_point(data = ac_MIN, aes(x = MidDate, y = MIN_raw)) +
  ggtitle("Voting intention for the Minju Party between the 2016 Boondang-eul elections",
          "Latent variable estimated with use of all firm's data") 

## Prob. of Victory

SAE_results = summary(stan_mod2_SAE)
MIN_results = summary(stan_mod2_MIN)

SAE_chains = dim(SAE_results$c_summary)[3]
MIN_chains = dim(MIN_results$c_summary)[3]

SAE_results_1 = SAE_results$c_summary[,,SAE_chains]
MIN_results_1 = MIN_results$c_summary[,,MIN_chains]

SAE_vics = matrix(nrow=100,ncol=length(SAE_results_1[,1])-2)
MIN_vics = matrix(nrow=100,ncol=length(MIN_results_1[,1])-2)

for(j in 1:100){
SAE_posterior = matrix(nrow=length(SAE_results_1[,1])-2,ncol=10000)
MIN_posterior = matrix(nrow=length(MIN_results_1[,1])-2,ncol=10000)

for(i in 1:nrow(SAE_posterior)){
  SAE_posterior[i,] = rnorm(10000,SAE_results_1[i,1],SAE_results_1[i,2])
  MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])
}

SAE_Final_Results = cbind(rowMeans(SAE_posterior),apply(SAE_posterior,1,min),apply(SAE_posterior,1,max))
MIN_Final_Results = cbind(rowMeans(MIN_posterior),apply(MIN_posterior,1,min),apply(MIN_posterior,1,max))

SAE_vic = rep(0,nrow(SAE_Final_Results))
MIN_vic = rep(0,nrow(MIN_Final_Results))

for(i in 1:nrow(SAE_posterior)){
  if(SAE_Final_Results[i,1] >= MIN_Final_Results[i,1]){
  SAE_vic[i] = mean(SAE_posterior[i,]>=MIN_Final_Results[i,3])
  MIN_vic[i] = mean(MIN_posterior[i,]>=SAE_Final_Results[i,2])
  }
  else{
    SAE_vic[i] = mean(SAE_posterior[i,]>=MIN_Final_Results[i,2])
    MIN_vic[i] = mean(MIN_posterior[i,]>=SAE_Final_Results[i,3])
  }
}

SAE_vics[j,] = SAE_vic
MIN_vics[j,] = MIN_vic
}

DATE = seq(START_DATE,END_DATE,1)
DATE = DATE[-length(DATE)]
SAE_Final_Results = data.frame(DATE,SAE_Final_Results)
MIN_Final_Results = data.frame(DATE,MIN_Final_Results)
colnames(SAE_Final_Results) = c('날짜','지지율','하한','상한') ## 새누리당 지지율 시계열
colnames(MIN_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열

SAE_VIC_prob = cbind(colMeans(SAE_vics),t(apply(SAE_vics,2,quantile,c(0.025,1-0.025)))) 
SAE_MIN_prob = cbind(colMeans(MIN_vics),t(apply(MIN_vics,2,quantile,c(0.025,1-0.025))))

SAE_VIC_prob = data.frame(DATE,SAE_VIC_prob)
MIN_VIC_prob = data.frame(DATE,SAE_MIN_prob)

colnames(SAE_VIC_prob) = c('날짜','승률','2.5%','97.5%') ## 새누리당 승율 지지율
colnames(MIN_VIC_prob) = c('날짜','승률','2.5%','97.5%') ## 민주당 승율 지지율 


# ggplot(SAE_VIC_prob, aes(x=SAE_VIC_prob$'날짜', y=SAE_VIC_prob$'승률'))+geom_point()



