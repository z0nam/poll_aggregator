setwd('C:\\Users\\environ\\Dropbox\\DOC\\여론조사')

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
# ggplot 한글깨짐 대비
library(extrafont)
font_import()
font_name = "NanumGothic"
par(family="NanumGothic")

## Data import

DATA <- read.csv("data/2020_여론조사_비례_utf8.csv")

startIndexName = "합계"
endIndexName = "기타"

party_col_index_start <- 

colnames(DATA) = c('ID','Title','startdate','durringdate','enddate','area1','area2','area3','press','house','method','vir','ARS','N',
                   'rate','sum','MIN','MIN_CHAM','MI','MINSAE','JUS','PEOPLE','REP','CHIN','FREE','OPEN_MIN','PUBLIC','BAR','ALT',
                   'MINP','SAEBO','PARK','MINTONG','AHN','OTHERS','NONE','nonres','link')

house = as.character(data.frame(table(DATA$house))[which(data.frame(table(DATA$house))[,2]>=2),1])
HOUSES = data.frame(house)

DATA = left_join(HOUSES,DATA,by='house')

DATA$startdate <- as.Date(as.character(DATA$startdate))
DATA$durringdate <- as.Date(as.character(DATA$durringdate))
DATA$enddate <- as.Date(as.character(DATA$enddate))
DATA = arrange(DATA,DATA$startdate)

head(DATA)


START_DATE <- as.Date(as.character("2019-12-20"))
END_DATE <- as.Date(as.character("2020-03-16"))


## block

MU_START_MI = DATA %>% filter(!is.na(MI))%>%group_by(enddate) %>% summarise(a = mean(MI))
MU_START_MI <- MU_START_MI$a[1] # 새누리당 초기값

MU_START_MIN = DATA %>% group_by(enddate) %>% summarise(a = mean(MIN))
MU_START_MIN <- MU_START_MIN$a[1] # 민주당 초기값

MU_START_JUS = DATA %>% filter(!is.na(JUS))%>%group_by(enddate) %>% summarise(a = mean(JUS))
MU_START_JUS <- MU_START_JUS$a[1] # 새누리당 초기값

MU_START_PEOPLE = DATA %>% filter(!is.na(PEOPLE))%>%group_by(enddate) %>% summarise(a = mean(PEOPLE))
MU_START_PEOPLE <- MU_START_PEOPLE$a[1] # 새누리당 초기값

MU_START_MINSAE = DATA %>% filter(!is.na(MINSAE))%>%group_by(enddate) %>% summarise(a = mean(MINSAE))
MU_START_MINSAE <- MU_START_MINSAE$a[1] # 새누리당 초기값


MU_FINISH_MI = DATA %>%filter(!is.na(MI))%>%group_by(enddate) %>% summarise(a = mean(MI))
MU_FINISH_MI <- MU_FINISH_MI$a[length(MU_FINISH_MI$a)] # 민주당 마지막값

MU_FINISH_MIN = DATA %>% group_by(enddate) %>% summarise(a = mean(MIN))
MU_FINISH_MIN <- MU_FINISH_MIN$a[length(MU_FINISH_MIN$a)] # 민주당 마지막 값

MU_FINISH_JUS = DATA %>% group_by(enddate) %>% summarise(a = mean(JUS))
MU_FINISH_JUS <- MU_FINISH_JUS$a[length(MU_FINISH_JUS$a)] # 민주당 마지막 값

MU_FINISH_PEOPLE = DATA %>% group_by(enddate) %>% summarise(a = mean(PEOPLE))
MU_FINISH_PEOPLE <- MU_FINISH_PEOPLE$a[length(MU_FINISH_PEOPLE$a)] # 민주당 마지막 값

MU_FINISH_MINSAE = DATA %>% group_by(enddate) %>% summarise(a = mean(MINSAE))
MU_FINISH_MINSAE <- MU_FINISH_MINSAE$a[length(MU_FINISH_MINSAE$a)] # 민주당 마지막 값


Y_MIN <- 30 ##
Y_MAX <- 80 ##  

## Method3

MIN_end_date = DATA %>% filter(!is.na(MIN))
days_between_elections =
as.integer(MIN_end_date$enddate[length(MIN_end_date$enddate)] -   MIN_end_date$startdate[1] )+1
 
MIN_all_polls <- DATA %>% filter(!is.na(MIN)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -sort(startdate[1])),  # ie number of days since starting election
         p = MIN / 100,
         se_MIN = sqrt(p * (1- p) / N) * 100,
         org = fct_reorder(house, MIN))

MIN_poll_orgs <- as.character(data.frame(table(MIN_all_polls$org))[which(data.frame(table(MIN_all_polls$org))[,2]>=2),1])

poll_orgs <- as.character(unique(MIN_all_polls$org))

p1 <- filter(MIN_all_polls, org == MIN_poll_orgs[[1]])
p2 <- filter(MIN_all_polls, org == MIN_poll_orgs[[2]])
p3 <- filter(MIN_all_polls, org == MIN_poll_orgs[[3]])
p4 <- filter(MIN_all_polls, org == MIN_poll_orgs[[4]])
p5 <- filter(MIN_all_polls, org == MIN_poll_orgs[[5]])
p6 <- filter(MIN_all_polls, org == MIN_poll_orgs[[6]])
p7 <- filter(MIN_all_polls, org == MIN_poll_orgs[[7]])

d3_MIN <- list(
  mu_start = MU_START_MIN,
  mu_finish = MU_FINISH_MIN,
  n_days = days_between_elections,
  y1_values = p1$MIN,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_MIN,
  y2_values = p2$MIN,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_MIN,
  y3_values = p3$MIN,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_MIN,
  y4_values = p4$MIN,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_MIN,
  y5_values = p5$MIN,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_MIN,
  
  y6_values = p6$MIN,
  y6_days = p6$MidDateNum,
  y6_n = nrow(p6),
  y6_se = p6$se_MIN,
  
  y7_values = p7$MIN,
  y7_days = p7$MidDateNum,
  y7_n = nrow(p7),
  y7_se = p7$se_MIN
)

stan_mod3_MIN <- stan(file = 'oz-polls-3.stan', data = d3_MIN,
                      control = list(max_treedepth = 15,
                                     adapt_delta = 0.8),
                      iter = 4000)


MIN_results = summary(stan_mod3_MIN)
MIN_chains = dim(MIN_results$c_summary)[3]
MIN_results_1 = MIN_results$c_summary[,,MIN_chains]

MIN_posterior = matrix(nrow=as.integer(MIN_end_date$enddate[length(MIN_end_date$enddate)] -
                                         MIN_end_date$startdate[1] ),ncol=10000)
for(i in 1:nrow(MIN_posterior)){
  MIN_posterior[i,] = rnorm(10000,MIN_results_1[i,1],MIN_results_1[i,2])}
MIN_Final_Results = cbind(rowMeans(MIN_posterior),apply(MIN_posterior,1,quantile,0.05)
                          ,apply(MIN_posterior,1,quantile,0.95))

DATE = (seq(MIN_end_date$enddate[1]-2,MIN_end_date$enddate[length(MIN_end_date$enddate)],1))
MIN_Final_Results = data.frame(DATE,MIN_Final_Results)
colnames(MIN_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열



MI_end_date = DATA %>% filter(!is.na(MI))
days_between_elections =
  as.integer(MI_end_date$enddate[length(MI_end_date$enddate)] -   MI_end_date$startdate[1] )+3

MI_all_polls <- DATA %>% filter(!is.na(MI)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -sort(startdate[1])+1),  # ie number of days since starting election (이부분에 대한 논의가 필요합니다.) <-- 더하기 1 한 것 때문? MI에서는 안했던데요. 
         p = MI / 100,
         se_MI = sqrt(p * (1- p) / N) * 100,
         org = fct_reorder(house, MI))

MI_poll_orgs <- as.character(data.frame(table(MI_all_polls$org))[which(data.frame(table(MI_all_polls$org))[,2]>=2),1])

# count number of research institutes
number_of_orgs <- length(MI_poll_orgs)

# switch by number_of_orgs

p1 <- filter(MI_all_polls, org == MI_poll_orgs[[1]])
p2 <- filter(MI_all_polls, org == MI_poll_orgs[[2]])
p3 <- filter(MI_all_polls, org == MI_poll_orgs[[3]])
p4 <- filter(MI_all_polls, org == MI_poll_orgs[[4]])
p5 <- filter(MI_all_polls, org == MI_poll_orgs[[5]])
p6 <- filter(MI_all_polls, org == MI_poll_orgs[[6]])
p7 <- filter(MI_all_polls, org == MI_poll_orgs[[7]])

d3_MI <- list(
  mu_start = MU_START_MI,
  mu_finish = MU_FINISH_MI,
  n_days = days_between_elections,
  y1_values = p1$MI,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_MI,
  y2_values = p2$MI,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_MI,
  y3_values = p3$MI,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_MI,
  y4_values = p4$MI,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_MI,
  y5_values = p5$MI,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_MI
)

stan_mod3_MI <- stan(file = 'oz-polls-3_MI.stan', data = d3_MI,
                     control = list(max_treedepth = 15,
                                    adapt_delta = 0.8),
                     iter = 4000)


MI_results = summary(stan_mod3_MI)
MI_chains = dim(MI_results$c_summary)[3]
MI_results_1 = MI_results$c_summary[,,MI_chains]

MI_posterior = matrix(nrow=as.integer(MI_end_date$enddate[length(MI_end_date$enddate)] -
                                        MI_end_date$startdate[1] ),ncol=10000)
for(i in 1:nrow(MI_posterior)){
  MI_posterior[i,] = rnorm(10000,MI_results_1[i,1],MI_results_1[i,2])}
MI_Final_Results = cbind(rowMeans(MI_posterior),apply(MI_posterior,1,quantile,0.05)
                         ,apply(MI_posterior,1,quantile,0.95))

DATE = (seq(MI_end_date$enddate[1],MI_end_date$enddate[length(MI_end_date$enddate)],1))
MI_Final_Results = data.frame(DATE,MI_Final_Results)
colnames(MI_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열



JUS_end_date = DATA %>% filter(!is.na(JUS))
days_between_elections =
  as.integer(JUS_end_date$enddate[length(JUS_end_date$enddate)] -   JUS_end_date$startdate[1] )

JUS_all_polls <- DATA %>% filter(!is.na(JUS)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -sort(startdate[1])-1),  # ie number of days since starting election
         p = JUS / 100,
         se_JUS = sqrt(p * (1- p) / N) * 100,
         org = fct_reorder(house, JUS))

poll_orgs <- as.character(unique(JUS_all_polls$org))

p1 <- filter(JUS_all_polls, org == poll_orgs[[1]])
p2 <- filter(JUS_all_polls, org == poll_orgs[[2]])
p3 <- filter(JUS_all_polls, org == poll_orgs[[3]])
p4 <- filter(JUS_all_polls, org == poll_orgs[[4]])
p5 <- filter(JUS_all_polls, org == poll_orgs[[5]])
p6 <- filter(JUS_all_polls, org == poll_orgs[[6]])
p7 <- filter(JUS_all_polls, org == poll_orgs[[7]])

d3_JUS <- list(
  mu_start = MU_START_JUS,
  mu_finish = MU_FINISH_JUS,
  n_days = days_between_elections,
  y1_values = p1$JUS,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_JUS,
  y2_values = p2$JUS,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_JUS,
  y3_values = p3$JUS,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_JUS,
  y4_values = p4$JUS,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_JUS,
  y5_values = p5$JUS,
  y5_days = p5$MidDateNum,
  y5_n = nrow(p5),
  y5_se = p5$se_JUS,
  
  y6_values = p6$JUS,
  y6_days = p6$MidDateNum,
  y6_n = nrow(p6),
  y6_se = p6$se_JUS,
  
  y7_values = p7$JUS,
  y7_days = p7$MidDateNum,
  y7_n = nrow(p7),
  y7_se = p7$se_JUS
)

PEOPLE_end_date = DATA %>% filter(!is.na(PEOPLE))
days_between_elections =
  as.integer(PEOPLE_end_date$enddate[length(PEOPLE_end_date$enddate)] -   PEOPLE_end_date$startdate[1] )

PEOPLE_all_polls <- DATA %>% filter(!is.na(PEOPLE)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -sort(startdate[1])),  # ie number of days since starting election
         p = PEOPLE / 100,
         se_PEOPLE = sqrt(p * (1- p) / N) * 100,
         org = fct_reorder(house, PEOPLE))

poll_orgs <- as.character(unique(PEOPLE_all_polls$org))

p1 <- filter(PEOPLE_all_polls, org == poll_orgs[[1]])
p2 <- filter(PEOPLE_all_polls, org == poll_orgs[[2]])
p3 <- filter(PEOPLE_all_polls, org == poll_orgs[[3]])
p4 <- filter(PEOPLE_all_polls, org == poll_orgs[[4]])
p5 <- filter(PEOPLE_all_polls, org == poll_orgs[[5]])

d3_PEOPLE <- list(
  mu_start = MU_START_PEOPLE,
  mu_finish = MU_FINISH_PEOPLE,
  n_days = days_between_elections,
  y1_values = p1$PEOPLE,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_PEOPLE,
  y2_values = p2$PEOPLE,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_PEOPLE,
  y3_values = p3$PEOPLE,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_PEOPLE,
  y4_values = p4$PEOPLE,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_PEOPLE
  
)

MINSAE_end_date = DATA %>% filter(!is.na(MINSAE))
days_between_elections =
  as.integer(MINSAE_end_date$enddate[length(MINSAE_end_date$enddate)] -   MINSAE_end_date$startdate[1] )

MINSAE_all_polls <- DATA %>% filter(!is.na(MINSAE)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -sort(startdate[1])),  # ie number of days since starting election
         p = MINSAE / 100,
         se_MINSAE = sqrt(p * (1- p) / N) * 100,
         org = fct_reorder(house, MINSAE))

poll_orgs <- as.character(unique(MINSAE_all_polls$org))

p1 <- filter(MINSAE_all_polls, org == poll_orgs[[1]])
p2 <- filter(MINSAE_all_polls, org == poll_orgs[[2]])
p3 <- filter(MINSAE_all_polls, org == poll_orgs[[4]])


d3_MINSAE <- list(
  mu_start = MU_START_MINSAE,
  mu_finish = MU_FINISH_MINSAE,
  n_days = days_between_elections,
  y1_values = p1$MINSAE,
  y1_days = p1$MidDateNum,
  y1_n = nrow(p1),
  y1_se = p1$se_MINSAE,
  y2_values = p2$MINSAE,
  y2_days = p2$MidDateNum,
  y2_n = nrow(p2),
  y2_se = p2$se_MINSAE,
  y3_values = p3$MINSAE,
  y3_days = p3$MidDateNum,
  y3_n = nrow(p3),
  y3_se = p3$se_MINSAE,
  y4_values = p4$MINSAE,
  y4_days = p4$MidDateNum,
  y4_n = nrow(p4),
  y4_se = p4$se_MINSAE
  
)


system.time({
  

  
  stan_mod3_MI <- stan(file = 'oz-polls-3_MI.stan', data = d3_MI,
                    control = list(max_treedepth = 15,
                                   adapt_delta = 0.8),
                    iter = 4000)
  
  stan_mod3_JUS <- stan(file = 'oz-polls-3.stan', data = d3_JUS,
                       control = list(max_treedepth = 15,
                                      adapt_delta = 0.8),
                       iter = 4000)
  
  stan_mod3_PEOPLE <- stan(file = 'oz-polls-3_PEOPLE.stan', data = d3_PEOPLE,
                        control = list(max_treedepth = 15,
                                       adapt_delta = 0.8),
                        iter = 4000)
  
  stan_mod3_MINSAE <- stan(file = 'oz-polls-3_MINSAE.stan', data = d3_MINSAE,
                           control = list(max_treedepth = 15,
                                          adapt_delta = 0.8),
                           iter = 4000)
}) # about 600 seconds



MI_results = summary(stan_mod3_MI)
MI_chains = dim(MI_results$c_summary)[3]
MI_results_1 = MI_results$c_summary[,,MI_chains]

MI_posterior = matrix(nrow=as.integer(MI_end_date$enddate[length(MI_end_date$enddate)] -
                                        MI_end_date$startdate[1] )-1,ncol=10000)
for(i in 1:nrow(MI_posterior)){
  MI_posterior[i,] = rnorm(10000,MI_results_1[i,1],MI_results_1[i,2])}
MI_Final_Results = cbind(rowMeans(MI_posterior),apply(MI_posterior,1,quantile,0.05),
                         apply(MI_posterior,1,quantile,0.95))

DATE = seq( MI_end_date$startdate[1],MI_end_date$enddate[length(MI_end_date$enddate)]-2,1)
MI_Final_Results = data.frame(DATE,MI_Final_Results)
colnames(MI_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열

JUS_results = summary(stan_mod3_JUS)
JUS_chains = dim(JUS_results$c_summary)[3]
JUS_results_1 = JUS_results$c_summary[,,JUS_chains]

JUS_posterior = matrix(nrow=as.integer(JUS_end_date$enddate[length(JUS_end_date$enddate)] -
                                         JUS_end_date$startdate[1] )-1,ncol=10000)
for(i in 1:nrow(JUS_posterior)){
  JUS_posterior[i,] = rnorm(10000,JUS_results_1[i,1],JUS_results_1[i,2])}
JUS_Final_Results = cbind(rowMeans(JUS_posterior),apply(JUS_posterior,1,quantile,0.05),
                          apply(JUS_posterior,1,quantile,0.95))

DATE = seq( JUS_end_date$startdate[1],JUS_end_date$enddate[length(JUS_end_date$enddate)]-2,1)
JUS_Final_Results = data.frame(DATE,JUS_Final_Results)
colnames(JUS_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열

PEOPLE_results = summary(stan_mod3_PEOPLE)
PEOPLE_chains = dim(PEOPLE_results$c_summary)[3]
PEOPLE_results_1 = PEOPLE_results$c_summary[,,PEOPLE_chains]

PEOPLE_posterior = matrix(nrow=as.integer(PEOPLE_end_date$enddate[length(PEOPLE_end_date$enddate)] -
                                            PEOPLE_end_date$startdate[1] )-1,ncol=10000)
for(i in 1:nrow(PEOPLE_posterior)){
  PEOPLE_posterior[i,] = rnorm(10000,PEOPLE_results_1[i,1],PEOPLE_results_1[i,2])}
PEOPLE_Final_Results = cbind(rowMeans(PEOPLE_posterior),apply(PEOPLE_posterior,1,quantile,0.05),
                             apply(PEOPLE_posterior,1,quantile,0.95))

DATE = seq( PEOPLE_end_date$startdate[1],PEOPLE_end_date$enddate[length(PEOPLE_end_date$enddate)]-2,1)
PEOPLE_Final_Results = data.frame(DATE,PEOPLE_Final_Results)
colnames(PEOPLE_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열

MINSAE_results = summary(stan_mod3_MINSAE)
MINSAE_chains = dim(MINSAE_results$c_summary)[3]
MINSAE_results_1 = MINSAE_results$c_summary[,,MINSAE_chains]

MINSAE_posterior = matrix(nrow=as.integer(MINSAE_end_date$enddate[length(MINSAE_end_date$enddate)] -
                                            MINSAE_end_date$startdate[1] )-1,ncol=10000)
for(i in 1:nrow(MINSAE_posterior)){
  MINSAE_posterior[i,] = rnorm(10000,MINSAE_results_1[i,1],MINSAE_results_1[i,2])}
MINSAE_Final_Results = cbind(rowMeans(MINSAE_posterior),apply(MINSAE_posterior,1,quantile,0.05),
                             apply(MINSAE_posterior,1,quantile,0.95))

DATE = seq( MINSAE_end_date$startdate[1],MINSAE_end_date$enddate[length(MINSAE_end_date$enddate)]-2,1)
MINSAE_Final_Results = data.frame(DATE,MINSAE_Final_Results)
colnames(MINSAE_Final_Results) = c('날짜','지지율','하한','상한') ## 민주당 지지율 시계열


write.csv(MI_results_1,'미래.csv')
write.csv(MIN_results_1,'민주.csv')
write.csv(JUS_results_1,'정의.csv')
write.csv(PEOPLE_results_1,'국민의.csv')
write.csv(JUS_results_1,'정의.csv')
write.csv(MINSAE_results_1,'민생.csv')          
