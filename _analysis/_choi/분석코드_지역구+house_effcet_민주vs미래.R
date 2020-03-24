for(p in 1:23){
library(dplyr);library(overlapping);library(truncnorm)
setwd('C:\\Users\\environ\\Dropbox\\DOC\\21대여론조사\\house_effect')

house_effect = data.frame()
for(i in list.files()){
  house_effect = rbind(house_effect,read.csv(i))
}
house_effect = house_effect[,-1]
house_effect = reshape(house_effect,direction='wide',idvar='house',timevar = 'party')


house_effect = house_effect[,-which(colnames(house_effect)=='house_effect.자유한국당')]
colnames(house_effect) = c("house","party_12","party_13","party_1","party_2","party_3","party_14","party_5","party_11","party_16","party_6","party_4")

setwd('C:\\Users\\environ\\Dropbox\\DOC\\21대여론조사')

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
#font_import()
#font_name = "NanumGothic"
#par(family="NanumGothic")


## Data import
DATE1 = data.frame(seq(as.Date("2019-12-31"),as.Date("2020-04-16"),1));colnames(DATE1)='DATE'
FINAL = data.frame(DATE1);colnames(FINAL) = 'DATE'


DATA <- read.csv("data\\지역구.csv")

region = data.frame(table(DATA$area3))[data.frame(table(DATA$area3))[,2]>=2,1]

a = region[p]

DATA[is.na(DATA)]=0
DATA[,which(colnames(DATA)=='party_1'):which(colnames(DATA)=='party_17')]=100*
  DATA[,which(colnames(DATA)=='party_1'):which(colnames(DATA)=='party_17')]/rowSums(DATA[,which(colnames(DATA)=='party_1'):which(colnames(DATA)=='party_17')])


DATA = left_join(DATA,house_effect,by='house')


DATA$startdate <- as.Date(as.character(DATA$startdate))
DATA$durringdate <- as.Date(as.character(DATA$durringdate))
DATA$enddate <- as.Date(as.character(DATA$enddate))
DATA = arrange(DATA,DATA$startdate)

DATA[is.na(DATA)]=0


DATA$party_1.x	=	DATA$party_1.x-DATA$party_1.y
DATA$party_2.x	=	DATA$party_2.x-DATA$party_2.y
DATA$party_3.x	=	DATA$party_3.x-DATA$party_3.y
DATA$party_4.x	=	DATA$party_4.x-DATA$party_4.y
DATA$party_5.x	=	DATA$party_5.x-DATA$party_5.y
DATA$party_6.x	=	DATA$party_6.x-DATA$party_6.y
DATA$party_11.x	=	DATA$party_11.x-DATA$party_11.y
DATA$party_12.x	=	DATA$party_12.x-DATA$party_12.y
DATA$party_13.x	=	DATA$party_13.x-DATA$party_13.y
DATA$party_14.x	=	DATA$party_14.x-DATA$party_14.y
DATA$party_16.x	=	DATA$party_16.x-DATA$party_16.y

colnames(DATA)[which(colnames(DATA)=="party_1.x")]="party_1"
colnames(DATA)[which(colnames(DATA)=="party_2.x")]="party_2"
colnames(DATA)[which(colnames(DATA)=="party_3.x")]="party_3"
colnames(DATA)[which(colnames(DATA)=="party_4.x")]="party_4"
colnames(DATA)[which(colnames(DATA)=="party_5.x")]="party_5"
colnames(DATA)[which(colnames(DATA)=="party_6.x")]="party_6"
colnames(DATA)[which(colnames(DATA)=="party_11.x")]="party_11"
colnames(DATA)[which(colnames(DATA)=="party_12.x")]="party_12"
colnames(DATA)[which(colnames(DATA)=="party_13.x")]="party_13"
colnames(DATA)[which(colnames(DATA)=="party_14.x")]="party_14"
colnames(DATA)[which(colnames(DATA)=="party_16.x")]="party_16"


DATA = DATA %>% filter(area3 == a) %>% data.frame
head(DATA)

## Initialization

MU_START_party_1	=	DATA%>%filter(!is.na(party_1))%>%group_by(enddate) %>% summarise (mean = mean(party_1)) %>% data.frame
MU_START_party_2	=	DATA%>%filter(!is.na(party_2))%>%group_by(enddate) %>% summarise (mean = mean(party_2)) %>% data.frame
MU_START_party_3	=	DATA%>%filter(!is.na(party_3))%>%group_by(enddate) %>% summarise (mean = mean(party_3)) %>% data.frame
MU_START_party_4	=	DATA%>%filter(!is.na(party_4))%>%group_by(enddate) %>% summarise (mean = mean(party_4)) %>% data.frame
MU_START_party_5	=	DATA%>%filter(!is.na(party_5))%>%group_by(enddate) %>% summarise (mean = mean(party_5)) %>% data.frame
MU_START_party_6	=	DATA%>%filter(!is.na(party_6))%>%group_by(enddate) %>% summarise (mean = mean(party_6)) %>% data.frame
MU_START_party_7	=	DATA%>%filter(!is.na(party_7))%>%group_by(enddate) %>% summarise (mean = mean(party_7)) %>% data.frame
MU_START_party_8	=	DATA%>%filter(!is.na(party_8))%>%group_by(enddate) %>% summarise (mean = mean(party_8)) %>% data.frame
MU_START_party_9	=	DATA%>%filter(!is.na(party_9))%>%group_by(enddate) %>% summarise (mean = mean(party_9)) %>% data.frame
MU_START_party_10	=	DATA%>%filter(!is.na(party_10))%>%group_by(enddate) %>% summarise (mean = mean(party_10)) %>% data.frame
MU_START_party_11	=	DATA%>%filter(!is.na(party_11))%>%group_by(enddate) %>% summarise (mean = mean(party_11)) %>% data.frame
MU_START_party_12	=	DATA%>%filter(!is.na(party_12))%>%group_by(enddate) %>% summarise (mean = mean(party_12)) %>% data.frame
MU_START_party_13	=	DATA%>%filter(!is.na(party_13))%>%group_by(enddate) %>% summarise (mean = mean(party_13)) %>% data.frame
MU_START_party_14	=	DATA%>%filter(!is.na(party_14))%>%group_by(enddate) %>% summarise (mean = mean(party_14)) %>% data.frame
MU_START_party_15	=	DATA%>%filter(!is.na(party_15))%>%group_by(enddate) %>% summarise (mean = mean(party_15)) %>% data.frame
MU_START_party_16	=	DATA%>%filter(!is.na(party_16))%>%group_by(enddate) %>% summarise (mean = mean(party_16)) %>% data.frame


MU_START_party_1	=	MU_START_party_1$mean[1]
MU_START_party_2	=	MU_START_party_2$mean[1]
MU_START_party_3	=	MU_START_party_3$mean[1]
MU_START_party_4	=	MU_START_party_4$mean[1]
MU_START_party_5	=	MU_START_party_5$mean[1]
MU_START_party_6	=	MU_START_party_6$mean[1]
MU_START_party_7	=	MU_START_party_7$mean[1]
MU_START_party_8  =	MU_START_party_8$mean[1]
MU_START_party_9	=	MU_START_party_9$mean[1]
MU_START_party_10	=	MU_START_party_10$mean[1]
MU_START_party_11	=	MU_START_party_11$mean[1]
MU_START_party_12	=	MU_START_party_12$mean[1]
MU_START_party_13	=	MU_START_party_13$mean[1]
MU_START_party_14	=	MU_START_party_14$mean[1]
MU_START_party_15	=	MU_START_party_15$mean[1]
MU_START_party_16	=	MU_START_party_16$mean[1]


MU_FINISH_party_1	=	DATA%>%filter(!is.na(party_1))%>%group_by(enddate) %>% summarise (mean = mean(party_1)) %>% data.frame
MU_FINISH_party_2	=	DATA%>%filter(!is.na(party_2))%>%group_by(enddate) %>% summarise (mean = mean(party_2)) %>% data.frame
MU_FINISH_party_3	=	DATA%>%filter(!is.na(party_3))%>%group_by(enddate) %>% summarise (mean = mean(party_3)) %>% data.frame
MU_FINISH_party_4	=	DATA%>%filter(!is.na(party_4))%>%group_by(enddate) %>% summarise (mean = mean(party_4)) %>% data.frame
MU_FINISH_party_5	=	DATA%>%filter(!is.na(party_5))%>%group_by(enddate) %>% summarise (mean = mean(party_5)) %>% data.frame
MU_FINISH_party_6	=	DATA%>%filter(!is.na(party_6))%>%group_by(enddate) %>% summarise (mean = mean(party_6)) %>% data.frame
MU_FINISH_party_7	=	DATA%>%filter(!is.na(party_7))%>%group_by(enddate) %>% summarise (mean = mean(party_7)) %>% data.frame
MU_FINISH_party_8	=	DATA%>%filter(!is.na(party_8))%>%group_by(enddate) %>% summarise (mean = mean(party_8)) %>% data.frame
MU_FINISH_party_9=	DATA%>%filter(!is.na(party_9))%>%group_by(enddate) %>% summarise (mean = mean(party_9)) %>% data.frame
MU_FINISH_party_10	=	DATA%>%filter(!is.na(party_10))%>%group_by(enddate) %>% summarise (mean = mean(party_10)) %>% data.frame
MU_FINISH_party_11	=	DATA%>%filter(!is.na(party_11))%>%group_by(enddate) %>% summarise (mean = mean(party_11)) %>% data.frame
MU_FINISH_party_12	=	DATA%>%filter(!is.na(party_12))%>%group_by(enddate) %>% summarise (mean = mean(party_12)) %>% data.frame
MU_FINISH_party_13	=	DATA%>%filter(!is.na(party_13))%>%group_by(enddate) %>% summarise (mean = mean(party_13)) %>% data.frame
MU_FINISH_party_14	=	DATA%>%filter(!is.na(party_14))%>%group_by(enddate) %>% summarise (mean = mean(party_14)) %>% data.frame
MU_FINISH_party_15	=	DATA%>%filter(!is.na(party_15))%>%group_by(enddate) %>% summarise (mean = mean(party_15)) %>% data.frame
MU_FINISH_party_16	=	DATA%>%filter(!is.na(party_16))%>%group_by(enddate) %>% summarise (mean = mean(party_16)) %>% data.frame

MU_FINISH_party_1	=	MU_FINISH_party_1$mean[nrow(MU_FINISH_party_1)]
MU_FINISH_party_2	=	MU_FINISH_party_2$mean[nrow(MU_FINISH_party_2)]
MU_FINISH_party_3	=	MU_FINISH_party_3$mean[nrow(MU_FINISH_party_3)]
MU_FINISH_party_4	=	MU_FINISH_party_4$mean[nrow(MU_FINISH_party_4)]
MU_FINISH_party_5	=	MU_FINISH_party_5$mean[nrow(MU_FINISH_party_5)]
MU_FINISH_party_6	=	MU_FINISH_party_6$mean[nrow(MU_FINISH_party_6)]
MU_FINISH_party_7	=	MU_FINISH_party_7$mean[nrow(MU_FINISH_party_7)]
MU_FINISH_party_8	=	MU_FINISH_party_8$mean[nrow(MU_FINISH_party_8)]
MU_FINISH_party_9	=	MU_FINISH_party_9$mean[nrow(MU_FINISH_party_9)]
MU_FINISH_party_10	=	MU_FINISH_party_10$mean[nrow(MU_FINISH_party_10)]
MU_FINISH_party_11	=	MU_FINISH_party_11$mean[nrow(MU_FINISH_party_11)]
MU_FINISH_party_12	=	MU_FINISH_party_12$mean[nrow(MU_FINISH_party_12)]
MU_FINISH_party_13	=	MU_FINISH_party_13$mean[nrow(MU_FINISH_party_13)]
MU_FINISH_party_14	=	MU_FINISH_party_14$mean[nrow(MU_FINISH_party_14)]
MU_FINISH_party_15	=	MU_FINISH_party_15$mean[nrow(MU_FINISH_party_15)]
MU_FINISH_party_16	=	MU_FINISH_party_16$mean[nrow(MU_FINISH_party_16)]

## Method L2: No House Effect in Local Election

party_1_enddate = DATA %>% filter(party_1>0)
party_1_days_between_elections =
  as.integer(party_1_enddate$enddate[length(party_1_enddate$enddate)]-   (party_1_enddate$enddate[1]-3 ))

ac_party_1 <- DATA %>% filter(!is.na(party_1)) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_1 / 100,
         se_party_1 = sqrt(p * (1- p) / N) * 100)


d2_party_1 <- list(
  mu_start = MU_START_party_1,
  mu_finish = MU_FINISH_party_1,
  n_days = party_1_days_between_elections,
  y_values = ac_party_1$party_1,
  y_days = ac_party_1$MidDateNum,
  y_n = nrow(ac_party_1),
  y_se = ac_party_1$se_party_1
) 

tryCatch({stan_mod2_party_1 <- stan(file = 'oz-polls-2.stan', data = d2_party_1,
                          control = list(max_treedepth = 20))})
tryCatch({stan_mod2_party_1_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_1,
                              control = list(max_treedepth = 20))})



party_2_enddate = DATA %>% filter(party_2>0)
party_2_days_between_elections =
  as.integer(party_2_enddate$enddate[length(party_2_enddate$enddate)]-   (party_2_enddate$enddate[1]-3 ))

ac_party_2 <- DATA %>% filter(party_2>0) %>% 
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_2 / 100,
         se_party_2 = sqrt(p * (1- p) / N) * 100)


d2_party_2 <- list(
  mu_start = MU_START_party_2,
  mu_finish = MU_FINISH_party_2,
  n_days = party_2_days_between_elections,
  y_values = ac_party_2$party_2,
  y_days = ac_party_2$MidDateNum,
  y_n = nrow(ac_party_2),
  y_se = ac_party_2$se_party_2
) 

tryCatch({stan_mod2_party_2 <- stan(file = 'oz-polls-2.stan', data = d2_party_2,
                          control = list(max_treedepth = 20))})
tryCatch({stan_mod2_party_2_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_2,
                              control = list(max_treedepth = 20))})

party_3_enddate = DATA %>% filter(party_3>0)
party_3_days_between_elections =
  as.integer(party_3_enddate$enddate[length(party_3_enddate$enddate)]-   (party_3_enddate$enddate[1]-3 ))

ac_party_3 <- DATA %>% filter(party_3>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_3 / 100,
         se_party_3 = sqrt(p * (1- p) / N) * 100)


d2_party_3 <- list(
  mu_start = MU_START_party_3,
  mu_finish = MU_FINISH_party_3,
  n_days = party_3_days_between_elections,
  y_values = ac_party_3$party_3,
  y_days = ac_party_3$MidDateNum,
  y_n = nrow(ac_party_3),
  y_se = ac_party_3$se_party_3
) 

tryCatch({stan_mod2_party_3 <- stan(file = 'oz-polls-2.stan', data = d2_party_3,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_3_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_3,
                              control = list(max_treedepth = 20))})

party_4_enddate = DATA %>% filter(party_4>0)
party_4_days_between_elections =
  as.integer(party_4_enddate$enddate[length(party_4_enddate$enddate)]-   (party_4_enddate$enddate[1]-3 ))

ac_party_4 <- DATA %>% filter(party_4>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_4 / 100,
         se_party_4 = sqrt(p * (1- p) / N) * 100)


d2_party_4 <- list(
  mu_start = MU_START_party_4,
  mu_finish = MU_FINISH_party_4,
  n_days = party_4_days_between_elections,
  y_values = ac_party_4$party_4,
  y_days = ac_party_4$MidDateNum,
  y_n = nrow(ac_party_4),
  y_se = ac_party_4$se_party_4
) 

tryCatch({stan_mod2_party_4 <- stan(file = 'oz-polls-2.stan', data = d2_party_4,
                          control = list(max_treedepth = 20))})
tryCatch({stan_mod2_party_4_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_4,
                              control = list(max_treedepth = 20))})


party_5_enddate = DATA %>% filter(party_5>0)
party_5_days_between_elections =
  as.integer(party_5_enddate$enddate[length(party_5_enddate$enddate)]-   (party_5_enddate$enddate[1]-3 ))

ac_party_5 <- DATA %>% filter(party_5>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_5 / 100,
         se_party_5 = sqrt(p * (1- p) / N) * 100)


d2_party_5 <- list(
  mu_start = MU_START_party_5,
  mu_finish = MU_FINISH_party_5,
  n_days = party_5_days_between_elections,
  y_values = ac_party_5$party_5,
  y_days = ac_party_5$MidDateNum,
  y_n = nrow(ac_party_5),
  y_se = ac_party_5$se_party_5
) 

tryCatch({stan_mod2_party_5 <- stan(file = 'oz-polls-2.stan', data = d2_party_5,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_5_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_5,
                              control = list(max_treedepth = 20))})


party_6_enddate = DATA %>% filter(party_6>0)
party_6_days_between_elections =
  as.integer(party_6_enddate$enddate[length(party_6_enddate$enddate)]-   (party_6_enddate$enddate[1]-3 ))

ac_party_6 <- DATA %>% filter(party_6>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_6 / 100,
         se_party_6 = sqrt(p * (1- p) / N) * 100)


d2_party_6 <- list(
  mu_start = MU_START_party_6,
  mu_finish = MU_FINISH_party_6,
  n_days = party_6_days_between_elections,
  y_values = ac_party_6$party_6,
  y_days = ac_party_6$MidDateNum,
  y_n = nrow(ac_party_6),
  y_se = ac_party_6$se_party_6
) 

tryCatch({stan_mod2_party_6 <- stan(file = 'oz-polls-2.stan', data = d2_party_6,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_6_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_6,
                              control = list(max_treedepth = 20))})

party_7_enddate = DATA %>% filter(party_7>0)
party_7_days_between_elections =
  as.integer(party_7_enddate$enddate[length(party_7_enddate$enddate)]-   (party_7_enddate$enddate[1]-3 ))

ac_party_7 <- DATA %>% filter(party_7>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_7 / 100,
         se_party_7 = sqrt(p * (1- p) / N) * 100)


d2_party_7 <- list(
  mu_start = MU_START_party_7,
  mu_finish = MU_FINISH_party_7,
  n_days = party_7_days_between_elections,
  y_values = ac_party_7$party_7,
  y_days = ac_party_7$MidDateNum,
  y_n = nrow(ac_party_7),
  y_se = ac_party_7$se_party_7
) 

tryCatch({stan_mod2_party_7 <- stan(file = 'oz-polls-2.stan', data = d2_party_7,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_7_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_7,
                              control = list(max_treedepth = 20))})

party_8_enddate = DATA %>% filter(party_8>0)
party_8_days_between_elections =
  as.integer(party_8_enddate$enddate[length(party_8_enddate$enddate)]-   (party_8_enddate$enddate[1]-3 ))

ac_party_8 <- DATA %>% filter(party_8>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_8 / 100,
         se_party_8 = sqrt(p * (1- p) / N) * 100)


d2_party_8 <- list(
  mu_start = MU_START_party_8,
  mu_finish = MU_FINISH_party_8,
  n_days = party_8_days_between_elections,
  y_values = ac_party_8$party_8,
  y_days = ac_party_8$MidDateNum,
  y_n = nrow(ac_party_8),
  y_se = ac_party_8$se_party_8
) 

tryCatch({stan_mod2_party_8 <- stan(file = 'oz-polls-2.stan', data = d2_party_8,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_8_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_8,
                              control = list(max_treedepth = 20))})

party_9_enddate = DATA %>% filter(party_9>0)
party_9_days_between_elections =
  as.integer(party_9_enddate$enddate[length(party_9_enddate$enddate)]-   (party_9_enddate$enddate[1]-3 ))

ac_party_9 <- DATA %>% filter(party_9>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_9 / 100,
         se_party_9 = sqrt(p * (1- p) / N) * 100)


d2_party_9 <- list(
  mu_start = MU_START_party_9,
  mu_finish = MU_FINISH_party_9,
  n_days = party_9_days_between_elections,
  y_values = ac_party_9$party_9,
  y_days = ac_party_9$MidDateNum,
  y_n = nrow(ac_party_9),
  y_se = ac_party_9$se_party_9
) 

tryCatch({stan_mod2_party_9 <- stan(file = 'oz-polls-2.stan', data = d2_party_9,
                          control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_9_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_9,
                              control = list(max_treedepth = 20))})


party_10_enddate = DATA %>% filter(party_10>0)
party_10_days_between_elections =
  as.integer(party_10_enddate$enddate[length(party_10_enddate$enddate)]-   (party_10_enddate$enddate[1]-3 ))

ac_party_10 <- DATA %>% filter(party_10>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_10 / 100,
         se_party_10 = sqrt(p * (1- p) / N) * 100)


d2_party_10 <- list(
  mu_start = MU_START_party_10,
  mu_finish = MU_FINISH_party_10,
  n_days = party_10_days_between_elections,
  y_values = ac_party_10$party_10,
  y_days = ac_party_10$MidDateNum,
  y_n = nrow(ac_party_10),
  y_se = ac_party_10$se_party_10
) 

tryCatch({stan_mod2_party_10 <- stan(file = 'oz-polls-2.stan', data = d2_party_10,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_10_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_10,
                               control = list(max_treedepth = 20))})


party_11_enddate = DATA %>% filter(party_11>0)
party_11_days_between_elections =
  as.integer(party_11_enddate$enddate[length(party_11_enddate$enddate)]-   (party_11_enddate$enddate[1]-3 ))

ac_party_11 <- DATA %>% filter(party_11>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_11 / 100,
         se_party_11 = sqrt(p * (1- p) / N) * 100)


d2_party_11 <- list(
  mu_start = MU_START_party_11,
  mu_finish = MU_FINISH_party_11,
  n_days = party_11_days_between_elections,
  y_values = ac_party_11$party_11,
  y_days = ac_party_11$MidDateNum,
  y_n = nrow(ac_party_11),
  y_se = ac_party_11$se_party_11
) 

tryCatch({stan_mod2_party_11 <- stan(file = 'oz-polls-2.stan', data = d2_party_11,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_11_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_11,
                               control = list(max_treedepth = 20))})


party_12_enddate = DATA %>% filter(party_12>0)
party_12_days_between_elections =
  as.integer(party_12_enddate$enddate[length(party_12_enddate$enddate)]-   (party_12_enddate$enddate[1]-3 ))

ac_party_12 <- DATA %>% filter(party_12>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_12 / 100,
         se_party_12 = sqrt(p * (1- p) / N) * 100)


d2_party_12 <- list(
  mu_start = MU_START_party_12,
  mu_finish = MU_FINISH_party_12,
  n_days = party_12_days_between_elections,
  y_values = ac_party_12$party_12,
  y_days = ac_party_12$MidDateNum,
  y_n = nrow(ac_party_12),
  y_se = ac_party_12$se_party_12
) 

tryCatch({stan_mod2_party_12 <- stan(file = 'oz-polls-2.stan', data = d2_party_12,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_12_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_12,
                               control = list(max_treedepth = 20))})

party_13_enddate = DATA %>% filter(party_13>0)
party_13_days_between_elections =
  as.integer(party_13_enddate$enddate[length(party_13_enddate$enddate)]-   (party_13_enddate$enddate[1]-3 ))

ac_party_13 <- DATA %>% filter(party_13>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_13 / 100,
         se_party_13 = sqrt(p * (1- p) / N) * 100)


d2_party_13 <- list(
  mu_start = MU_START_party_13,
  mu_finish = MU_FINISH_party_13,
  n_days = party_13_days_between_elections,
  y_values = ac_party_13$party_13,
  y_days = ac_party_13$MidDateNum,
  y_n = nrow(ac_party_13),
  y_se = ac_party_13$se_party_13
) 

tryCatch({stan_mod2_party_13 <- stan(file = 'oz-polls-2.stan', data = d2_party_13,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_13_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_13,
                               control = list(max_treedepth = 20))})

party_14_enddate = DATA %>% filter(party_14>0)
party_14_days_between_elections =
  as.integer(party_14_enddate$enddate[length(party_14_enddate$enddate)]-   (party_14_enddate$enddate[1]-3 ))

ac_party_14 <- DATA %>% filter(party_14>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_14 / 100,
         se_party_14 = sqrt(p * (1- p) / N) * 100)


d2_party_14 <- list(
  mu_start = MU_START_party_14,
  mu_finish = MU_FINISH_party_14,
  n_days = party_14_days_between_elections,
  y_values = ac_party_14$party_14,
  y_days = ac_party_14$MidDateNum,
  y_n = nrow(ac_party_14),
  y_se = ac_party_14$se_party_14
) 

tryCatch({stan_mod2_party_14 <- stan(file = 'oz-polls-2.stan', data = d2_party_14,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_14_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_14,
                               control = list(max_treedepth = 20))})


party_15_enddate = DATA %>% filter(party_15>0)
party_15_days_between_elections =
  as.integer(party_15_enddate$enddate[length(party_15_enddate$enddate)]-   (party_15_enddate$enddate[1]-3 ))

ac_party_15 <- DATA %>% filter(party_15>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_15 / 100,
         se_party_15 = sqrt(p * (1- p) / N) * 100)


d2_party_15 <- list(
  mu_start = MU_START_party_15,
  mu_finish = MU_FINISH_party_15,
  n_days = party_15_days_between_elections,
  y_values = ac_party_15$party_15,
  y_days = ac_party_15$MidDateNum,
  y_n = nrow(ac_party_15),
  y_se = ac_party_15$se_party_15
) 

tryCatch({stan_mod2_party_15 <- stan(file = 'oz-polls-2.stan', data = d2_party_15,
                           control = list(max_treedepth = 20))})
tryCatch({stan_mod2_party_15_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_15,
                               control = list(max_treedepth = 20))})


party_16_enddate = DATA %>% filter(party_16>0)
party_16_days_between_elections =
  as.integer(party_16_enddate$enddate[length(party_16_enddate$enddate)]-   (party_16_enddate$enddate[1]-3 ))

ac_party_16 <- DATA %>% filter(party_16>0) %>%
  mutate(MidDate = startdate + (enddate - startdate) / 2,
         MidDateNum = as.integer(MidDate -  (enddate[1]-3)),  # ie number of days since starting election
         p = party_16 / 100,
         se_party_16 = sqrt(p * (1- p) / N) * 100)


d2_party_16 <- list(
  mu_start = MU_START_party_16,
  mu_finish = MU_FINISH_party_16,
  n_days = party_16_days_between_elections,
  y_values = ac_party_16$party_16,
  y_days = ac_party_16$MidDateNum,
  y_n = nrow(ac_party_16),
  y_se = ac_party_16$se_party_16
) 

tryCatch({stan_mod2_party_16 <- stan(file = 'oz-polls-2.stan', data = d2_party_16,
                           control = list(max_treedepth = 20))})

tryCatch({stan_mod2_party_16_VIC <- stan(file = 'oz-polls-2_vic.stan', data = d2_party_16,
                               control = list(max_treedepth = 20))})

if(length(dim(stan_mod2_party_1)>0)){
party_1_results = summary(stan_mod2_party_1)
party_1_chains = dim(party_1_results$c_summary)[3]
party_1_results_1 = party_1_results$c_summary[,,party_1_chains]

party_1_posterior = matrix(nrow=party_1_days_between_elections,ncol=10000)

for(i in 1:nrow(party_1_posterior)){
  party_1_posterior[i,] = rnorm(10000,party_1_results_1[i,1],party_1_results_1[i,2])}
party_1_Final_Results = cbind(rowMeans(party_1_posterior),apply(party_1_posterior,1,quantile,0.05)
                              ,apply(party_1_posterior,1,quantile,0.95))

DATE = (seq(party_1_enddate$enddate[1]-2,party_1_enddate$enddate[length(party_1_enddate$enddate)],1))
party_1_Final_Results = data.frame(DATE,party_1_Final_Results)
colnames(party_1_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_1_Final_Results = party_1_Final_Results[which(party_1_Final_Results$DATE==party_1_enddate$enddate[1])[1]:
                                                nrow(party_1_Final_Results),]


party_1_Final_Results	=	data.frame(party_1_Final_Results,"더불어민주당","party_1")
colnames(party_1_Final_Results)[5:6]=c("NAME",'NAME1') 
FINAL = left_join(FINAL,party_1_Final_Results)
}

if(length(dim(stan_mod2_party_2)>0)){
party_2_results = summary(stan_mod2_party_2)
party_2_chains = dim(party_2_results$c_summary)[3]
party_2_results_1 = party_2_results$c_summary[,,party_2_chains]

party_2_posterior = matrix(nrow=party_2_days_between_elections,ncol=10000)

for(i in 1:nrow(party_2_posterior)){
  party_2_posterior[i,] = rnorm(10000,party_2_results_1[i,1],party_2_results_1[i,2])}
party_2_Final_Results = cbind(rowMeans(party_2_posterior),apply(party_2_posterior,1,quantile,0.05)
                              ,apply(party_2_posterior,1,quantile,0.95))

DATE = (seq(party_2_enddate$enddate[1]-2,party_2_enddate$enddate[length(party_2_enddate$enddate)],1))
party_2_Final_Results = data.frame(DATE,party_2_Final_Results)
colnames(party_2_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_2_Final_Results = party_2_Final_Results[which(party_2_Final_Results$DATE==party_2_enddate$enddate[1])[1]:
                                                nrow(party_2_Final_Results),]
party_2_Final_Results	=	data.frame(party_2_Final_Results,"미래통합당",'party_2')
colnames(party_2_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_2_Final_Results)
FINAL = rbind(FINAL,a)}

if(length(dim(stan_mod2_party_3)>0)){
party_3_results = summary(stan_mod2_party_3)
party_3_chains = dim(party_3_results$c_summary)[3]
party_3_results_1 = party_3_results$c_summary[,,party_3_chains]

party_3_posterior = matrix(nrow=party_3_days_between_elections,ncol=10000)

for(i in 1:nrow(party_3_posterior)){
  party_3_posterior[i,] = rnorm(10000,party_3_results_1[i,1],party_3_results_1[i,2])}
party_3_Final_Results = cbind(rowMeans(party_3_posterior),apply(party_3_posterior,1,quantile,0.05)
                              ,apply(party_3_posterior,1,quantile,0.95))

DATE = (seq(party_3_enddate$enddate[1]-2,party_3_enddate$enddate[length(party_3_enddate$enddate)],1))
party_3_Final_Results = data.frame(DATE,party_3_Final_Results)
colnames(party_3_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_3_Final_Results = party_3_Final_Results[which(party_3_Final_Results$DATE==party_3_enddate$enddate[1])[1]:
                                                nrow(party_3_Final_Results),]

party_3_Final_Results	=	data.frame(party_3_Final_Results,"민생당",'party_3')
colnames(party_3_Final_Results)[5:6]=c("NAME","NAME1")
a = left_join(DATE1,party_3_Final_Results)
FINAL = rbind(FINAL,a)}

if(length(dim(stan_mod2_party_4)>0)){
party_4_results = summary(stan_mod2_party_4)
party_4_chains = dim(party_4_results$c_summary)[3]
party_4_results_1 = party_4_results$c_summary[,,party_4_chains]

party_4_posterior = matrix(nrow=party_4_days_between_elections,ncol=10000)

for(i in 1:nrow(party_4_posterior)){
  party_4_posterior[i,] = rnorm(10000,party_4_results_1[i,1],party_4_results_1[i,2])}
party_4_Final_Results = cbind(rowMeans(party_4_posterior),apply(party_4_posterior,1,quantile,0.05)
                              ,apply(party_4_posterior,1,quantile,0.95))

DATE = (seq(party_4_enddate$enddate[1]-2,party_4_enddate$enddate[length(party_4_enddate$enddate)],1))
party_4_Final_Results = data.frame(DATE,party_4_Final_Results)
colnames(party_4_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_4_Final_Results = party_4_Final_Results[which(party_4_Final_Results$DATE==party_4_enddate$enddate[1])[1]:
                                                nrow(party_4_Final_Results),]

party_4_Final_Results	=	data.frame(party_4_Final_Results,"정의당",'party_4')
colnames(party_4_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_4_Final_Results)
FINAL = rbind(FINAL,a)}

if(length(dim(stan_mod2_party_5)>0)){
party_5_results = summary(stan_mod2_party_5)
party_5_chains = dim(party_5_results$c_summary)[3]
party_5_results_1 = party_5_results$c_summary[,,party_5_chains]

party_5_posterior = matrix(nrow=party_5_days_between_elections,ncol=10000)

for(i in 1:nrow(party_5_posterior)){
  party_5_posterior[i,] = rnorm(10000,party_5_results_1[i,1],party_5_results_1[i,2])}
party_5_Final_Results = cbind(rowMeans(party_5_posterior),apply(party_5_posterior,1,quantile,0.05)
                              ,apply(party_5_posterior,1,quantile,0.95))

DATE = (seq(party_5_enddate$enddate[1]-2,party_5_enddate$enddate[length(party_5_enddate$enddate)],1))
party_5_Final_Results = data.frame(DATE,party_5_Final_Results)
colnames(party_5_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_5_Final_Results = party_5_Final_Results[which(party_5_Final_Results$DATE==party_5_enddate$enddate[1])[1]:
                                                nrow(party_5_Final_Results),]
party_5_Final_Results	=	data.frame(party_5_Final_Results,"민중당",'party_5') 
colnames(party_5_Final_Results)[5:6]=c("NAME","NAME1")
a = left_join(DATE1,party_5_Final_Results)
FINAL = rbind(FINAL,a)}

if(length(dim(stan_mod2_party_6)>0)){
party_6_results = summary(stan_mod2_party_6)
party_6_chains = dim(party_6_results$c_summary)[3]
party_6_results_1 = party_6_results$c_summary[,,party_6_chains]

party_6_posterior = matrix(nrow=party_6_days_between_elections,ncol=10000)

for(i in 1:nrow(party_6_posterior)){
  party_6_posterior[i,] = rnorm(10000,party_6_results_1[i,1],party_6_results_1[i,2])}
party_6_Final_Results = cbind(rowMeans(party_6_posterior),apply(party_6_posterior,1,quantile,0.05)
                              ,apply(party_6_posterior,1,quantile,0.95))

DATE = (seq(party_6_enddate$enddate[1]-2,party_6_enddate$enddate[length(party_6_enddate$enddate)],1))
party_6_Final_Results = data.frame(DATE,party_6_Final_Results)
colnames(party_6_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_6_Final_Results = party_6_Final_Results[which(party_6_Final_Results$DATE==party_6_enddate$enddate[1])[1]:
                                                nrow(party_6_Final_Results),]

party_6_Final_Results=data.frame(party_6_Final_Results,"자유공화당",'party_6')
colnames(party_6_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_6_Final_Results)
FINAL = rbind(FINAL,a)}

if(length(dim(stan_mod2_party_7)>0)){
party_7_results = summary(stan_mod2_party_7)
party_7_chains = dim(party_7_results$c_summary)[3]
party_7_results_1 = party_7_results$c_summary[,,party_7_chains]

party_7_posterior = matrix(nrow=party_7_days_between_elections,ncol=10000)

for(i in 1:nrow(party_7_posterior)){
  party_7_posterior[i,] = rnorm(10000,party_7_results_1[i,1],party_7_results_1[i,2])}
party_7_Final_Results = cbind(rowMeans(party_7_posterior),apply(party_7_posterior,1,quantile,0.05)
                              ,apply(party_7_posterior,1,quantile,0.95))

DATE = (seq(party_7_enddate$enddate[1]-2,party_7_enddate$enddate[length(party_7_enddate$enddate)],1))
party_7_Final_Results = data.frame(DATE,party_7_Final_Results)
colnames(party_7_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_7_Final_Results = party_7_Final_Results[which(party_7_Final_Results$DATE==party_7_enddate$enddate[1])[1]:
                                                nrow(party_7_Final_Results),]


party_7_Final_Results=data.frame(party_7_Final_Results,"무소속1",'party_7')
colnames(party_7_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_7_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_8)>0)){
party_8_results = summary(stan_mod2_party_8)
party_8_chains = dim(party_8_results$c_summary)[3]
party_8_results_1 = party_8_results$c_summary[,,party_8_chains]

party_8_posterior = matrix(nrow=party_8_days_between_elections,ncol=10000)

for(i in 1:nrow(party_8_posterior)){
  party_8_posterior[i,] = rnorm(10000,party_8_results_1[i,1],party_8_results_1[i,2])}
party_8_Final_Results = cbind(rowMeans(party_8_posterior),apply(party_8_posterior,1,quantile,0.05)
                              ,apply(party_8_posterior,1,quantile,0.95))

DATE = (seq(party_8_enddate$enddate[1]-2,party_8_enddate$enddate[length(party_8_enddate$enddate)],1))
party_8_Final_Results = data.frame(DATE,party_8_Final_Results)
colnames(party_8_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_8_Final_Results = party_8_Final_Results[which(party_8_Final_Results$DATE==party_8_enddate$enddate[1])[1]:
                                                nrow(party_8_Final_Results),]
party_8_Final_Results=data.frame(party_8_Final_Results,"무소속2",'party_8')
colnames(party_8_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_8_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_9)>0)){
party_9_results = summary(stan_mod2_party_9)
party_9_chains = dim(party_9_results$c_summary)[3]
party_9_results_1 = party_9_results$c_summary[,,party_9_chains]

party_9_posterior = matrix(nrow=party_9_days_between_elections,ncol=10000)

for(i in 1:nrow(party_9_posterior)){
  party_9_posterior[i,] = rnorm(10000,party_9_results_1[i,1],party_9_results_1[i,2])}
party_9_Final_Results = cbind(rowMeans(party_9_posterior),apply(party_9_posterior,1,quantile,0.05)
                              ,apply(party_9_posterior,1,quantile,0.95))

DATE = (seq(party_9_enddate$enddate[1]-2,party_9_enddate$enddate[length(party_9_enddate$enddate)],1))
party_9_Final_Results = data.frame(DATE,party_9_Final_Results)
colnames(party_9_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_9_Final_Results = party_9_Final_Results[which(party_9_Final_Results$DATE==party_9_enddate$enddate[1])[1]:
                                                nrow(party_9_Final_Results),]

party_9_Final_Results=data.frame(party_9_Final_Results,"친박신당",'party_9')
colnames(party_9_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_9_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_10)>0)){
party_10_results = summary(stan_mod2_party_10)
party_10_chains = dim(party_10_results$c_summary)[3]
party_10_results_1 = party_10_results$c_summary[,,party_10_chains]

party_10_posterior = matrix(nrow=party_10_days_between_elections,ncol=10000)

for(i in 1:nrow(party_10_posterior)){
  party_10_posterior[i,] = rnorm(10000,party_10_results_1[i,1],party_10_results_1[i,2])}
party_10_Final_Results = cbind(rowMeans(party_10_posterior),apply(party_10_posterior,1,quantile,0.05)
                               ,apply(party_10_posterior,1,quantile,0.95))

DATE = (seq(party_10_enddate$enddate[1]-2,party_10_enddate$enddate[length(party_10_enddate$enddate)],1))
party_10_Final_Results = data.frame(DATE,party_10_Final_Results)
colnames(party_10_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_10_Final_Results = party_10_Final_Results[which(party_10_Final_Results$DATE==party_10_enddate$enddate[1])[1]:
                                                  nrow(party_10_Final_Results),]

party_10_Final_Results=data.frame(party_10_Final_Results,"열린민주당",'party_10')
colnames(party_10_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_10_Final_Results)
FINAL = rbind(FINAL,a)
}


if(length(dim(stan_mod2_party_11)>0)){
party_11_results = summary(stan_mod2_party_11)
party_11_chains = dim(party_11_results$c_summary)[3]
party_11_results_1 = party_11_results$c_summary[,,party_11_chains]

party_11_posterior = matrix(nrow=party_11_days_between_elections,ncol=10000)

for(i in 1:nrow(party_11_posterior)){
  party_11_posterior[i,] = rnorm(10000,party_11_results_1[i,1],party_11_results_1[i,2])}
party_11_Final_Results = cbind(rowMeans(party_11_posterior),apply(party_11_posterior,1,quantile,0.05)
                               ,apply(party_11_posterior,1,quantile,0.95))

DATE = (seq(party_11_enddate$enddate[1]-2,party_11_enddate$enddate[length(party_11_enddate$enddate)],1))
party_11_Final_Results = data.frame(DATE,party_11_Final_Results)
colnames(party_11_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_11_Final_Results = party_11_Final_Results[which(party_11_Final_Results$DATE==party_11_enddate$enddate[1])[1]:
                                                  nrow(party_11_Final_Results),]

party_11_Final_Results=data.frame(party_11_Final_Results,"바른미래당",'party_11')
colnames(party_11_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_11_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_12)>0)){
party_12_results = summary(stan_mod2_party_12)
party_12_chains = dim(party_12_results$c_summary)[3]
party_12_results_1 = party_12_results$c_summary[,,party_12_chains]

party_12_posterior = matrix(nrow=party_12_days_between_elections,ncol=10000)

for(i in 1:nrow(party_12_posterior)){
  party_12_posterior[i,] = rnorm(10000,party_12_results_1[i,1],party_12_results_1[i,2])}
party_12_Final_Results = cbind(rowMeans(party_12_posterior),apply(party_12_posterior,1,quantile,0.05)
                               ,apply(party_12_posterior,1,quantile,0.95))

DATE = (seq(party_12_enddate$enddate[1]-2,party_12_enddate$enddate[length(party_12_enddate$enddate)],1))
party_12_Final_Results = data.frame(DATE,party_12_Final_Results)
colnames(party_12_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_12_Final_Results = party_12_Final_Results[which(party_12_Final_Results$DATE==party_12_enddate$enddate[1])[1]:
                                                  nrow(party_12_Final_Results),] 

party_12_Final_Results=data.frame(party_12_Final_Results,"국민의당",'party_12')
colnames(party_12_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_12_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_13)>0)){
party_13_results = summary(stan_mod2_party_13)
party_13_chains = dim(party_13_results$c_summary)[3]
party_13_results_1 = party_13_results$c_summary[,,party_13_chains]

party_13_posterior = matrix(nrow=party_13_days_between_elections,ncol=10000)

for(i in 1:nrow(party_13_posterior)){
  party_13_posterior[i,] = rnorm(10000,party_13_results_1[i,1],party_13_results_1[i,2])}
party_13_Final_Results = cbind(rowMeans(party_13_posterior),apply(party_13_posterior,1,quantile,0.05)
                               ,apply(party_13_posterior,1,quantile,0.95))

DATE = (seq(party_13_enddate$enddate[1]-2,party_13_enddate$enddate[length(party_13_enddate$enddate)],1))
party_13_Final_Results = data.frame(DATE,party_13_Final_Results)
colnames(party_13_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_13_Final_Results = party_13_Final_Results[which(party_13_Final_Results$DATE==party_13_enddate$enddate[1])[1]:
                                                  nrow(party_13_Final_Results),]

party_13_Final_Results=data.frame(party_13_Final_Results,"대안신당",'party_13')
colnames(party_13_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_13_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_14)>0)){
party_14_results = summary(stan_mod2_party_14)
party_14_chains = dim(party_14_results$c_summary)[3]
party_14_results_1 = party_14_results$c_summary[,,party_14_chains]

party_14_posterior = matrix(nrow=party_14_days_between_elections,ncol=10000)

for(i in 1:nrow(party_14_posterior)){
  party_14_posterior[i,] = rnorm(10000,party_14_results_1[i,1],party_14_results_1[i,2])}
party_14_Final_Results = cbind(rowMeans(party_14_posterior),apply(party_14_posterior,1,quantile,0.05)
                               ,apply(party_14_posterior,1,quantile,0.95))

DATE = (seq(party_14_enddate$enddate[1]-2,party_14_enddate$enddate[length(party_14_enddate$enddate)],1))
party_14_Final_Results = data.frame(DATE,party_14_Final_Results)
colnames(party_14_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_14_Final_Results = party_14_Final_Results[which(party_14_Final_Results$DATE==party_14_enddate$enddate[1])[1]:
                                                  nrow(party_14_Final_Results),]

party_14_Final_Results=data.frame(party_14_Final_Results,"민주평화당",'party_14')
colnames(party_14_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_14_Final_Results)
FINAL = rbind(FINAL,a)

}

if(length(dim(stan_mod2_party_15)>0)){
party_15_results = summary(stan_mod2_party_15)
party_15_chains = dim(party_15_results$c_summary)[3]
party_15_results_1 = party_15_results$c_summary[,,party_15_chains]

party_15_posterior = matrix(nrow=party_15_days_between_elections,ncol=10000)

for(i in 1:nrow(party_15_posterior)){
  party_15_posterior[i,] = rnorm(10000,party_15_results_1[i,1],party_15_results_1[i,2])}
party_15_Final_Results = cbind(rowMeans(party_15_posterior),apply(party_15_posterior,1,quantile,0.05)
                               ,apply(party_15_posterior,1,quantile,0.95))

DATE = (seq(party_15_enddate$enddate[1]-2,party_15_enddate$enddate[length(party_15_enddate$enddate)],1))
party_15_Final_Results = data.frame(DATE,party_15_Final_Results)
colnames(party_15_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_15_Final_Results = party_15_Final_Results[which(party_15_Final_Results$DATE==party_15_enddate$enddate[1])[1]:
                                                  nrow(party_15_Final_Results),]

party_15_Final_Results=data.frame(party_15_Final_Results,"국가혁명배당금당",'party_15')
colnames(party_15_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_15_Final_Results)
FINAL = rbind(FINAL,a)
}

if(length(dim(stan_mod2_party_16)>0)){
party_16_results = summary(stan_mod2_party_16)
party_16_chains = dim(party_16_results$c_summary)[3]
party_16_results_1 = party_16_results$c_summary[,,party_16_chains]

party_16_posterior = matrix(nrow=party_16_days_between_elections,ncol=10000)

for(i in 1:nrow(party_16_posterior)){
  party_16_posterior[i,] = rnorm(10000,party_16_results_1[i,1],party_16_results_1[i,2])}
party_16_Final_Results = cbind(rowMeans(party_16_posterior),apply(party_16_posterior,1,quantile,0.05)
                               ,apply(party_16_posterior,1,quantile,0.95))

DATE = (seq(party_16_enddate$enddate[1]-2,party_16_enddate$enddate[length(party_16_enddate$enddate)],1))
party_16_Final_Results = data.frame(DATE,party_16_Final_Results)
colnames(party_16_Final_Results) = c('DATE','지지율','하한','상한') ## 민주당 지지율 시계열
party_16_Final_Results = party_16_Final_Results[which(party_16_Final_Results$DATE==party_16_enddate$enddate[1])[1]:
                                                  nrow(party_16_Final_Results),]
party_16_Final_Results=data.frame(party_16_Final_Results,"새로운보수당",'party_16')
colnames(party_16_Final_Results)[5:6]=c("NAME",'NAME1')
a = left_join(DATE1,party_16_Final_Results)
FINAL = rbind(FINAL,a)
}
a = region[p]

FINAL = data.frame(FINAL,a)
write.csv(FINAL,paste0(paste0('결과\\FINAL_',a),'.csv'))

K = FINAL %>% filter(!is.na(NAME)) %>% group_by(NAME1,NAME) %>% summarise(a = max(지지율))%>% data.frame
 
if(K[1,1]=="party_1"){A=party_1_posterior;G = party_1_Final_Results$DATE
party_1_results_VIC = summary(stan_mod2_party_1_VIC)
party_1_chains_VIC = dim(party_1_results_VIC$c_summary)[3]
A1 = party_1_results_VIC$c_summary[,,party_1_chains_VIC]}



if(K[1,1]=="party_2"){A=party_2_posterior;G = party_2_Final_Results$DATE

party_2_results_VIC = summary(stan_mod2_party_2_VIC)
party_2_chains_VIC = dim(party_2_results_VIC$c_summary)[3]
A1 = party_2_results_VIC$c_summary[,,party_2_chains_VIC]

}
if(K[1,1]=="party_3"){A=party_3_posterior;G = party_3_Final_Results$DATE

party_3_results_VIC = summary(stan_mod2_party_3_VIC)
party_3_chains_VIC = dim(party_3_results_VIC$c_summary)[3]
A1 = party_3_results_VIC$c_summary[,,party_3_chains_VIC]


}
if(K[1,1]=="party_4"){A=party_4_posterior;G = party_4_Final_Results$DATE

party_4_results_VIC = summary(stan_mod2_party_4_VIC)
party_4_chains_VIC = dim(party_4_results_VIC$c_summary)[3]
A1 = party_4_results_VIC$c_summary[,,party_4_chains_VIC]


}
if(K[1,1]=="party_5"){A=party_5_posterior;G = party_5_Final_Results$DATE

party_5_results_VIC = summary(stan_mod2_party_5_VIC)
party_5_chains_VIC = dim(party_5_results_VIC$c_summary)[3]
A1 = party_5_results_VIC$c_summary[,,party_5_chains_VIC]
}

if(K[1,1]=="party_6"){A=party_6_posterior;G = party_6_Final_Results$DATE

party_6_results_VIC = summary(stan_mod2_party_6_VIC)
party_6_chains_VIC = dim(party_6_results_VIC$c_summary)[3]
A1 = party_6_results_VIC$c_summary[,,party_6_chains_VIC]


}
if(K[1,1]=="party_7"){A=party_7_posterior;G = party_7_Final_Results$DATE


party_7_results_VIC = summary(stan_mod2_party_7_VIC)
party_7_chains_VIC = dim(party_7_results_VIC$c_summary)[3]
A1 = party_7_results_VIC$c_summary[,,party_7_chains_VIC]




}
if(K[1,1]=="party_8"){A=party_8_posterior;G = party_8_Final_Results$DATE


party_8_results_VIC = summary(stan_mod2_party_8_VIC)
party_8_chains_VIC = dim(party_8_results_VIC$c_summary)[3]
A1 = party_8_results_VIC$c_summary[,,party_8_chains_VIC]



}
if(K[1,1]=="party_9"){A=party_9_posterior;G = party_9_Final_Results$DATE}
if(K[1,1]=="party_10"){A=party_10_posterior;G = party_10_Final_Results$DATE}
if(K[1,1]=="party_11"){A=party_11_posterior;G = party_11_Final_Results$DATE}
if(K[1,1]=="party_12"){A=party_12_posterior;G = party_12_Final_Results$DATE}
if(K[1,1]=="party_13"){A=party_13_posterior;G = party_13_Final_Results$DATE}
if(K[1,1]=="party_14"){A=party_14_posterior;G = party_14_Final_Results$DATE}
if(K[1,1]=="party_15"){A=party_15_posterior;G = party_15_Final_Results$DATE}
if(K[1,1]=="party_16"){A=party_16_posterior;G = party_16_Final_Results$DATE}


if(K[2,1]=="party_1"){B=party_1_posterior;C=party_1_Final_Results$DATE


party_1_results_VIC = summary(stan_mod2_party_1_VIC)
party_1_chains_VIC = dim(party_1_results_VIC$c_summary)[3]
B1 = party_1_results_VIC$c_summary[,,party_1_chains_VIC]



}
if(K[2,1]=="party_2"){B=party_2_posterior;C=party_2_Final_Results$DATE

party_2_results_VIC = summary(stan_mod2_party_2_VIC)
party_2_chains_VIC = dim(party_2_results_VIC$c_summary)[3]
B1 = party_2_results_VIC$c_summary[,,party_2_chains_VIC]



}
if(K[2,1]=="party_3"){B=party_3_posterior;C=party_3_Final_Results$DATE

party_3_results_VIC = summary(stan_mod2_party_3_VIC)
party_3_chains_VIC = dim(party_3_results_VIC$c_summary)[3]
B1 = party_3_results_VIC$c_summary[,,party_3_chains_VIC]

}
if(K[2,1]=="party_4"){B=party_4_posterior;C=party_4_Final_Results$DATE

party_4_results_VIC = summary(stan_mod2_party_4_VIC)
party_4_chains_VIC = dim(party_4_results_VIC$c_summary)[3]
B1 = party_4_results_VIC$c_summary[,,party_4_chains_VIC]


}
if(K[2,1]=="party_5"){B=party_5_posterior;C=party_5_Final_Results$DATE
party_5_results_VIC = summary(stan_mod2_party_5_VIC)
party_5_chains_VIC = dim(party_5_results_VIC$c_summary)[3]
B1 = party_5_results_VIC$c_summary[,,party_5_chains_VIC]



}
if(K[2,1]=="party_6"){B=party_6_posterior;C=party_6_Final_Results$DATE
party_6_results_VIC = summary(stan_mod2_party_6_VIC)
party_6_chains_VIC = dim(party_6_results_VIC$c_summary)[3]
B1 = party_6_results_VIC$c_summary[,,party_6_chains_VIC]




}
if(K[2,1]=="party_7"){B=party_7_posterior;C=party_7_Final_Results$DATE

party_7_results_VIC = summary(stan_mod2_party_7_VIC)
party_7_chains_VIC = dim(party_7_results_VIC$c_summary)[3]
B1 = party_7_results_VIC$c_summary[,,party_7_chains_VIC]



}
if(K[2,1]=="party_8"){B=party_8_posterior;C=party_8_Final_Results$DATE

party_8_results_VIC = summary(stan_mod2_party_8_VIC)
party_8_chains_VIC = dim(party_8_results_VIC$c_summary)[3]
B1 = party_8_results_VIC$c_summary[,,party_8_chains_VIC]



}
if(K[2,1]=="party_9"){B=party_9_posterior;C=party_9_Final_Results$DATE}
if(K[2,1]=="party_10"){B=party_10_posterior;C=party_10_Final_Results$DATE}
if(K[2,1]=="party_11"){B=party_11_posterior;C=party_11_Final_Results$DATE}
if(K[2,1]=="party_12"){B=party_12_posterior;C=party_12_Final_Results$DATE}
if(K[2,1]=="party_13"){B=party_13_posterior;C=party_13_Final_Results$DATE}
if(K[2,1]=="party_14"){B=party_14_posterior;C=party_14_Final_Results$DATE}
if(K[2,1]=="party_15"){B=party_15_posterior;C=party_15_Final_Results$DATE}
if(K[2,1]=="party_16"){B=party_16_posterior;C=party_16_Final_Results$DATE}

k  = min(nrow(A),nrow(B))

differ = cbind(rowMeans(A[(nrow(A)-k+1):nrow(A),] - B[(nrow(B)-k+1):nrow(B),]),
               apply(A[(nrow(A)-k+1):nrow(A),] - B[(nrow(B)-k+1):nrow(B),],1,min),
               apply(A[(nrow(A)-k+1):nrow(A),] - B[(nrow(B)-k+1):nrow(B),],1,max))

if(length(C)<length(G)){
  d = length(C)
  d = k - d
  differ = differ[(d+1):nrow(differ),]
  differ = data.frame(C,differ)
} else{
  d = length(G)
  d = k - d
  differ = differ[(d+1):nrow(differ),]
  differ = data.frame(G,differ)}

colnames(differ) = c('DATE','차이','상한','하한')
FINAL_differ = left_join(DATE1,differ)
FINAL_differ = data.frame(FINAL_differ,a)

d = min(nrow(A1),nrow(B1));d=d-1
A1_vics = B1_vics =  matrix(nrow=d,ncol=100)

for(j in 1:100){
  for(i in d:1){
    
    P1 = A1[((nrow(A1_vics)-d)+1):(nrow(A1_vics)),]
    P2 = B1[((nrow(B1_vics)-d)+1):(nrow(B1_vics)),]
    
    x = list(a = rnorm(10000,P1[i,1],P1[i,2]),
             b = rnorm(10000,P2[i,1],P2[i,2]))
    
    out <- overlap( x,100)
    a=out$xpoints[[1]]
    
      A1_vics[i,j] = mean(x$a>a)
      B1_vics[i,j] = mean(x$b>a)
      
    }}

A1_VICS = 100*cbind(rowMeans(A1_vics,na.rm=T),apply(A1_vics,1,min,na.rm=T),apply(A1_vics,1,max,na.rm=T))
B1_VICS = 100*cbind(rowMeans(B1_vics,na.rm=T),apply(B1_vics,1,min,na.rm=T),apply(B1_vics,1,max,na.rm=T))


if(length(C)<length(G)){
  d = length(C)
  d = k - d
  A1_VICS = A1_VICS[(d+1):nrow(A1_VICS),]
  A1_VICS = data.frame(C,A1_VICS)
  B1_VICS = B1_VICS[(d+1):nrow(B1_VICS),]
  B1_VICS = data.frame(C,B1_VICS)

  } else{
  d = length(G)
  d = k - d
  A1_VICS = A1_VICS[(d+1):nrow(A1_VICS),]
  A1_VICS = data.frame(C,A1_VICS)
  B1_VICS = B1_VICS[(d+1):nrow(B1_VICS),]
  B1_VICS = data.frame(C,B1_VICS)
  }

colnames(A1_VICS) = c('DATE','승률','상한','하한')
colnames(B1_VICS) =  c('DATE','승률','상한','하한')

A1_VICS = left_join(DATE1,A1_VICS)
A1_VICS = data.frame(A1_VICS,as.character(K[1,2]))
colnames(A1_VICS)[5] = 'NAME'

B1_VICS = left_join(DATE1,B1_VICS)
B1_VICS = data.frame(B1_VICS,as.character(K[2,2]))
colnames(B1_VICS)[5] = 'NAME'
a = region[p]
FINAL_VICS=rbind(A1_VICS,B1_VICS)
FINAL_VICS = data.frame(FINAL_VICS,a)

write.csv(FINAL_VICS,paste0(paste0('결과\\FINAL_VICS_',a),'.csv'))
write.csv(FINAL_differ,paste0(paste0('결과\\FINAL_differ',a),'.csv'))

rm(list=ls())
}
