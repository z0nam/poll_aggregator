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

data <- read.csv("20대data.csv")
colnames(data) = c('startdate','durringdate','enddate','area1','area2','area3','SAE_raw','MIN_raw','PEOPLE_raw',
                   'JUS_raw','UNK1_raw','UNK2_raw','UNK3_raw','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','non_res',
                   'house','press','method','ARS','N','rate','weight','age')

#data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] = 
#  data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] * 100

data = data %>% filter(area3=='대전중구') ## 지역 선택

data$startdate <- as.Date(as.character(data$startdate))
data$durringdate <- as.Date(as.character(data$durringdate))
data$enddate <- as.Date(as.character(data$enddate))
data = arrange(data,data$startdate)

head(data)

## Initialization

START_DATE <- as.Date(as.character("2016-03-10"))
END_DATE <- as.Date(as.character("2016-04-07"))

MU_START_SAE = data %>% group_by(enddate) %>% summarise(a = mean(SAE_raw))
MU_START_SAE <- MU_START_SAE$a[1] # 새누리당 초기값

MU_START_MIN = data %>% group_by(enddate) %>% summarise(a = mean(MIN_raw))
MU_START_MIN <- MU_START_MIN$a[1] # 민주당 초기값

MU_START_PEOPLE = data %>% group_by(enddate) %>% summarise(a = mean(PEOPLE_raw))
MU_START_PEOPLE <- MU_START_PEOPLE$a[1] # 국민의당 초기값

MU_START_JUS = data %>% group_by(enddate) %>% summarise(a = mean(JUS_raw))
MU_START_JUS <- MU_START_JUS$a[1] # 정의당 초기값

MU_START_UNK1 = data %>% group_by(enddate) %>% summarise(a = mean(UNK1_raw))
MU_START_UNK1 <- MU_START_UNK1$a[1] # 무소속 1 초기값

MU_START_UNK2 = data %>% group_by(enddate) %>% summarise(a = mean(UNK2_raw))
MU_START_UNK2 <- MU_START_UNK2$a[1] # 무소속 2 초기값

MU_START_UNK3 = data %>% group_by(enddate) %>% summarise(a = mean(UNK3_raw))
MU_START_UNK3 <- MU_START_UNK3$a[1] # 무소속 3 초기값


SE_SAE = sqrt(MU_START_SAE*(1-MU_START_SAE)/data$N)
SE_MIN = sqrt(MU_START_MIN*(1-MU_START_MIN)/data$N)
SE_PEOPLE = sqrt(MU_START_PEOPLE*(1-MU_START_PEOPLE)/data$N)
SE_JUS = sqrt(MU_START_JUS*(1-MU_START_JUS)/data$N)
SE_UNK1 = sqrt(MU_START_UNK1*(1-MU_START_UNK1)/data$N)
SE_UNK2 = sqrt(MU_START_UNK2*(1-MU_START_UNK2)/data$N)
SE_UNK3 = sqrt(MU_START_UNK3*(1-MU_START_UNK3)/data$N)

SAE_vics1 = rep(0,100)
MIN_vics1 = rep(0,100)
PEOPLE_vics1 = rep(0,100)
JUS_vics1 = rep(0,100)
UNK1_vics1 = rep(0,100)
UNK2_vics1 = rep(0,100)
UNK3_vics1 = rep(0,100)

SAE_vics2 = rep(0,100)
MIN_vics2 = rep(0,100)
PEOPLE_vics2 = rep(0,100)
JUS_vics2 = rep(0,100)
UNK1_vics2 = rep(0,100)
UNK2_vics2 = rep(0,100)
UNK3_vics2 = rep(0,100)

for(i in 1:100){
  
SAE_posterior = rnorm(10000,MU_START_SAE,SE_SAE)*100
MIN_posterior = rnorm(10000,MU_START_MIN,SE_MIN)*100
PEOPLE_posterior = rnorm(10000,MU_START_PEOPLE,SE_PEOPLE)*100
JUS_posterior = rnorm(10000,MU_START_JUS,SE_JUS)*100
UNK1_posterior = rnorm(10000,MU_START_UNK1,SE_UNK1)*100
UNK2_posterior = rnorm(10000,MU_START_UNK2,SE_UNK2)*100
UNK3_posterior = rnorm(10000,MU_START_UNK3,SE_UNK3)*100

maxs = c(max(SAE_posterior),max(MIN_posterior),max(PEOPLE_posterior),max(JUS_posterior),
         max(UNK1_posterior),max(UNK2_posterior),max(UNK3_posterior))

mins = c(min(SAE_posterior),min(MIN_posterior),min(PEOPLE_posterior),min(JUS_posterior),
         min(UNK1_posterior),min(UNK2_posterior),min(UNK3_posterior))

maxs_SAE = maxs[-1]
mins_SAE = mins[-1]

maxs_MIN = maxs[-2]
mins_MIN = mins[-2]

maxs_PEOPLE = maxs[-3]
mins_PEOPLE = mins[-3]

maxs_JUS = maxs[-4]
mins_JUS = mins[-4]

maxs_UNK1 = maxs[-5]
mins_UNK1 = mins[-5]

maxs_UNK2 = maxs[-6]
mins_UNK2 = mins[-6]

maxs_UNK3 = maxs[-7]
mins_UNK3 = mins[-7]

a = mean(SAE_posterior>=max(maxs_SAE))
b = mean(SAE_posterior>=max(mins_SAE))
SAE_vics1[i] = a
SAE_vics2[i] = b

a = mean(MIN_posterior>=max(maxs_MIN))
b = mean(MIN_posterior>=max(mins_MIN))
MIN_vics1[i] = a
MIN_vics2[i] = b

a = mean(PEOPLE_posterior>=max(maxs_PEOPLE))
b = mean(PEOPLE_posterior>=max(mins_PEOPLE))
PEOPLE_vics1[i] = a
PEOPLE_vics2[i] = b

a = mean(JUS_posterior>=max(maxs_JUS))
b = mean(JUS_posterior>=max(mins_JUS))
JUS_vics1[i] = a
JUS_vics2[i] = b

a = mean(UNK1_posterior>=max(maxs_UNK1))
b = mean(UNK1_posterior>=max(mins_UNK1))
UNK1_vics1[i] = a
UNK1_vics2[i] = b

a = mean(UNK2_posterior>=max(maxs_UNK2))
b = mean(UNK2_posterior>=max(mins_UNK2))
UNK2_vics1[i] = a
UNK2_vics2[i] = b


a = mean(UNK3_posterior>=max(maxs_UNK3))
b = mean(UNK3_posterior>=max(mins_UNK3))
UNK3_vics1[i] = a
UNK3_vics2[i] = b
}




