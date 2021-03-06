# ---
#   title: "Poll Aggregator with sigma 3"
# author: "Namun Cho"
# date: "3/10/2020"
# output: html_document
# ---
#   
#   ```{r setup, include=FALSE}
rm(list=ls())
# knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)
library(knitr)

rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores() - 2)
options(mc.cores = 1) # seg falut 11 때문에 (메모리 부족) core는 1만 돌려야 한다. 

library(extrafont)   # 실행한 적 없는 경우 콘솔에서 font_import() 한 번 실행할 것. (ttf import to R) 시간 오래 걸림, ggplot2에서 한글 깨지지 않는 경우 본 행은 무시해도 무방함. 
theme_update(text=element_text(family="NanumGothic"))

setwd('~/_analysis/_cho')
FILENAME="20thData_3.csv" # _toy: 경우의 수 별 4개 지역 로딩
# FILENAME="20대data.csv" # 전체 지역 로딩

#library(reticulate)
#use_python("/Library/Frameworks/Python.framework/Versions/3.7/bin/python3")

SIGNIFICANT_LEVEL = 0.99 # 유의수준
z <- qnorm((1-SIGNIFICANT_LEVEL)/2, lower.tail=F) # critical value for norm dist

# todo: date 최소 최대 문자열에서 찾아서 
YEAR <- 2016

START_DATE <- as.Date(as.character("2016-03-10"))
END_DATE <- as.Date(as.character("2016-04-07"))
days_between_elections <- as.integer(diff(as.Date(c(START_DATE, END_DATE))))  + 1

Y_MIN <- 0
Y_MAX <- 100

# 해당 선거시기의 최대 정당 리스트를 순서대로 작성할 것. 

PartyList <- list(SAE='새누리당',MIN='민주당',PEOPLE='국민의당',JUS='정의당',UNK1='무소속1',UNK2='무소속2',UNK3='무소속3')

cat("Reading data...\n")

data <- read.csv(FILENAME)

cat ("Done!\n")

colnames(data) = c('startdate','durringdate','enddate',
                   'area1','area2','area3',
                   'SAE_raw','MIN_raw','PEOPLE_raw',
                   'JUS_raw','UNK1_raw','UNK2_raw','UNK3_raw',
                   'SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3',
                   'non_res',
                   'house','press','method','ARS','N','rate',
                   'weight','age')


colnames_for_result <- c('날짜','지역','정당','지지율','하한','상한','승률','승률하한','승률상한') # 맨 나중에 붙이자. 
total_result <- data.frame() # 빈 df 생성. 
total_prob <- data.frame()

source("functions_sigma3.R") # 함수는 따로 부름

# colname과 PartyList key name이 일치해야 한다. 

# non_rews: 무응답, rate: 응답률, weight: 가중치, N: 응답자수, house: 조사기관
# area1: 광역, area2: 선거구, area3: 광역+선거구
# press: 의뢰기관

# 20대 응답 분포. 253-149 = 104지역은 아예 poll 등록이 되어 있지 않다. 

# (count) |
#   응답률 |      Freq.     Percent        Cum.
# ------------+-----------------------------------
#   1 |         44       29.53       29.53
#   2 |         27       18.12       47.65
#   3 |         33       22.15       69.80
#   4 |         16       10.74       80.54
#   5 |         10        6.71       87.25
#   6 |          4        2.68       89.93
#   7 |          6        4.03       93.96
#   8 |          1        0.67       94.63
#   9 |          1        0.67       95.30
#  10 |          1        0.67       95.97
#  11 |          2        1.34       97.32
#  12 |          3        2.01       99.33
#  15 |          1        0.67      100.00
# ------------+-----------------------------------
#   Total |        149      100.00

# ```
# 
# ```{r poll_aggregator_init}
# ```

## 계산절차 (poll_aggregator.R)

# - 판단1. 조사 횟수는 X 이상인가? (X: 의미있는 조사수. 일단 2로 세팅. )
# - 1일 경우: 조사된 수치에 오차구간 sd/2/sqrt(N) 으로 유의수준 세팅 (95%sd=1.96) --> pass
# - 3 이상일 경우: 계산 시행 --> pass
# - 2 이상일 경우: 별도로 관리
# - 계산: 정당수 체크. 2020에서는 모두 0인 조사는 없는 것으로 처리. 1% 이상이었다가 0이었다가 하는 경우는 양당구도 조사로 인해 배제되었을 가능성이 있음. --> 여기에서는 간접 추정하지만 21대 조사시에는 조사결과가 0인 것과 미조사인 부분은 구분해야 함. 
# - todo: 실제 21대 조사시에는 조사당수 (M), 그리고 미조사된 부분은 0이 아닌 missing (na) 처리 해야 함. 
# - 이렇게 할 경우 지역별로, 조사별로 측정 시간이 모두 달라지는 문제가 있음. stan_poll 3 에 해당하는 상황. (ALP에 house 별로 조사를 시행함)
# - 지역별 수치는 조사수가 부족하여 house effect를 사후적으로 넣거나 넣지 않을 예정임. 최소한 이 지역별 루프에서는 산출하지 않음. 
# 
# ### 조정해야 할 것
# 
# - stan file 의 편차 관련 부분은 관측수에 따라 크게/작게 조정해야 함. 가령 측정수가 3일 경우는 크게 잡고, 10일 경우는 적게 잡아야 하는데, 얼마나 잡아야 하는가에 대한 선험적인 기준이 없음. 여러 가지로 돌려보고 20대 총선을 가장 잘 설명하는 것으로 결정하고 넣어야 할 것으로 보임. 
# - 이를 위해 stan file에 위 수치에 대한 인자를 넘기는 것으로 수정할 것. 
# 
# ```{r calculate by region

regions <- unique(data$area3) # 지역 뽑아내기. 
sprintf("MSG: 총 %d 개의 지역을 처리합니다. ", length(regions))
for (region in regions){
  print(sprintf("MSG: 처리 지역: %s", region))
  num_of_obs <- data %>% 
    filter(area3==region)
  num_of_obs <- length(num_of_obs$startdate)
  print(sprintf("MSG: 관측수: %d", num_of_obs))
  localdata <- data %>% filter(area3==region)
  
  localdata[,which(colnames(localdata)=='SAE_raw'):which(colnames(localdata)=='UNK3')] = 
    localdata[,which(colnames(localdata)=='SAE_raw'):which(colnames(localdata)=='UNK3')] * 100
  
  localdata$startdate <- as.Date(as.character(localdata$startdate))
  localdata$durringdate <- as.Date(as.character(localdata$durringdate))
  localdata$enddate <- as.Date(as.character(localdata$enddate))
  localdata = arrange(localdata,localdata$enddate)
  
  if (num_of_obs>1){
    for( party in names(PartyList)){
      total_result <- calculate_ci_by_party(party)
    }
    total_prob <- calculate_probability()
  } else if (num_of_obs==1) {
    #read_chunk("single_poll.R")
    # 측정수가 하나뿐일 때 쓸 스크립트. 
    # 작성: 최은철, 수정: 조남운
    
    # 혹시라도 100을 곱한 것이 계산에 문제가 있을까봐 나중에 100을 곱했음. 
    
    #data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] = 
    #  data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] * 100
    
  }
  
  else {
    stop("FATAL:check the data. Number of obs is not positive integer")
  }
  
}


true_real_result <- left_join(total_result, total_prob, by=c("date", "region", "party"))
write.csv(true_real_result, file=sprintf("result_%s_.csv",FILENAME))
# ```
