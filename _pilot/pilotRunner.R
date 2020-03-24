rm(list=ls())

setwd("/Users/j/Documents/writings/2020/20200303_poll_aggregater/_pilot")

library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 1)

# ggplot 한글깨짐 대비
library(extrafont)
# font_import()
font_name = "NanumGothic"
par(family="NanumGothic")

## preprocess_global
## todo: use googlesheet4 to import sheet directly

local_data_filename <- "data/2020_local_utf8.csv"
house_effect_output_filename <- "house_effect/house_effect.csv"
TOTAL_RESULT_FILENAME <- "result/total_result.csv"

global_data <- read.csv("data/2020_global_utf8.csv") # 전국비례데이터
local_data <- read.csv(local_data_filename) # 전국지방데이터

party_col_index_start <- (grep("합계", colnames(global_data))+1)
party_col_index_end <- (grep("기타", colnames(global_data))-1)

party_names = colnames(global_data)[(party_col_index_start):(party_col_index_end)]

party_colnames = sprintf("party_%d", 1:length(party_names))

colnames(global_data) = c('ID','Title','startdate','durringdate','enddate','area1','area2','area3','press','house','method','vir','ARS','N', 'rate','sum', party_colnames , 'OTHERS','NONE','nonres','link')

global_data[is.na(global_data)]<-0


# eliminate the effect of no response

global_data[,which(colnames(global_data)==party_colnames[1]):which(colnames(global_data)==party_colnames[length(party_colnames)])] = global_data[,which(colnames(global_data)==party_colnames[1]):which(colnames(global_data)==party_colnames[length(party_colnames)])] / rowSums(global_data[,which(colnames(global_data)==party_colnames[1]):which(colnames(global_data)==party_colnames[length(party_colnames)])])*100

house_list <- data.frame(as.character(data.frame(table(global_data$house))[which(data.frame(table(global_data$house))[,2]>=2),1]))
colnames(house_list) <- c("house_name")

global_data$startdate <- as.Date(as.character(global_data$startdate))
global_data$durringdate <- as.Date(as.character(global_data$durringdate))
global_data$enddate <- as.Date(as.character(global_data$enddate))
global_data <- arrange(global_data, global_data$startdate)

START_DATE <- as.Date(as.character("2019-12-20"))
END_DATE <- Sys.Date() # today


######## STEP1: HOUSE EFFECT
num_of_iterations = 10000
source("do_some_for_calculate_house_effect.R")

global_result <- data.frame()
house_effect <- data.frame()
aggregated_result <- list()

# if(file.exists("house_effect/house_effect.csv")){
#   print("previous house_effect.csv exists: removed.")
#   file.remove("house_effect/house_effect.csv")
# }

for (i in 1:length(party_names)){
  aggregated_result <- calculate_house_effect(i)
  global_result <- aggregated_result[[1]]
  house_effect <- aggregated_result[[2]]
}


write.csv(global_result, file="result/global_result.csv")
write.csv(house_effect, file=house_effect_output_filename)
# house_effect <- read.csv(file = house_effect_output_filename) # 한 function에서 두 df를 return할 수 없어서 csv로 저장하여 받음

######## STEP2: LOCAL EXPECTATION

cat("STEP2: Calculating local expectation\n")

# for testing: comment when producing
# rm(list=ls())
# setwd("/home/ruser/_analysis/_pilot/")
# local_data_filename <- "data/2020_local_utf8.csv"
# house_effect_output_filename <- "house_effect/house_effect.csv"
# TOTAL_RESULT_FILENAME <- "result/total_result.csv"
# local_data <- read.csv(local_data_filename)
# house_effect <- read.csv(house_effect_output_filename)
#### END for testing



# MBC: 조사가 없더라도 최초 조사일부터 최종 투표일까지 날짜가 기록되길 바람
FULL_DATE <- data.frame(seq(as.Date("2019-12-31"), as.Date("2020-04-16"),1))
colnames(FULL_DATE) <- "Date"

local_data[is.na(local_data)] <- 0   # na 자료를 0으로 변환. 

SIGNIFICANT_LEVEL = 0.90 # 유의수준
z <- qnorm((1-SIGNIFICANT_LEVEL)/2, lower.tail=F) # critical value for norm dist

local_party_col_index_start <- (grep("합계", colnames(local_data))+1)
local_party_col_index_end <- (grep("기타", colnames(local_data))-1)

local_party_names = colnames(local_data)[(local_party_col_index_start):(local_party_col_index_end)]

colnames(local_data)[which(names(local_data)=="조사기관")] <- "house"

local_party_colnames = sprintf("local_party_%d", 1:length(local_party_names))

# 지지율 계산

local_data[,which(colnames(local_data)==local_party_names[1]):which(colnames(local_data)==local_party_names[length(local_party_names)])] <- 100 * local_data[,which(colnames(local_data)==local_party_names[1]):which(colnames(local_data)==local_party_names[length(local_party_names)])]/rowSums(local_data[,which(colnames(local_data)==local_party_names[1]):which(colnames(local_data)==local_party_names[length(local_party_names)])])

# house_effect wide form transformation

house_effect <- house_effect %>% select(-1) # drop the first column
house_effect_wide <- house_effect %>% reshape(direction='wide', idvar='house', timevar='party')
house_effect_wide[is.na(house_effect_wide)] <- 0

local_data <- local_data %>%
  left_join(house_effect_wide, by="house")

local_data[is.na(local_data)] <- 0

source("apply_house_effect.R")

local_data <- apply_house_effect() # partyname_fixed  e.g., "더불어민주당_fixed"

colnames_for_result <- c('날짜','지역','정당','지지율','하한','상한','승률','승률하한','승률상한') # 맨 나중에 붙이자. 
total_result <- data.frame() # 빈 df 생성. 
total_prob <- data.frame()

source("functions_sigma2.R") # 함수는 따로 부름

colnames(local_data)[which(names(local_data)=="선거구_2")] <- "area3"
colnames(local_data)[which(names(local_data)=="조사시작")] <- "startdate"
colnames(local_data)[which(names(local_data)=="중간날짜")] <- "duringdate"
colnames(local_data)[which(names(local_data)=="조사끝")] <- "enddate"
colnames(local_data)[which(names(local_data)=="응답자수.N..응답완료사례수.")] <- "N"


regions <- unique(local_data$area3)

sprintf("MSG: 총 %d 개의 지역을 처리합니다. ", length(regions))

for(region in regions){
  # region <- "경기이천시"# for test ################
  print(sprintf("MSG: 처리 지역: %s", region))
  num_of_obs <- local_data %>%
    filter(area3 == region) %>%
    tally %>%
    as.integer
  
  print(sprintf("MSG: 관측수: %d", num_of_obs))
  
  region_data <- local_data %>% filter(area3==region)
  
  region_data$startdate <- as.Date(as.character(region_data$startdate))
  region_data$duringdate <- as.Date(as.character(region_data$duringdate))
  region_data$enddate <- as.Date(as.character(region_data$enddate))
  
  region_data <- arrange(region_data, region_data$enddate)
  
  if (num_of_obs>1){
    for(party in local_party_names){
      total_result <- calculate_ci_by_party(party)
    }
    total_prob <- calculate_probability()
  } else if (num_of_obs==1){
    # do if num of obs is 1
  } else {
    stop("FATAL:check the data. Number of obs is not positive integer")
  }
}

write.csv(true_real_result, file=sprintf("result_%s_.csv",TOTAL_RESULT_FILENAME))

# hi there
cat("Done.\n")


