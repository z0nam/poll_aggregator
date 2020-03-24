rm(list=ls())

setwd("/Users/j/Documents/writings/2020/20200303_poll_aggregater/_analysis/_pilot-2")

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

global_data <- read.csv("data/2020_global_utf8.csv")
house_effect_output_filename <- "house_effect/house_effect.csv"

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

source("do_some_for_calculate_house_effect.R")

global_result <- data.frame()

if(file.exists("house_effect/house_effect.csv")){
  print("previous house_effect.csv exists: removed.")
  file.remove("house_effect/house_effect.csv")
}

for (i in 1:length(party_names)){
  global_result <- calculate_house_effect(i)
}

write.csv(global_result, file="result/global_result.csv")

cat("Done.\n")
