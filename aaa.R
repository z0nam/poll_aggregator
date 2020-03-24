library(tseries);library(forecast);library(lmtest);library(urca); 
library(astsa);library(ggplot2);library(dplyr);library(TSA)
setwd('C:\\Users\\environ\\Dropbox\\DOC\\여론조사')
DATA = read.csv('20대data.csv')
colnames(DATA) = c('startdate','durringdate','enddate','area1','area2','area3','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','house','press','method','ARS','N','rate','weight','age')
Chongro = DATA %>% filter(area3=='서울종로구')


