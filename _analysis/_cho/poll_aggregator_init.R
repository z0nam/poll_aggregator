# Poll data Aggregator for Korean Senate Election 2020
# 최은철, 조남운


# init. 자신의 컴퓨터 환경에 맞춰서 작성할 것. 

## data import

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

# non_rews: 무응답, rate: 응답률, weight: 가중치, N: 응답자수, house: 조사기관
# area1: 광역, area2: 선거구, area3: 광역+선거구
# press: 의뢰기관

# make rates to percentage  (x 100)
data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] = 
  data[,which(colnames(data)=='SAE_raw'):which(colnames(data)=='UNK3')] * 100

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


