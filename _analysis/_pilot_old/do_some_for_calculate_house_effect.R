calculate_house_effect <- function(party_index){
  # party_index <- 7 # to test
  
  print(sprintf("Start calculating house effect for party: %s (%s)", party_colnames[party_index], party_names[party_index] ))
  
  party_colname = party_colnames[party_index]
  
  # MU_START_MI = global_data %>% filter(!is.na(party_1))%>%group_by(enddate) %>% summarise(a = mean(party_1))
  # MU_START_MI <- MU_START_MI$a[1] # 새누리당 초기값
  
  MU_START <- global_data %>% 
    select(c("enddate", party_colname)) %>%
    filter(.[party_colname] != 0) %>% 
    filter(enddate == min(enddate)) %>%
    select(c(party_colname))%>%
    summarise(mean(get(party_colname))) %>%
    as.double()
  
  MU_FINISH <- global_data %>%
    select(c("enddate", party_colname)) %>%
    filter(.[party_colname] != 0) %>%
    filter(enddate == max(enddate)) %>%
    select(c(party_colname)) %>%
    summarise(mean(get(party_colname))) %>%
    as.double()
    
  end_date <- global_data %>% 
    filter(.[party_colname] != 0) %>%
    select(enddate) %>%
    unlist() %>%
    as.Date(origin="1970-1-1")
  
  start_date <- global_data %>%
    filter(.[party_colname] != 0) %>%
    select(startdate) %>%
    unlist() %>%
    as.Date(origin="1970-1-1")
  
  
  ## house management
  
  all_polls <- global_data %>% 
    filter(.[party_colname] != 0) %>%
    mutate(MidDate = startdate + (enddate - startdate) / 2,
           MidDateNum = as.integer(MidDate -sort(startdate[1])+1),  # ie number of days since starting election (이부분에 대한 논의가 필요합니다.)
           p = get(party_colname) / 100,
           se = sqrt(p * (1- p) / N) * 100,
           org = fct_reorder(house, get(party_colname)))
  
  poll_orgs <- as.character(data.frame(table(all_polls$org))[which(data.frame(table(all_polls$org))[,2]>=2),1])
  
  mid_date <- all_polls %>%
    filter(.[party_colname] != 0) %>%
    select(MidDate) %>%
    unlist() %>%
    as.Date(origin="1970-1-1")
  
  days_between_elections = as.integer(max(mid_date) - min(mid_date) + 3)
  
  d3 <- list(
    mu_start = MU_START,
    mu_finish = MU_FINISH,
    n_days = days_between_elections
  )
  
  # count number of research institutes
  number_of_orgs <- length(poll_orgs)
  
  if(number_of_orgs < 2) {
    print(sprintf("party %s has no record of poll (%d): return", party_names[party_index], number_of_orgs))
    return(global_result)
  }
  
  if(number_of_orgs > 10)stop(sprintf("여론조사기관의 총 갯수가 %d >10 입니다. 현재 이 모듈은 10개 이상의 여론조사기관을 다룰 수 없습니다. 즉시 010-6343-2884 (조남운)에게 연락주세요")) # todo: 10개로 늘려라. 
  
  # switch by number_of_orgs
  
  while(TRUE){
    p1 <- filter(all_polls, org == poll_orgs[[1]])
    d3 <- append(
      d3,
      list(
        y1_values = as.numeric(unlist(p1[party_colname])),
        y1_days = p1$MidDateNum,
        y1_n = nrow(p1),
        y1_se = p1$se
      )
    )
    if(number_of_orgs==1) break
    p2 <- filter(all_polls, org == poll_orgs[[2]])
    d3 <- append(
      d3,
      list(
        y2_values = as.numeric(unlist(p2[party_colname])),
        y2_days = p2$MidDateNum,
        y2_n = nrow(p2),
        y2_se = p2$se
      )
    )
    if(number_of_orgs==2) break
    p3 <- filter(all_polls, org == poll_orgs[[3]])
    d3 <- append(
      d3,
      list(
        y3_values = as.numeric(unlist(p3[party_colname])),
        y3_days = p3$MidDateNum,
        y3_n = nrow(p3),
        y3_se = p3$se
      )
    )
    if(number_of_orgs==3) break
    p4 <- filter(all_polls, org == poll_orgs[[4]])
    d3 <- append(
      d3,
      list(
        y4_values = as.numeric(unlist(p4[party_colname])),
        y4_days = p4$MidDateNum,
        y4_n = nrow(p4),
        y4_se = p4$se
      )
    )
    if(number_of_orgs==4) break
    p5 <- filter(all_polls, org == poll_orgs[[5]])
    d3 <- append(
      d3,
      list(
        y5_values = as.numeric(unlist(p5[party_colname])),
        y5_days = p5$MidDateNum,
        y5_n = nrow(p5),
        y5_se = p5$se
      )
    )
    if(number_of_orgs==5) break
    p6 <- filter(all_polls, org == poll_orgs[[6]])
    d3 <- append(
      d3,
      list(
        y6_values = as.numeric(unlist(p6[party_colname])),
        y6_days = p6$MidDateNum,
        y6_n = nrow(p6),
        y6_se = p6$se
      )
    )
    if(number_of_orgs==6) break
    p7 <- filter(all_polls, org == poll_orgs[[7]])
    d3 <- append(
      d3,
      list(
        y7_values = as.numeric(unlist(p7[party_colname])),
        y7_days = p7$MidDateNum,
        y7_n = nrow(p7),
        y7_se = p7$se
      )
    )
    if(number_of_orgs==7) break
    p8 <- filter(all_polls, org == poll_orgs[[8]])
    d3 <- append(
      d3,
      list(
        y8_values = as.numeric(unlist(p8[party_colname])),
        y8_days = p8$MidDateNum,
        y8_n = nrow(p8),
        y8_se = p8$se
      )
    )
    if(number_of_orgs==6) break
    p9 <- filter(all_polls, org == poll_orgs[[9]])
    d3 <- append(
      d3,
      list(
        y9_values = as.numeric(unlist(p9[party_colname])),
        y9_days = p9$MidDateNum,
        y9_n = nrow(p9),
        y9_se = p9$se
      )
    )
    if(number_of_orgs==6) break
    p10 <- filter(all_polls, org == poll_orgs[[10]])
    d3 <- append(
      d3,
      list(
        y10_values = as.numeric(unlist(p10[party_colname])),
        y10_days = p10$MidDateNum,
        y10_n = nrow(p10),
        y10_se = p10$se
      )
    )
    break
  }
  
  stan_filename <- sprintf("stan/oz-polls-3_%d.stan", number_of_orgs)
  print(sprintf("stan filename = %s", stan_filename))
  
  stan_mod3 <- stan(file = stan_filename, data = d3,
                       control = list(max_treedepth = 15,
                                      adapt_delta = 0.8),
                       iter = 4000)
  
  
  ## end by num of orgs
  
  results = summary(stan_mod3)
  chains = dim(results$c_summary)[3]
  results_1 = results$c_summary[,,chains]
  
  house_effect <- results_1[(nrow(results_1)-2-number_of_orgs+1):(nrow(results_1)-2),1]
  house_effect_by_party <- data.frame(
    party = party_names[party_index],
    orgs = poll_orgs,
    house_effect = house_effect
  )
  write.csv(house_effect_by_party, file=sprintf("house_effect/%s.csv", party_names[party_index]))
  
  posterior = matrix(nrow=as.integer(max(end_date)-min(start_date) ),ncol=10000)
  for(i in 1:nrow(posterior)){
    posterior[i,] = rnorm(10000, results_1[i,1], results_1[i,2])}
  Final_Results = cbind(rowMeans(posterior),apply(posterior,1,quantile,0.05)
                           ,apply(posterior,1,quantile,0.95))
  
  DATE = seq(min(end_date), min(end_date)+nrow(Final_Results)-1,1)
  Final_Results = data.frame(DATE, party_names[party_index], Final_Results)
  colnames(Final_Results) = c('date','party','approve_rate','approve_lower','approve_upper') ## 민주당 지지율 시계열
  global_result <- rbind(global_result, Final_Results)
  print(sprintf("total %d rows added", nrow(Final_Results)))
  return(global_result)
}
