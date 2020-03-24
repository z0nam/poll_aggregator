
# plot_results 는 공개된 버젼을 그대로 사용

# plot_results <- function(stan_m){
#   if(class(stan_m) != "stanfit"){
#     stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")}
#   ex <- as.data.frame(rstan::extract(stan_m, "mu"))
#   #names(ex) <- 1:d1_SAE$n_days
#   names(ex) <- 1:d1$n_days
#   
#   p <- ex %>%
#     gather(day, value) %>%
#     mutate(day = as.numeric(day),
#            day = as.Date(day, origin = START_DATE)) %>%
#     group_by(day) %>%
#     summarise(mean = mean(value),
#               upper = quantile(value, 1-(1-SIGNIFICANT_LEVEL)/2),
#               lower = quantile(value, (1-SIGNIFICANT_LEVEL)/2)) %>%
#     ggplot(aes(x = day)) +
#     labs(x = sprintf("Shaded region shows a pointwise %d credible interval.", SIGNIFICANT_LEVEL*100),
#          y = "Voting intention for the Party(%)",
#          caption = "Source: Jackman's pscl R package") + 
#     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#     geom_line(aes(y = mean)) + 
#     scale_y_continuous(breaks = Y_MIN:Y_MAX, sec.axis = dup_axis(name="")) + # 30:60은 보여줄 그래프의 y하한 과 y 상한임. 
#     theme(panel.grid.minor = element_blank())
#   
#   return(p)
# } 


# to make function

calculate_ci_by_party <- function(party){
  print(sprintf("calculate_ci_by_party: %s", party))
  #arg0: party is string for PartyList Key (i.e., "SAE", "MIN")
  
  # party_key <- paste0(party, "_raw") # raw를 쓸지 말지 고민.
  party_key <- party
  
  MU_START <- localdata %>% group_by(enddate) %>% summarise(a = mean(eval(parse(text=paste0(party_key)))))
  MU_START <- MU_START$a[1] # 초기값
  MU_FINISH = localdata %>% group_by(enddate) %>% summarise(a = mean(eval(parse(text=paste0(party_key)))))
  MU_FINISH <- MU_FINISH$a[length(MU_FINISH$a)] # 마지막값

# 
#   d1 <- list(
#     mu_start = MU_START, 
#     mu_finish = MU_FINISH, 
#     n_days = days_between_elections
#   )   

  # ac_SAE 대신임
  partydata <- localdata %>%
    mutate(MidDate = startdate + (enddate-startdate)/2, 
           MidDateNum = as.integer(MidDate - START_DATE),
           p = eval(parse(text=paste0(party_key)))/100,
           se = sqrt(p*(1-p)/N)*100)

  d2 <- list(
    mu_start = MU_START,
    mu_finish = MU_FINISH,
    n_days = days_between_elections,
    y_values = eval(parse(text=paste0("partydata$", party_key))),
    y_days = eval(parse(text=paste0("partydata$MidDateNum"))),
    y_n = nrow(localdata),
    y_se = eval(parse(text=paste0("partydata$se")))
  )

  tryCatch({
    print(sprintf("stan_mod2 for %s: START", party))
    stan_mod2 <- stan(file = 'oz-polls-2_sigma3.stan', data = d2,
                      control = list(max_treedepth = 20))
    print(sprintf("stan_mod2 for %s: END, dim: %d", party, length(dim(stan_mod2))))
    },
    warning=function(e){
      print(sprintf("warning --> omit this party %s", party))
      return(998)
    },
    error=function(e){
      print(sprintf("error --> omit this party %s", party))
      return(999)
    }
  )

  DATE = seq(START_DATE,END_DATE,1)
  DATE = DATE[-length(DATE)]
  
  if(!(exists("stan_mod2"))){
    stan_mod2 <- 999
  }
  if (length(dim(stan_mod2)>0)){
    results = summary(stan_mod2)
    chains = dim(results$c_summary)[3]
    results_1 = results$c_summary[,,chains]
    vics = matrix(nrow=100,ncol=length(results_1[,1])-2)
    posterior = matrix(nrow=length(results_1[,1])-2,ncol=10000)
    for(i in 1:nrow(posterior)){
      posterior[i,] = rnorm(10000,results_1[i,1],results_1[i,2])}
  }else{
    print(sprintf("There is no party (%s): return", party))
    return(total_result)
  }
  
  # df에 결과 정리하기
  
  Final_Results = cbind(rowMeans(posterior),apply(posterior,1,min),apply(posterior,1,max))
  print(results_1[,1:2])
  results_1 <- head(results_1, -2)
  # results_1 <- results_1 %>% select("mean", "sd")
  Final_Results = data.frame(DATE,region,party=PartyList[party],Final_Results,results_1[,1:2])
  colnames(Final_Results) <- c(
    "date",
    "region",
    "party",
    "mean_rate",
    "lower_rate",
    "upper_rate",
    "mu",
    "sd"
  )
  
  # 원데이터에 저장하기
  print(sprintf("%d rows added to total_result", nrow(Final_Results)))
  total_result <- rbind(total_result, Final_Results)
  print(sprintf("now total number of rows in total_result: %d", nrow(total_result)))
  return(total_result)
}


calculate_probability <- function(){
  # 현재의 지역과 파티가 정의되어 있는지 체크
  # 유효하면 진행, 아니면 return 
  print(sprintf("calculate_probability: start!"))
  region_to_search <- region
  print(sprintf("region to search: %s", region))
  localdata <- total_result %>% filter(region==region_to_search)
  local_party_list = unique(localdata$party)
  print(sprintf("Total num of parties: %d", length(local_party_list)))
  print(local_party_list)
  days = seq(START_DATE, END_DATE-1, by="day")
  for( day in seq_along(days)){
    # posteria <- rnorm(10000, tmp_data["mu"], tmp_data["sd"])
    print(paste("Date:", days[day]))
    tmp_data <- localdata %>% filter(date == days[day])
    posteria <- c()
    num_win <- c()
    print("tmp_data get")
    for(p in local_party_list){
      print(sprintf("for party %s:",p))
      single_row <- tmp_data %>% filter(party==p)
      mu <- as.numeric(single_row["mu"])
      sd <- as.numeric(single_row["sd"])
      posteria <- rbind(posteria, rnorm(10000, mu, sd))
      num_win <- cbind(num_win, 0)
    }
    print("done.")
    for (i in 1:10000){
      num_win[which(posteria[,i] == max(posteria[,i]))] <- num_win[which(posteria[,i] == max(posteria[,i]))] + 1
    }
    for(p_index in 1:length(local_party_list)){
      new_row = data.frame(days[day], region, local_party_list[p_index], num_win[p_index]/10000)
      colnames(new_row)=c("date", "region", "party", "win_prob")
      total_prob <- rbind(total_prob, new_row)
      print(sprintf("new row added. total size of total_prob:%d", nrow(total_prob)))
    }
  }
  return(total_prob)
}
