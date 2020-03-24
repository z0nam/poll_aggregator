library(tidyverse)
library(scales)
library(pscl)
library(forcats)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- read.table(file="_data/jongro_test.txt", sep="\t", header = T)
colnames(data) = c('startdate','durringdate','enddate','area1','area2','area3','SAE','MIN','PEOPLE','JUS','UNK1','UNK2','UNK3','house','press','method','ARS','N','rate','weight','age')

head(data)


START_DATE <- "2016-03-15"
END_DATE <- "2016-04-20"

MU_START <- 35
MU_FINISH <- 52.6


days_between_elections <- as.integer(diff(as.Date(c(START_DATE, END_DATE))))  + 1

d1 <- list(mu_start = MU_START, mu_finish = MU_FINISH, n_days = days_between_elections)  # mu_start: 대략 시작시점의 민주당선호율, mu_finish: 최종 민주당 득표율

# plot_results 는 공개된 버젼을 그대로 사용

plot_results <- function(stan_m){
  if(class(stan_m) != "stanfit"){
    stop("stan_m must be an object of class stanfit, with parameters mu representing latent vote at a point in time")}
  ex <- as.data.frame(rstan::extract(stan_m, "mu"))
  names(ex) <- 1:d1$n_days
  
  p <- ex %>%
    gather(day, value) %>%
    mutate(day = as.numeric(day),
           day = as.Date(day, origin = START_DATE)) %>%
    group_by(day) %>%
    summarise(mean = mean(value),
              upper = quantile(value, 0.975),
              lower = quantile(value, 0.025)) %>%
    ggplot(aes(x = day)) +
    labs(x = "Shaded region shows a pointwise 95% credible interval.",
         y = "Voting intention for the ALP(%)",
         caption = "Source: Jackman's pscl R package") + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) + 
    scale_y_continuous(breaks = 30:60, sec.axis = dup_axis(name="")) + # 30:60은 보여줄 그래프의 y하한 과 y 상한임. 
    theme(panel.grid.minor = element_blank())
  
  return(p)
}



ac <- data %>%
  mutate(MidDate = startdate + (enddate-startdate)/2, 
         MidDateNum = as.integer(MidDate - as.Date(START_DATE)),
         p = MIN/100,
         se_MIN = sqrt(p*(1-p)/N)*100)

d2 <- list(
  mu_start = MU_START,
  mu_finish = MU_FINISH,
  n_days = days_between_elections,
  y_values = ac$MIN,
  y_days = ac$MidDateNum,
  y_n = nrow(ac),
  y_se = ac$se_MIN
)

system.time({
  stan_mod2 <- stan(file = 'oz-polls-2.stan', data = d2,
                    control = list(max_treedepth = 20))
}) # 510 seconds// me: 473

plot_results(stan_mod2) +
  geom_point(data = ac, aes(x = MidDate, y = ALP)) +
  ggtitle("Voting intention for the ALP between the 2004 and 2007 Australian elections",
          "Latent variable estimated with use of just one firm's polling data (Nielsen)")

