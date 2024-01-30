###############################
## NBA spread predictor
## Lachlan Bubb
## 09/01/2021
###############################

rm(list = ls()); gc();

# setwd("~/nbaSpreads/")
source("99_functions.R")

# Accuracy   <- 1 - error_rate
error_rate <- 0.48
odds       <- 110
returnRate <- oddsToDecimal(odds)

initialFund <- 0
betSize     <- 10
days        <- 10
numRuns     <- 100

outcome <- 
  data.table(day = seq_len(days)) %>%
  .[rep(seq_len(days), numRuns)]  %>%
  .[, run := 1:.N, by = day]      %>% 
  .[, rand := runif(.N)]          %>%
  .[, win := rand <= (1 - error_rate)]  %>% 
  .[, bet := betSize] %>%
  .[, return := fifelse(win, bet * returnRate, -bet)] %>%
  .[, bank := initialFund + cumsum(return), by = run]

ggplot(outcome) +
  geom_line(aes(x = day, y = bank, col = as.factor(run))) +
  geom_smooth(aes(x = day, y = bank), method = "lm")

summary(outcome[day == days, bank]) %>% print
outcome[day == days][, .N, keyby = bank][, .(bank, N, N/sum(N))]
outcome[day == days][, hist(bank)]

outcome <- 
  data.table(day = seq_len(days)) %>%
  .[rep(seq_len(days), numRuns)]  %>%
  .[, run := 1:.N, by = day]      %>% 
  .[, rand := runif(.N)]          %>%
  .[, win := rand >= error_rate]  %>% 
  .[, bet := fifelse(shift(win, 1, "lag", fill = FALSE), 2 * betSize, betSize)] %>%
  .[, return := fifelse(win, bet * returnRate, -bet)] %>%
  .[, bank := initialFund + cumsum(return), by = run]

ggplot(outcome) + 
  geom_line(aes(x = day, y = bank, col = as.factor(run))) + 
  geom_smooth(aes(x = day, y = bank), method = "lm")

summary(outcome[day == days, bank]) %>% print
