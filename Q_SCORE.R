library(tidyverse)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(openxlsx)

F_score <- read.csv("F_SCORE.csv")
Q_score <- read.csv("Q_SCORE.csv")

F_score$X <- gsub("-US","", as.character(F_score$X))
Q_score$X <- gsub("-US","", as.character(Q_score$X))

# 8 & 9 

F_tickers <- F_score %>% filter(F_SCORE >= 8) %>% select(X)
Q_tickers <- Q_score %>% filter(QSCORE >= 8) %>% select(X)

f_env <- new.env()
q_env <- new.env()

getSymbols(Symbols = F_tickers$X, env = f_env, from = '2003-01-01')

getSymbols(Symbols = Q_tickers$X, env = q_env, from = '2003-01-01')





F_Port <- do.call(merge, eapply(f_env, function(x){
  x <- Ad(x)
  w <- Cl(to.monthly(x))
  colnames(w) <- colnames(x)
  w
}))

Q_Port <- do.call(merge, eapply(q_env, function(x){
  x <- Ad(x)
  w <- Cl(to.monthly(x))
  colnames(w) <- colnames(x)
  w
}))

F_Port <- F_Port[ , colSums(is.na(F_Port)) == 0]
F_Port_Return <- ROC(F_Port)
F_wealth_index <- Return.portfolio(F_Port_Return, wealth.index = TRUE)

Q_Port <- Q_Port[ , colSums(is.na(Q_Port)) == 0]
Q_Port_Return <- ROC(Q_Port)
Q_wealth_index <- Return.portfolio(Q_Port_Return, wealth.index = TRUE)

SharpeRatio.annualized(Return.portfolio(F_Port_Return))
SharpeRatio.annualized(Return.portfolio(Q_Port_Return))

Return.annualized(Return.portfolio(F_Port_Return))
Return.annualized(Return.portfolio(Q_Port_Return))

SortinoRatio(Return.portfolio(F_Port_Return))
SortinoRatio(Return.portfolio(Q_Port_Return))

sd(Return.portfolio(F_Port_Return))*sqrt(12)
sd(Return.portfolio(Q_Port_Return))*sqrt(12)

maxDrawdown(Return.portfolio(F_Port_Return))
maxDrawdown(Return.portfolio(Q_Port_Return))

# 7,8,9 only

F9_tickers <- F_score %>% filter(F_SCORE >= 7) %>% select(X)
Q9_tickers <- Q_score %>% filter(QSCORE >= 7) %>% select(X)

f9_env <- new.env()
q9_env <- new.env()

getSymbols(Symbols = F9_tickers$X, env = f9_env, from = '2003-01-01')

getSymbols(Symbols = Q9_tickers$X, env = q9_env, from = '2003-01-01')

ncol(Q9_Port_Return)



F9_Port <- do.call(merge, eapply(f9_env, function(x){
  x <- Ad(x)
  w <- Cl(to.monthly(x))
  colnames(w) <- colnames(x)
  w
}))

Q9_Port <- do.call(merge, eapply(q9_env, function(x){
  x <- Ad(x)
  w <- Cl(to.monthly(x))
  colnames(w) <- colnames(x)
  w
}))

F9_Port <- F9_Port[ , colSums(is.na(F9_Port)) == 0]
F9_Port_Return <- ROC(F9_Port)
F9_wealth_index <- Return.portfolio(F9_Port_Return, wealth.index = TRUE)

Q9_Port <- Q9_Port[ , colSums(is.na(Q9_Port)) == 0]
Q9_Port_Return <- ROC(Q9_Port)
Q9_wealth_index <- Return.portfolio(Q9_Port_Return, wealth.index = TRUE)

SharpeRatio.annualized(Return.portfolio(F9_Port_Return))
SharpeRatio.annualized(Return.portfolio(Q9_Port_Return))

Return.annualized(Return.portfolio(F9_Port_Return))
Return.annualized(Return.portfolio(Q9_Port_Return))

SortinoRatio(Return.portfolio(F9_Port_Return))
SortinoRatio(Return.portfolio(Q9_Port_Return))

sd(Return.portfolio(F9_Port_Return))*sqrt(12)
sd(Return.portfolio(Q9_Port_Return))*sqrt(12)

maxDrawdown(Return.portfolio(F9_Port_Return))
maxDrawdown(Return.portfolio(Q9_Port_Return))

F_drawdowns <- Drawdowns(Return.portfolio(F_Port_Return))
Q_drawdowns <- Drawdowns(Return.portfolio(Q_Port_Return))

F9_drawdowns <- Drawdowns(Return.portfolio(F9_Port_Return))
Q9_drawdowns <- Drawdowns(Return.portfolio(Q9_Port_Return))

xl_list <- list("tmp1" = F_wealth_index, "tmp2" = Q_wealth_index, 
                "tmp3" = F9_wealth_index, "tmp4" = Q9_wealth_index, 
                "tmp5" = F_drawdowns, "tmp6" = Q_drawdowns, 
                "tmp7" = F9_drawdowns, "tmp8" = Q9_drawdowns)

#write.xlsx(xl_list, file = "port_perf.xlsx")

# Factors
aqr_factors <- read.xlsx("AQR_QMJ.xlsx")
