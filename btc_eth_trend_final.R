library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(lubridate)
library(xts)
library(TTR)
library(knitr)

url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/btc_eth_price_history.csv'
price_history <- read.csv(url)

#----------------------------------------------------------------------------------------------------------
#convert dates to dttm
price_history$timestamp <- ymd_hms(price_history$timestamp)
price_history$time_close <- ymd_hms(price_history$time_close)
price_history$time_high <- ymd_hms(price_history$time_high)
price_history$time_low <- ymd_hms(price_history$time_low)

price_history$time_open <- ymd(price_history$time_open)

btc_data <- price_history |> filter(name=='Bitcoin')

#remove duplicate rows
btc_prices <- btc_data |> select(timestamp,open,high,low,close,volume) |> distinct() |> arrange(timestamp)

btc_prices |>
  distinct() |>
  nrow()

nrow(btc_prices)

btc_prices |>
  count(timestamp) |>
  filter(n > 1)

#convert to xts
btc_xts <- xts(btc_prices[,-1], order.by = btc_prices$timestamp)


#add SMA
btc_xts$sma_1w <- SMA(Cl(btc_xts),n=7)
btc_xts$sma_1m <- SMA(Cl(btc_xts),n=30)
btc_xts$sma_3m <- SMA(Cl(btc_xts),n=90)
btc_xts$sma_6m <- SMA(Cl(btc_xts),n=180)
btc_xts$sma_1y <- SMA(Cl(btc_xts),n=365)

#add EMA
btc_xts$ema_1w <- EMA(Cl(btc_xts),n=7)
btc_xts$ema_1m <- EMA(Cl(btc_xts),n=30)
btc_xts$ema_3m <- EMA(Cl(btc_xts),n=90)
btc_xts$ema_6m <- EMA(Cl(btc_xts),n=180)
btc_xts$ema_1y <- EMA(Cl(btc_xts),n=365)

#macd
btc_xts$macd <- MACD(Cl(btc_xts))[,1]
btc_xts$macd_signal <- MACD(Cl(btc_xts))[,2]


smas <- c(7,30,90,180,365)

t_cost <- 0.001

#performance_data_frame
performance_df <- data.frame(
  strategy = character(),
  start_date = POSIXct(),
  end_date = POSIXct(),
  ann_return = double(),
  ann_vol = double(),
  sharpe = double(),
  max_dd = double()
)


#----------------------------------------------------------------------------------------------------------
#Price - SMAs

smas_long_short_close <- btc_xts[,c('close','sma_1w','sma_1m','sma_3m','sma_6m','sma_1y')]
smas_long_short_close$btc_return <- dailyReturn(smas_long_short_close$close,type = 'arithmetic')

smas_signals_list_close <- list()
smas_position_list_close <- list()
smas_gross_list_close <- list()
smas_turnover_list_close <- list()
smas_net_list_close <- list()

#calculate signals, positions, gross returns, turnover, and net returns
#for each SMA strategy

for (i in seq_along(smas)) {
  
      
      key <- paste0('close', "_", smas[i])
      
      signal_vals <- ifelse(
        coredata(smas_long_short_close[, 1]) > coredata(smas_long_short_close[, i + 1]),
        1, -1
      )
      
      signal <- xts::xts(signal_vals, order.by = zoo::index(smas_long_short_close))
      colnames(signal) <- paste0("signal", '_close', "_", smas[i])
      
      position <- xts::lag.xts(signal, k = 1)
      colnames(position) <- paste0("position", '_close', "_", smas[i])
      
      gross <- smas_long_short_close$btc_return * position
      gross <- xts::xts(coredata(gross), order.by = zoo::index(smas_long_short_close))
      colnames(gross) <- paste0("gross", '_close', "_", smas[i])
      
      turnover <- abs(position - xts::lag.xts(position, k = 1))
      turnover <- xts::xts(coredata(turnover), order.by = zoo::index(smas_long_short_close))
      turnover[is.na(turnover)] <- 0
      colnames(turnover) <- paste0("turnover", '_close', "_", smas[i])
      
      net <- gross - turnover * t_cost
      net <- xts::xts(coredata(net), order.by = zoo::index(smas_long_short_close))
      colnames(net) <- paste0("net", "close", "_", smas[i])
      
      smas_signals_list_close[[key]] <- signal
      smas_position_list_close[[key]] <- position
      smas_gross_list_close[[key]]    <- gross
      smas_turnover_list_close[[key]] <- turnover
      smas_net_list_close[[key]]      <- net

}


returns_smas_long_short_close <- xts::merge.xts(
  smas_long_short_close[, "btc_return"],
  do.call(xts::merge.xts, smas_gross_list_close),
  do.call(xts::merge.xts, smas_net_list_close)
)

start_point <- min(which(complete.cases(returns_smas_long_short_close[, c(1:ncol(returns_smas_long_short_close))])))
end_point <- nrow(returns_smas_long_short_close)

charts.PerformanceSummary(returns_smas_long_short_close[start_point:end_point,])
table.AnnualizedReturns(returns_smas_long_short_close[start_point:end_point,])
maxDrawdown(returns_smas_long_short_close[start_point:end_point,])
SharpeRatio.annualized(returns_smas_long_short_close[start_point:end_point,])

#net only

charts.PerformanceSummary(returns_smas_long_short_close[start_point:end_point,c(1,7:11)])
table.AnnualizedReturns(returns_smas_long_short_close[start_point:end_point,c(1,7:11)])
maxDrawdown(returns_smas_long_short_close[start_point:end_point,c(1,7:11)])
SharpeRatio.annualized(returns_smas_long_short_close[start_point:end_point,c(1,7:11)])




#bitcoin performance
performance_df <- performance_df |> add_row(strategy = 'bitcoin',
                                            start_date = index(returns_smas_long_short_close)[start_point],
                                            end_date = index(returns_smas_long_short_close)[end_point],
                                            ann_return = table.AnnualizedReturns(returns_smas_long_short_close[,1])[1,1],
                                            ann_vol = table.AnnualizedReturns(returns_smas_long_short_close[,1])[2,1],
                                            sharpe = table.AnnualizedReturns(returns_smas_long_short_close[,1])[3,1],
                                            max_dd = maxDrawdown(returns_smas_long_short_close[,1]))

for (k in 7:11){
  performance_df <- performance_df |> add_row(strategy = paste('sma_', colnames(returns_smas_long_short_close)[k],sep = ''),
                                              start_date = index(returns_smas_long_short_close)[start_point],
                                              end_date = index(returns_smas_long_short_close)[end_point],
                                              ann_return = table.AnnualizedReturns(returns_smas_long_short_close[,k])[1,1],
                                              ann_vol = table.AnnualizedReturns(returns_smas_long_short_close[,k])[2,1],
                                              sharpe = table.AnnualizedReturns(returns_smas_long_short_close[,k])[3,1],
                                              max_dd = maxDrawdown(returns_smas_long_short_close[,k]))
}

#----------------------------------------------------------------------------------------------------------
#equal weighted SMAs signals
smas_equal_weight <- xts::merge.xts(
  smas_long_short_close[, "btc_return"],
  do.call(xts::merge.xts, smas_position_list_close))

smas_equal_weight$agg_position <- rowMeans(smas_equal_weight[,c(2:6)])
smas_equal_weight$gross <- smas_equal_weight$btc_return * smas_equal_weight$agg_position

smas_equal_weight$turnover <- abs(smas_equal_weight$agg_position - xts::lag.xts(smas_equal_weight$agg_position, k =1))


smas_equal_weight$net <- smas_equal_weight$gross - smas_equal_weight$turnover * t_cost

start_point_equal_weight <- min(which(complete.cases(smas_equal_weight[, c(1,8,10)])))
end_point_equal_weight <- nrow(smas_equal_weight)

charts.PerformanceSummary(smas_equal_weight[start_point_equal_weight:end_point_equal_weight,c(1,8,10)])
table.AnnualizedReturns(smas_equal_weight[start_point_equal_weight:end_point_equal_weight,c(1,8,10)])
maxDrawdown(smas_equal_weight[start_point_equal_weight:end_point_equal_weight,c(1,8,10)])
SharpeRatio.annualized(smas_equal_weight[start_point_equal_weight:end_point_equal_weight,c(1,8,10)])

performance_df <- performance_df |> add_row(strategy = 'smas_equal_weight_gross',
                                            start_date = index(smas_equal_weight)[start_point_equal_weight],
                                            end_date = index(smas_equal_weight)[end_point_equal_weight],
                                            ann_return = table.AnnualizedReturns(smas_equal_weight[,8])[1,1],
                                            ann_vol = table.AnnualizedReturns(smas_equal_weight[,8])[2,1],
                                            sharpe = table.AnnualizedReturns(smas_equal_weight[,8])[3,1],
                                            max_dd = maxDrawdown(smas_equal_weight[,8]))

performance_df <- performance_df |> add_row(strategy = 'smas_equal_weight_net',
                                            start_date = index(smas_equal_weight)[start_point_equal_weight],
                                            end_date = index(smas_equal_weight)[end_point_equal_weight],
                                            ann_return = table.AnnualizedReturns(smas_equal_weight[,10])[1,1],
                                            ann_vol = table.AnnualizedReturns(smas_equal_weight[,10])[2,1],
                                            sharpe = table.AnnualizedReturns(smas_equal_weight[,10])[3,1],
                                            max_dd = maxDrawdown(smas_equal_weight[,10]))

#----------------------------------------------------------------------------------------------------------
#inverse vol weighted SMAs
roll_period <- 60

smas_inverse_vol <- smas_equal_weight[,c(1:6)]

#calc return stream of each SMA signal
smas_inverse_vol$return_7 <- smas_inverse_vol$position_close_7 * smas_inverse_vol$btc_return
smas_inverse_vol$return_30 <- smas_inverse_vol$position_close_30 * smas_inverse_vol$btc_return
smas_inverse_vol$return_90 <- smas_inverse_vol$position_close_90 * smas_inverse_vol$btc_return
smas_inverse_vol$return_180 <- smas_inverse_vol$position_close_180 * smas_inverse_vol$btc_return
smas_inverse_vol$return_365 <- smas_inverse_vol$position_close_365 * smas_inverse_vol$btc_return

#calc rolling vol of each return stream
#7 day
smas_inverse_vol$vol_7 <- rollapply(smas_inverse_vol$return_7, width = roll_period,
                                    FUN = sd, align = 'right', fill=NA)

smas_inverse_vol$vol_7 <- lag(smas_inverse_vol$vol_7, 1)

#30 day
smas_inverse_vol$vol_30 <- rollapply(smas_inverse_vol$return_30, width = roll_period,
                                    FUN = sd, align = 'right', fill=NA)

smas_inverse_vol$vol_30 <- lag(smas_inverse_vol$vol_30, 1)

#90 day
smas_inverse_vol$vol_90 <- rollapply(smas_inverse_vol$return_90, width = roll_period,
                                     FUN = sd, align = 'right', fill=NA)

smas_inverse_vol$vol_90 <- lag(smas_inverse_vol$vol_90, 1)

#180 day
smas_inverse_vol$vol_180 <- rollapply(smas_inverse_vol$return_180, width = roll_period,
                                      FUN = sd, align = 'right', fill=NA)

smas_inverse_vol$vol_180 <- lag(smas_inverse_vol$vol_180, 1)

#365 day
smas_inverse_vol$vol_365 <- rollapply(smas_inverse_vol$return_365, width = roll_period,
                                      FUN = sd, align = 'right', fill=NA)

smas_inverse_vol$vol_365 <- lag(smas_inverse_vol$vol_365, 1)

#calculate inverse vol
smas_inverse_vol$inv_vol_7 <- 1/smas_inverse_vol$vol_7
smas_inverse_vol$inv_vol_30 <- 1/smas_inverse_vol$vol_30
smas_inverse_vol$inv_vol_90 <- 1/smas_inverse_vol$vol_90
smas_inverse_vol$inv_vol_180 <- 1/smas_inverse_vol$vol_180
smas_inverse_vol$inv_vol_365 <- 1/smas_inverse_vol$vol_365

#calculate signal weight
inv_vol_sums_cols <- smas_inverse_vol[,c('inv_vol_7','inv_vol_30','inv_vol_90',
                                         'inv_vol_180','inv_vol_365')]

sum_inv_vols <- rowSums(inv_vol_sums_cols,na.rm = F)


smas_inverse_vol$weight_7 <- smas_inverse_vol$inv_vol_7/sum_inv_vols
smas_inverse_vol$weight_30 <- smas_inverse_vol$inv_vol_30/sum_inv_vols
smas_inverse_vol$weight_90 <- smas_inverse_vol$inv_vol_90/sum_inv_vols
smas_inverse_vol$weight_180 <- smas_inverse_vol$inv_vol_180/sum_inv_vols
smas_inverse_vol$weight_365 <- smas_inverse_vol$inv_vol_365/sum_inv_vols

#aggregated position
smas_inverse_vol$agg_position <- smas_inverse_vol$position_close_7 * smas_inverse_vol$weight_7 + 
  smas_inverse_vol$position_close_30 * smas_inverse_vol$weight_30 +
  smas_inverse_vol$position_close_90 * smas_inverse_vol$weight_90 +
  smas_inverse_vol$position_close_180 * smas_inverse_vol$weight_180 +
  smas_inverse_vol$position_close_365 * smas_inverse_vol$weight_365

#gross return
smas_inverse_vol$gross <- smas_inverse_vol$btc_return * smas_inverse_vol$agg_position

#turnover
smas_inverse_vol$turnover <- abs(smas_inverse_vol$agg_position - xts::lag.xts(smas_inverse_vol$agg_position, k =1))

#net
smas_inverse_vol$net <- smas_inverse_vol$gross - smas_inverse_vol$turnover * t_cost

start_point_inv_vol <- min(which(complete.cases(smas_inverse_vol[, c(1,28,30)])))
end_point_inv_vol<- nrow(smas_inverse_vol)

charts.PerformanceSummary(smas_inverse_vol[start_point_inv_vol:end_point_inv_vol,c(1,28,30)])
table.AnnualizedReturns(smas_inverse_vol[start_point_inv_vol:end_point_inv_vol,c(1,28,30)])
maxDrawdown(smas_inverse_vol[start_point_inv_vol:end_point_inv_vol,c(1,28,30)])
SharpeRatio.annualized(smas_inverse_vol[start_point_inv_vol:end_point_inv_vol,c(1,28,30)])


performance_df <- performance_df |> add_row(strategy = 'smas_inv_vol_gross',
                                            start_date = index(smas_inverse_vol)[start_point_inv_vol],
                                            end_date = index(smas_inverse_vol)[end_point_inv_vol],
                                            ann_return = table.AnnualizedReturns(smas_inverse_vol[,28])[1,1],
                                            ann_vol = table.AnnualizedReturns(smas_inverse_vol[,28])[2,1],
                                            sharpe = table.AnnualizedReturns(smas_inverse_vol[,28])[3,1],
                                            max_dd = maxDrawdown(smas_inverse_vol[,28]))

performance_df <- performance_df |> add_row(strategy = 'smas_inv_vol_net',
                                            start_date = index(smas_inverse_vol)[start_point_inv_vol],
                                            end_date = index(smas_inverse_vol)[end_point_inv_vol],
                                            ann_return = table.AnnualizedReturns(smas_inverse_vol[,30])[1,1],
                                            ann_vol = table.AnnualizedReturns(smas_inverse_vol[,30])[2,1],
                                            sharpe = table.AnnualizedReturns(smas_inverse_vol[,30])[3,1],
                                            max_dd = maxDrawdown(smas_inverse_vol[,30]))




#----------------------------------------------------------------------------------------------------------
#Price - EMAs
emas <- c(7,30,90,180,365)

emas_long_short_close <- btc_xts[,c('close','ema_1w','ema_1m','ema_3m','ema_6m','ema_1y')]
emas_long_short_close$btc_return <- dailyReturn(emas_long_short_close$close,type = 'arithmetic')

emas_signals_list_close <- list()
emas_position_list_close <- list()
emas_gross_list_close <- list()
emas_turnover_list_close <- list()
emas_net_list_close <- list()


#calculate signals, positions, gross returns, turnover, and net returns
#for each EMA strategy

for (i in seq_along(emas)) {
  
  
  key <- paste0('close', "_", emas[i])
  
  signal_vals_ema <- ifelse(
    coredata(emas_long_short_close[, 1]) > coredata(emas_long_short_close[, i + 1]),
    1, -1
  )
  
  signal_ema <- xts::xts(signal_vals_ema, order.by = zoo::index(emas_long_short_close))
  colnames(signal_ema) <- paste0("signal_ema", '_close', "_", emas[i])
  
  position_ema <- xts::lag.xts(signal_ema, k = 1)
  colnames(position_ema) <- paste0("position_ema", '_close', "_", emas[i])
  
  gross_ema <- emas_long_short_close$btc_return * position_ema
  gross_ema <- xts::xts(coredata(gross_ema), order.by = zoo::index(emas_long_short_close))
  colnames(gross_ema) <- paste0("gross_ema", '_close', "_", emas[i])
  
  turnover_ema <- abs(position_ema - xts::lag.xts(position_ema, k = 1))
  turnover_ema <- xts::xts(coredata(turnover_ema), order.by = zoo::index(emas_long_short_close))
  turnover_ema[is.na(turnover_ema)] <- 0
  colnames(turnover_ema) <- paste0("turnover_ema", '_close', "_", emas[i])
  
  net_ema <- gross_ema - turnover_ema * t_cost
  net_ema <- xts::xts(coredata(net_ema), order.by = zoo::index(emas_long_short_close))
  colnames(net_ema) <- paste0("net_ema", "close", "_", emas[i])
  
  emas_signals_list_close[[key]] <- signal_ema
  emas_position_list_close[[key]] <- position_ema
  emas_gross_list_close[[key]]    <- gross_ema
  emas_turnover_list_close[[key]] <- turnover_ema
  emas_net_list_close[[key]]      <- net_ema
  
}


returns_emas_long_short_close <- xts::merge.xts(
  emas_long_short_close[, "btc_return"],
  do.call(xts::merge.xts, emas_gross_list_close),
  do.call(xts::merge.xts, emas_net_list_close)
)

start_point <- min(which(complete.cases(returns_emas_long_short_close[, c(1:ncol(returns_emas_long_short_close))])))
end_point <- nrow(returns_emas_long_short_close)

charts.PerformanceSummary(returns_emas_long_short_close[start_point:end_point,])
table.AnnualizedReturns(returns_emas_long_short_close[start_point:end_point,])
maxDrawdown(returns_emas_long_short_close[start_point:end_point,])
SharpeRatio.annualized(returns_emas_long_short_close[start_point:end_point,])

#net only

charts.PerformanceSummary(returns_emas_long_short_close[start_point:end_point,c(1,7:11)])
table.AnnualizedReturns(returns_emas_long_short_close[start_point:end_point,c(1,7:11)])
maxDrawdown(returns_emas_long_short_close[start_point:end_point,c(1,7:11)])
SharpeRatio.annualized(returns_emas_long_short_close[start_point:end_point,c(1,7:11)])

for (k in 7:11){
  performance_df <- performance_df |> add_row(strategy = paste('sma_', colnames(returns_emas_long_short_close)[k],sep = ''),
                                              start_date = index(returns_emas_long_short_close)[start_point],
                                              end_date = index(returns_emas_long_short_close)[end_point],
                                              ann_return = table.AnnualizedReturns(returns_emas_long_short_close[,k])[1,1],
                                              ann_vol = table.AnnualizedReturns(returns_emas_long_short_close[,k])[2,1],
                                              sharpe = table.AnnualizedReturns(returns_emas_long_short_close[,k])[3,1],
                                              max_dd = maxDrawdown(returns_emas_long_short_close[,k]))
}


#----------------------------------------------------------------------------------------------------------
#equal weighted EMAs signals
emas_equal_weight <- xts::merge.xts(
  emas_long_short_close[, "btc_return"],
  do.call(xts::merge.xts, emas_position_list_close))

emas_equal_weight$agg_position <- rowMeans(emas_equal_weight[,c(2:6)])
emas_equal_weight$gross <- emas_equal_weight$btc_return * emas_equal_weight$agg_position

emas_equal_weight$turnover <- abs(emas_equal_weight$agg_position - xts::lag.xts(emas_equal_weight$agg_position, k =1))
#turnover <- abs(position - xts::lag.xts(position, k = 1))
#smas_equal_weight$turnover <- smas_equal_weight$turnover[is.na(turnover)] <- 0

emas_equal_weight$net <- emas_equal_weight$gross - emas_equal_weight$turnover * t_cost

start_point_equal_weight_ema <- min(which(complete.cases(emas_equal_weight[, c(1,8,10)])))
end_point_equal_weight_ema <- nrow(emas_equal_weight)

charts.PerformanceSummary(emas_equal_weight[start_point_equal_weight_ema:end_point_equal_weight_ema,c(1,8,10)])
table.AnnualizedReturns(emas_equal_weight[start_point_equal_weight_ema:end_point_equal_weight_ema,c(1,8,10)])
maxDrawdown(emas_equal_weight[start_point_equal_weight_ema:end_point_equal_weight_ema,c(1,8,10)])
SharpeRatio.annualized(emas_equal_weight[start_point_equal_weight_ema:end_point_equal_weight_ema,c(1,8,10)])

performance_df <- performance_df |> add_row(strategy = 'emas_equal_weight_gross',
                                            start_date = index(emas_equal_weight)[start_point_equal_weight_ema],
                                            end_date = index(emas_equal_weight)[end_point_equal_weight_ema],
                                            ann_return = table.AnnualizedReturns(emas_equal_weight[,8])[1,1],
                                            ann_vol = table.AnnualizedReturns(emas_equal_weight[,8])[2,1],
                                            sharpe = table.AnnualizedReturns(emas_equal_weight[,8])[3,1],
                                            max_dd = maxDrawdown(emas_equal_weight[,8]))

performance_df <- performance_df |> add_row(strategy = 'emas_equal_weight_net',
                                            start_date = index(emas_equal_weight)[start_point_equal_weight_ema],
                                            end_date = index(emas_equal_weight)[end_point_equal_weight_ema],
                                            ann_return = table.AnnualizedReturns(emas_equal_weight[,10])[1,1],
                                            ann_vol = table.AnnualizedReturns(emas_equal_weight[,10])[2,1],
                                            sharpe = table.AnnualizedReturns(emas_equal_weight[,10])[3,1],
                                            max_dd = maxDrawdown(emas_equal_weight[,10]))




#----------------------------------------------------------------------------------------------------------
#inverse vol weighted EMAs
roll_period_ema <- 60

emas_inverse_vol <- emas_equal_weight[,c(1:6)]

#calc return stream of each ema signal
emas_inverse_vol$return_7 <- emas_inverse_vol$position_ema_close_7 * emas_inverse_vol$btc_return
emas_inverse_vol$return_30 <- emas_inverse_vol$position_ema_close_30 * emas_inverse_vol$btc_return
emas_inverse_vol$return_90 <- emas_inverse_vol$position_ema_close_90 * emas_inverse_vol$btc_return
emas_inverse_vol$return_180 <- emas_inverse_vol$position_ema_close_180 * emas_inverse_vol$btc_return
emas_inverse_vol$return_365 <- emas_inverse_vol$position_ema_close_365 * emas_inverse_vol$btc_return

#calc rolling vol of each return stream
#7 day
emas_inverse_vol$vol_7 <- rollapply(emas_inverse_vol$return_7, width = roll_period_ema,
                                    FUN = sd, align = 'right', fill=NA)

emas_inverse_vol$vol_7 <- lag(emas_inverse_vol$vol_7, 1)

#30 day
emas_inverse_vol$vol_30 <- rollapply(emas_inverse_vol$return_30, width = roll_period_ema,
                                     FUN = sd, align = 'right', fill=NA)

emas_inverse_vol$vol_30 <- lag(emas_inverse_vol$vol_30, 1)

#90 day
emas_inverse_vol$vol_90 <- rollapply(emas_inverse_vol$return_90, width = roll_period_ema,
                                     FUN = sd, align = 'right', fill=NA)

emas_inverse_vol$vol_90 <- lag(emas_inverse_vol$vol_90, 1)

#180 day
emas_inverse_vol$vol_180 <- rollapply(emas_inverse_vol$return_180, width = roll_period_ema,
                                      FUN = sd, align = 'right', fill=NA)

emas_inverse_vol$vol_180 <- lag(emas_inverse_vol$vol_180, 1)

#365 day
emas_inverse_vol$vol_365 <- rollapply(emas_inverse_vol$return_365, width = roll_period_ema,
                                      FUN = sd, align = 'right', fill=NA)

emas_inverse_vol$vol_365 <- lag(emas_inverse_vol$vol_365, 1)

#calculate inverse vol
emas_inverse_vol$inv_vol_7 <- 1/emas_inverse_vol$vol_7
emas_inverse_vol$inv_vol_30 <- 1/emas_inverse_vol$vol_30
emas_inverse_vol$inv_vol_90 <- 1/emas_inverse_vol$vol_90
emas_inverse_vol$inv_vol_180 <- 1/emas_inverse_vol$vol_180
emas_inverse_vol$inv_vol_365 <- 1/emas_inverse_vol$vol_365

#calculate signal weight
inv_vol_sums_cols_ema <- emas_inverse_vol[,c('inv_vol_7','inv_vol_30','inv_vol_90',
                                         'inv_vol_180','inv_vol_365')]

sum_inv_vols_ema <- rowSums(inv_vol_sums_cols_ema,na.rm = F)


emas_inverse_vol$weight_7 <- emas_inverse_vol$inv_vol_7/sum_inv_vols_ema
emas_inverse_vol$weight_30 <- emas_inverse_vol$inv_vol_30/sum_inv_vols_ema
emas_inverse_vol$weight_90 <- emas_inverse_vol$inv_vol_90/sum_inv_vols_ema
emas_inverse_vol$weight_180 <- emas_inverse_vol$inv_vol_180/sum_inv_vols_ema
emas_inverse_vol$weight_365 <- emas_inverse_vol$inv_vol_365/sum_inv_vols_ema

#aggregated position
emas_inverse_vol$agg_position <- emas_inverse_vol$position_ema_close_7 * emas_inverse_vol$weight_7 + 
  emas_inverse_vol$position_ema_close_30 * emas_inverse_vol$weight_30 +
  emas_inverse_vol$position_ema_close_90 * emas_inverse_vol$weight_90 +
  emas_inverse_vol$position_ema_close_180 * emas_inverse_vol$weight_180 +
  emas_inverse_vol$position_ema_close_365 * emas_inverse_vol$weight_365

#gross return
emas_inverse_vol$gross <- emas_inverse_vol$btc_return * emas_inverse_vol$agg_position

#turnover
emas_inverse_vol$turnover <- abs(emas_inverse_vol$agg_position - xts::lag.xts(emas_inverse_vol$agg_position, k =1))

#net
emas_inverse_vol$net <- emas_inverse_vol$gross - emas_inverse_vol$turnover * t_cost

start_point_inv_vol_ema <- min(which(complete.cases(emas_inverse_vol[, c(1,28,30)])))
end_point_inv_vol_ema <- nrow(emas_inverse_vol)

charts.PerformanceSummary(emas_inverse_vol[start_point_inv_vol_ema:end_point_inv_vol_ema,c(1,28,30)])
table.AnnualizedReturns(emas_inverse_vol[start_point_inv_vol_ema:end_point_inv_vol_ema,c(1,28,30)])
maxDrawdown(emas_inverse_vol[start_point_inv_vol_ema:end_point_inv_vol_ema,c(1,28,30)])
SharpeRatio.annualized(emas_inverse_vol[start_point_inv_vol_ema:end_point_inv_vol_ema,c(1,28,30)])

performance_df <- performance_df |> add_row(strategy = 'emas_inv_vol_gross',
                                            start_date = index(emas_inverse_vol)[start_point_inv_vol_ema],
                                            end_date = index(emas_inverse_vol)[end_point_inv_vol_ema],
                                            ann_return = table.AnnualizedReturns(emas_inverse_vol[,28])[1,1],
                                            ann_vol = table.AnnualizedReturns(emas_inverse_vol[,28])[2,1],
                                            sharpe = table.AnnualizedReturns(emas_inverse_vol[,28])[3,1],
                                            max_dd = maxDrawdown(emas_inverse_vol[,28]))

performance_df <- performance_df |> add_row(strategy = 'emas_inv_vol_net',
                                            start_date = index(emas_inverse_vol)[start_point_inv_vol_ema],
                                            end_date = index(emas_inverse_vol)[end_point_inv_vol_ema],
                                            ann_return = table.AnnualizedReturns(emas_inverse_vol[,30])[1,1],
                                            ann_vol = table.AnnualizedReturns(emas_inverse_vol[,30])[2,1],
                                            sharpe = table.AnnualizedReturns(emas_inverse_vol[,30])[3,1],
                                            max_dd = maxDrawdown(emas_inverse_vol[,30]))

#----------------------------------------------------------------------------------------------------------
#MACD model
macd_close <- btc_xts[,c('close','macd','macd_signal')]
macd_close$btc_return <- dailyReturn(macd_close$close,type = 'arithmetic')


macd_close$macd_signal <- ifelse(macd_close$macd > macd_close$macd_signal, 1, -1)
macd_close$position <- xts::lag.xts(macd_close$macd_signal, k = 1)


macd_close$gross <- macd_close$btc_return * macd_close$position

macd_close$turnover <- abs(macd_close$position - xts::lag.xts(macd_close$position, k =1))


macd_close$net <- macd_close$gross - macd_close$turnover * t_cost

start_point_macd <- min(which(complete.cases(macd_close[, c(4,6,8)])))
end_point_macd <- nrow(macd_close)

charts.PerformanceSummary(macd_close[start_point_macd:end_point_macd,c(4,6,8)])
table.AnnualizedReturns(macd_close[start_point_macd:end_point_macd,c(4,6,8)])
maxDrawdown(macd_close[start_point_macd:end_point_macd,c(4,6,8)])
SharpeRatio.annualized(macd_close[start_point_macd:end_point_macd,c(4,6,8)])

performance_df <- performance_df |> add_row(strategy = 'macd_gross',
                                            start_date = index(macd_close)[start_point_macd],
                                            end_date = index(macd_close)[end_point_macd],
                                            ann_return = table.AnnualizedReturns(macd_close[,6])[1,1],
                                            ann_vol = table.AnnualizedReturns(macd_close[,6])[2,1],
                                            sharpe = table.AnnualizedReturns(macd_close[,6])[3,1],
                                            max_dd = maxDrawdown(macd_close[,6]))

performance_df <- performance_df |> add_row(strategy = 'macd_net',
                                            start_date = index(macd_close)[start_point_macd],
                                            end_date = index(macd_close)[end_point_macd],
                                            ann_return = table.AnnualizedReturns(macd_close[,8])[1,1],
                                            ann_vol = table.AnnualizedReturns(macd_close[,8])[2,1],
                                            sharpe = table.AnnualizedReturns(macd_close[,8])[3,1],
                                            max_dd = maxDrawdown(macd_close[,8]))


#----------------------------------------------------------------------------------------------------------
#combine returns for summary table and analysis
combined_performance_xts <- merge(returns_smas_long_short_close,returns_emas_long_short_close)
combined_performance_xts <- merge(combined_performance_xts, smas_equal_weight[,c(8,10)])
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "gross"] <- "gross_smas_equal_weight"
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "net"] <- "net_smas_equal_weight"

#inverse vol weighted smas
combined_performance_xts <- merge(combined_performance_xts, smas_inverse_vol[,c(28,30)])
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "gross"] <- "gross_smas_inv_vol"
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "net"] <- "net_smas_inv_vol"


#inverse vol weighted emas
combined_performance_xts <- merge(combined_performance_xts, emas_equal_weight[,c(8,10)])
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "gross"] <- "gross_emas_equal_weight"
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "net"] <- "net_emas_equal_weight"


#macd
combined_performance_xts <- merge(combined_performance_xts, macd_close[,c(6,8)])
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "gross"] <- "gross_macd"
colnames(combined_performance_xts)[colnames(combined_performance_xts) == "net"] <- "net_macd"


combined_performance_xts$btc_return.1 <- NULL

combined_performance_xts <- na.omit(combined_performance_xts)

#xts of net returns only
combined_net_performance_xts <- combined_performance_xts[, c(
  "btc_return",
  grep("^net", colnames(combined_performance_xts), value = TRUE)
)]



charts.PerformanceSummary(combined_net_performance_xts)
table.AnnualizedReturns(combined_net_performance_xts)
maxDrawdown(combined_net_performance_xts)
SharpeRatio.annualized(combined_net_performance_xts)

#summary dataframe to be used for report
net_performance_df_btc <- data.frame(
  strategy = character(),
  ann_return = double(),
  ann_vol = double(),
  sharpe = double(),
  max_dd = double()
)

#input each strategy and performance metrics into the dataframe
net_performance_df_btc <- do.call(
  rbind,
  lapply(colnames(combined_net_performance_xts), function(strategy_name) {
    
    ret_xts <- combined_net_performance_xts[, strategy_name, drop = FALSE]
    ret_xts <- na.omit(ret_xts)
    
    ann_tbl <- table.AnnualizedReturns(ret_xts)
    max_dd  <- maxDrawdown(ret_xts)
    
    data.frame(
      strategy   = strategy_name,
      ann_return = as.numeric(ann_tbl["Annualized Return", 1]),
      ann_vol    = as.numeric(ann_tbl["Annualized Std Dev", 1]),
      sharpe     = as.numeric(ann_tbl["Annualized Sharpe (Rf=0%)", 1]),
      max_dd     = as.numeric(max_dd),
      row.names  = NULL
    )
  })
)


net_performance_df_btc


#format for Latex
net_performance_df_btc_latex <- net_performance_df_btc %>%
  mutate(
    strategy   = gsub("_", "\\\\_", strategy),
    ann_return = sprintf("%.1f\\%%", 100 * ann_return),
    ann_vol    = sprintf("%.1f\\%%", 100 * ann_vol),
    sharpe     = sprintf("%.2f", sharpe),
    max_dd     = sprintf("%.1f\\%%", 100 * max_dd)
  )

latex_table <- knitr::kable(
  net_performance_df_btc_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r", "r"),
  col.names = c("Strategy", "Ann. Return", "Ann. Vol", "Sharpe", "Max DD"),
  caption = "Net performance of Bitcoin Trend Following Strategies",
  label = "tab:net_performance",
  escape = FALSE
)

cat(latex_table)

#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#Ethereum trend implementation
eth_data <- price_history |> filter(name=='Ethereum')

#remove duplicate rows
eth_prices <- eth_data |> select(timestamp,open,high,low,close,volume) |> distinct() |> arrange(timestamp)

eth_prices |>
  distinct() |>
  nrow()

nrow(eth_prices)

eth_prices |>
  count(timestamp) |>
  filter(n > 1)

#convert to xts
eth_xts <- xts(eth_prices[,-1], order.by = eth_prices$timestamp)


#add SMA
eth_xts$sma_1w <- SMA(Cl(eth_xts),n=7)
eth_xts$sma_1m <- SMA(Cl(eth_xts),n=30)
eth_xts$sma_3m <- SMA(Cl(eth_xts),n=90)
eth_xts$sma_6m <- SMA(Cl(eth_xts),n=180)
eth_xts$sma_1y <- SMA(Cl(eth_xts),n=365)

#add EMA
eth_xts$ema_1w <- EMA(Cl(eth_xts),n=7)
eth_xts$ema_1m <- EMA(Cl(eth_xts),n=30)
eth_xts$ema_3m <- EMA(Cl(eth_xts),n=90)
eth_xts$ema_6m <- EMA(Cl(eth_xts),n=180)
eth_xts$ema_1y <- EMA(Cl(eth_xts),n=365)

#macd
eth_xts$macd <- MACD(Cl(eth_xts))[,1]
eth_xts$macd_signal <- MACD(Cl(eth_xts))[,2]


eth_smas <- c(7,30,90,180,365)

#t_cost <- 0.001

#performance_data_frame
eth_performance_df <- data.frame(
  strategy = character(),
  start_date = POSIXct(),
  end_date = POSIXct(),
  ann_return = double(),
  ann_vol = double(),
  sharpe = double(),
  max_dd = double()
)


#----------------------------------------------------------------------------------------------------------
#Price - SMAs

eth_smas_long_short_close <- eth_xts[,c('close','sma_1w','sma_1m','sma_3m','sma_6m','sma_1y')]
eth_smas_long_short_close$eth_return <- dailyReturn(eth_smas_long_short_close$close,type = 'arithmetic')

eth_smas_signals_list_close <- list()
eth_smas_position_list_close <- list()
eth_smas_gross_list_close <- list()
eth_smas_turnover_list_close <- list()
eth_smas_net_list_close <- list()

#calculate signals, positions, gross returns, turnover, and net returns
#for each SMA strategy

for (i in seq_along(eth_smas)) {
  
  key <- paste0('close', "_", eth_smas[i])
  
  eth_signal_vals <- ifelse(
    coredata(eth_smas_long_short_close[, 1]) > coredata(eth_smas_long_short_close[, i + 1]),
    1, -1
  )
  
  eth_signal <- xts::xts(eth_signal_vals, order.by = zoo::index(eth_smas_long_short_close))
  colnames(eth_signal) <- paste0("signal", '_close', "_", eth_smas[i])
  
  eth_position <- xts::lag.xts(eth_signal, k = 1)
  colnames(eth_position) <- paste0("position", '_close', "_", eth_smas[i])
  
  eth_gross <- eth_smas_long_short_close$eth_return * eth_position
  eth_gross <- xts::xts(coredata(eth_gross), order.by = zoo::index(eth_smas_long_short_close))
  colnames(eth_gross) <- paste0("gross", '_close', "_", eth_smas[i])
  
  eth_turnover <- abs(eth_position - xts::lag.xts(eth_position, k = 1))
  eth_turnover <- xts::xts(coredata(eth_turnover), order.by = zoo::index(eth_smas_long_short_close))
  eth_turnover[is.na(eth_turnover)] <- 0
  colnames(eth_turnover) <- paste0("turnover", '_close', "_", eth_smas[i])
  
  eth_net <- eth_gross - eth_turnover * t_cost
  eth_net <- xts::xts(coredata(eth_net), order.by = zoo::index(eth_smas_long_short_close))
  colnames(eth_net) <- paste0("net", "close", "_", eth_smas[i])
  
  eth_smas_signals_list_close[[key]] <- eth_signal
  eth_smas_position_list_close[[key]] <- eth_position
  eth_smas_gross_list_close[[key]]    <- eth_gross
  eth_smas_turnover_list_close[[key]] <- eth_turnover
  eth_smas_net_list_close[[key]]      <- eth_net
  
}


eth_returns_smas_long_short_close <- xts::merge.xts(
  eth_smas_long_short_close[, "eth_return"],
  do.call(xts::merge.xts, eth_smas_gross_list_close),
  do.call(xts::merge.xts, eth_smas_net_list_close)
)

eth_start_point <- min(which(complete.cases(eth_returns_smas_long_short_close[, c(1:ncol(eth_returns_smas_long_short_close))])))
eth_end_point <- nrow(eth_returns_smas_long_short_close)

charts.PerformanceSummary(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,])
table.AnnualizedReturns(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,])
maxDrawdown(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,])
SharpeRatio.annualized(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,])

#net only

charts.PerformanceSummary(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,c(1,7:11)])
table.AnnualizedReturns(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,c(1,7:11)])
maxDrawdown(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,c(1,7:11)])
SharpeRatio.annualized(eth_returns_smas_long_short_close[eth_start_point:eth_end_point,c(1,7:11)])


#ethereum performance
eth_performance_df <- eth_performance_df |> add_row(strategy = 'Ethereum',
                                                    start_date = index(eth_returns_smas_long_short_close)[eth_start_point],
                                                    end_date = index(eth_returns_smas_long_short_close)[eth_end_point],
                                                    ann_return = table.AnnualizedReturns(eth_returns_smas_long_short_close[,1])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_returns_smas_long_short_close[,1])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_returns_smas_long_short_close[,1])[3,1],
                                                    max_dd = maxDrawdown(eth_returns_smas_long_short_close[,1]))

for (k in 7:11){
  eth_performance_df <- eth_performance_df |> add_row(strategy = paste('sma_', colnames(eth_returns_smas_long_short_close)[k],sep = ''),
                                                      start_date = index(eth_returns_smas_long_short_close)[eth_start_point],
                                                      end_date = index(eth_returns_smas_long_short_close)[eth_end_point],
                                                      ann_return = table.AnnualizedReturns(eth_returns_smas_long_short_close[,k])[1,1],
                                                      ann_vol = table.AnnualizedReturns(eth_returns_smas_long_short_close[,k])[2,1],
                                                      sharpe = table.AnnualizedReturns(eth_returns_smas_long_short_close[,k])[3,1],
                                                      max_dd = maxDrawdown(eth_returns_smas_long_short_close[,k]))
}

#----------------------------------------------------------------------------------------------------------
#equal weighted SMAs signals
eth_smas_equal_weight <- xts::merge.xts(
  eth_smas_long_short_close[, "eth_return"],
  do.call(xts::merge.xts, eth_smas_position_list_close))

eth_smas_equal_weight$agg_position <- rowMeans(eth_smas_equal_weight[,c(2:6)])
eth_smas_equal_weight$gross <- eth_smas_equal_weight$eth_return * eth_smas_equal_weight$agg_position

eth_smas_equal_weight$turnover <- abs(eth_smas_equal_weight$agg_position - xts::lag.xts(eth_smas_equal_weight$agg_position, k =1))

eth_smas_equal_weight$net <- eth_smas_equal_weight$gross - eth_smas_equal_weight$turnover * t_cost

eth_start_point_equal_weight <- min(which(complete.cases(eth_smas_equal_weight[, c(1,8,10)])))
eth_end_point_equal_weight <- nrow(eth_smas_equal_weight)

charts.PerformanceSummary(eth_smas_equal_weight[eth_start_point_equal_weight:eth_end_point_equal_weight,c(1,8,10)])
table.AnnualizedReturns(eth_smas_equal_weight[eth_start_point_equal_weight:eth_end_point_equal_weight,c(1,8,10)])
maxDrawdown(eth_smas_equal_weight[eth_start_point_equal_weight:eth_end_point_equal_weight,c(1,8,10)])
SharpeRatio.annualized(eth_smas_equal_weight[eth_start_point_equal_weight:eth_end_point_equal_weight,c(1,8,10)])

eth_performance_df <- eth_performance_df |> add_row(strategy = 'smas_equal_weight_gross',
                                                    start_date = index(eth_smas_equal_weight)[eth_start_point_equal_weight],
                                                    end_date = index(eth_smas_equal_weight)[eth_end_point_equal_weight],
                                                    ann_return = table.AnnualizedReturns(eth_smas_equal_weight[,8])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_smas_equal_weight[,8])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_smas_equal_weight[,8])[3,1],
                                                    max_dd = maxDrawdown(eth_smas_equal_weight[,8]))

eth_performance_df <- eth_performance_df |> add_row(strategy = 'smas_equal_weight_net',
                                                    start_date = index(eth_smas_equal_weight)[eth_start_point_equal_weight],
                                                    end_date = index(eth_smas_equal_weight)[eth_end_point_equal_weight],
                                                    ann_return = table.AnnualizedReturns(eth_smas_equal_weight[,10])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_smas_equal_weight[,10])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_smas_equal_weight[,10])[3,1],
                                                    max_dd = maxDrawdown(eth_smas_equal_weight[,10]))

#----------------------------------------------------------------------------------------------------------
#inverse vol weighted SMAs
#roll_period <- 60

eth_smas_inverse_vol <- eth_smas_equal_weight[,c(1:6)]

#calc return stream of each SMA signal
eth_smas_inverse_vol$return_7 <- eth_smas_inverse_vol$position_close_7 * eth_smas_inverse_vol$eth_return
eth_smas_inverse_vol$return_30 <- eth_smas_inverse_vol$position_close_30 * eth_smas_inverse_vol$eth_return
eth_smas_inverse_vol$return_90 <- eth_smas_inverse_vol$position_close_90 * eth_smas_inverse_vol$eth_return
eth_smas_inverse_vol$return_180 <- eth_smas_inverse_vol$position_close_180 * eth_smas_inverse_vol$eth_return
eth_smas_inverse_vol$return_365 <- eth_smas_inverse_vol$position_close_365 * eth_smas_inverse_vol$eth_return

#calc rolling vol of each return stream
eth_smas_inverse_vol$vol_7 <- rollapply(eth_smas_inverse_vol$return_7, width = roll_period,
                                        FUN = sd, align = 'right', fill=NA)
eth_smas_inverse_vol$vol_7 <- lag(eth_smas_inverse_vol$vol_7, 1)

eth_smas_inverse_vol$vol_30 <- rollapply(eth_smas_inverse_vol$return_30, width = roll_period,
                                         FUN = sd, align = 'right', fill=NA)
eth_smas_inverse_vol$vol_30 <- lag(eth_smas_inverse_vol$vol_30, 1)

eth_smas_inverse_vol$vol_90 <- rollapply(eth_smas_inverse_vol$return_90, width = roll_period,
                                         FUN = sd, align = 'right', fill=NA)
eth_smas_inverse_vol$vol_90 <- lag(eth_smas_inverse_vol$vol_90, 1)

eth_smas_inverse_vol$vol_180 <- rollapply(eth_smas_inverse_vol$return_180, width = roll_period,
                                          FUN = sd, align = 'right', fill=NA)
eth_smas_inverse_vol$vol_180 <- lag(eth_smas_inverse_vol$vol_180, 1)

eth_smas_inverse_vol$vol_365 <- rollapply(eth_smas_inverse_vol$return_365, width = roll_period,
                                          FUN = sd, align = 'right', fill=NA)
eth_smas_inverse_vol$vol_365 <- lag(eth_smas_inverse_vol$vol_365, 1)

#calculate inverse vol
eth_smas_inverse_vol$inv_vol_7 <- 1/eth_smas_inverse_vol$vol_7
eth_smas_inverse_vol$inv_vol_30 <- 1/eth_smas_inverse_vol$vol_30
eth_smas_inverse_vol$inv_vol_90 <- 1/eth_smas_inverse_vol$vol_90
eth_smas_inverse_vol$inv_vol_180 <- 1/eth_smas_inverse_vol$vol_180
eth_smas_inverse_vol$inv_vol_365 <- 1/eth_smas_inverse_vol$vol_365

#calculate signal weight
eth_inv_vol_sums_cols <- eth_smas_inverse_vol[,c('inv_vol_7','inv_vol_30','inv_vol_90',
                                                 'inv_vol_180','inv_vol_365')]

eth_sum_inv_vols <- rowSums(eth_inv_vol_sums_cols,na.rm = F)

eth_smas_inverse_vol$weight_7 <- eth_smas_inverse_vol$inv_vol_7/eth_sum_inv_vols
eth_smas_inverse_vol$weight_30 <- eth_smas_inverse_vol$inv_vol_30/eth_sum_inv_vols
eth_smas_inverse_vol$weight_90 <- eth_smas_inverse_vol$inv_vol_90/eth_sum_inv_vols
eth_smas_inverse_vol$weight_180 <- eth_smas_inverse_vol$inv_vol_180/eth_sum_inv_vols
eth_smas_inverse_vol$weight_365 <- eth_smas_inverse_vol$inv_vol_365/eth_sum_inv_vols

#aggregated position
eth_smas_inverse_vol$agg_position <- eth_smas_inverse_vol$position_close_7 * eth_smas_inverse_vol$weight_7 + 
  eth_smas_inverse_vol$position_close_30 * eth_smas_inverse_vol$weight_30 +
  eth_smas_inverse_vol$position_close_90 * eth_smas_inverse_vol$weight_90 +
  eth_smas_inverse_vol$position_close_180 * eth_smas_inverse_vol$weight_180 +
  eth_smas_inverse_vol$position_close_365 * eth_smas_inverse_vol$weight_365

#gross return
eth_smas_inverse_vol$gross <- eth_smas_inverse_vol$eth_return * eth_smas_inverse_vol$agg_position

#turnover
eth_smas_inverse_vol$turnover <- abs(eth_smas_inverse_vol$agg_position - xts::lag.xts(eth_smas_inverse_vol$agg_position, k =1))

#net
eth_smas_inverse_vol$net <- eth_smas_inverse_vol$gross - eth_smas_inverse_vol$turnover * t_cost

eth_start_point_inv_vol <- min(which(complete.cases(eth_smas_inverse_vol[, c(1,28,30)])))
eth_end_point_inv_vol<- nrow(eth_smas_inverse_vol)

charts.PerformanceSummary(eth_smas_inverse_vol[eth_start_point_inv_vol:eth_end_point_inv_vol,c(1,28,30)])
table.AnnualizedReturns(eth_smas_inverse_vol[eth_start_point_inv_vol:eth_end_point_inv_vol,c(1,28,30)])
maxDrawdown(eth_smas_inverse_vol[eth_start_point_inv_vol:eth_end_point_inv_vol,c(1,28,30)])
SharpeRatio.annualized(eth_smas_inverse_vol[eth_start_point_inv_vol:eth_end_point_inv_vol,c(1,28,30)])

eth_performance_df <- eth_performance_df |> add_row(strategy = 'smas_inv_vol_gross',
                                                    start_date = index(eth_smas_inverse_vol)[eth_start_point_inv_vol],
                                                    end_date = index(eth_smas_inverse_vol)[eth_end_point_inv_vol],
                                                    ann_return = table.AnnualizedReturns(eth_smas_inverse_vol[,28])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_smas_inverse_vol[,28])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_smas_inverse_vol[,28])[3,1],
                                                    max_dd = maxDrawdown(eth_smas_inverse_vol[,28]))

eth_performance_df <- eth_performance_df |> add_row(strategy = 'smas_inv_vol_net',
                                                    start_date = index(eth_smas_inverse_vol)[eth_start_point_inv_vol],
                                                    end_date = index(eth_smas_inverse_vol)[eth_end_point_inv_vol],
                                                    ann_return = table.AnnualizedReturns(eth_smas_inverse_vol[,30])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_smas_inverse_vol[,30])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_smas_inverse_vol[,30])[3,1],
                                                    max_dd = maxDrawdown(eth_smas_inverse_vol[,30]))

#----------------------------------------------------------------------------------------------------------
#Price - EMAs
eth_emas <- c(7,30,90,180,365)

eth_emas_long_short_close <- eth_xts[,c('close','ema_1w','ema_1m','ema_3m','ema_6m','ema_1y')]
eth_emas_long_short_close$eth_return <- dailyReturn(eth_emas_long_short_close$close,type = 'arithmetic')

eth_emas_signals_list_close <- list()
eth_emas_position_list_close <- list()
eth_emas_gross_list_close <- list()
eth_emas_turnover_list_close <- list()
eth_emas_net_list_close <- list()


#calculate signals, positions, gross returns, turnover, and net returns
#for each EMA strategy

for (i in seq_along(eth_emas)) {
  
  key <- paste0('close', "_", eth_emas[i])
  
  eth_signal_vals_ema <- ifelse(
    coredata(eth_emas_long_short_close[, 1]) > coredata(eth_emas_long_short_close[, i + 1]),
    1, -1
  )
  
  eth_signal_ema <- xts::xts(eth_signal_vals_ema, order.by = zoo::index(eth_emas_long_short_close))
  colnames(eth_signal_ema) <- paste0("signal_ema", '_close', "_", eth_emas[i])
  
  eth_position_ema <- xts::lag.xts(eth_signal_ema, k = 1)
  colnames(eth_position_ema) <- paste0("position_ema", '_close', "_", eth_emas[i])
  
  eth_gross_ema <- eth_emas_long_short_close$eth_return * eth_position_ema
  eth_gross_ema <- xts::xts(coredata(eth_gross_ema), order.by = zoo::index(eth_emas_long_short_close))
  colnames(eth_gross_ema) <- paste0("gross_ema", '_close', "_", eth_emas[i])
  
  eth_turnover_ema <- abs(eth_position_ema - xts::lag.xts(eth_position_ema, k = 1))
  eth_turnover_ema <- xts::xts(coredata(eth_turnover_ema), order.by = zoo::index(eth_emas_long_short_close))
  eth_turnover_ema[is.na(eth_turnover_ema)] <- 0
  colnames(eth_turnover_ema) <- paste0("turnover_ema", '_close', "_", eth_emas[i])
  
  eth_net_ema <- eth_gross_ema - eth_turnover_ema * t_cost
  eth_net_ema <- xts::xts(coredata(eth_net_ema), order.by = zoo::index(eth_emas_long_short_close))
  colnames(eth_net_ema) <- paste0("net_ema", "close", "_", eth_emas[i])
  
  eth_emas_signals_list_close[[key]] <- eth_signal_ema
  eth_emas_position_list_close[[key]] <- eth_position_ema
  eth_emas_gross_list_close[[key]]    <- eth_gross_ema
  eth_emas_turnover_list_close[[key]] <- eth_turnover_ema
  eth_emas_net_list_close[[key]]      <- eth_net_ema
  
}


eth_returns_emas_long_short_close <- xts::merge.xts(
  eth_emas_long_short_close[, "eth_return"],
  do.call(xts::merge.xts, eth_emas_gross_list_close),
  do.call(xts::merge.xts, eth_emas_net_list_close)
)

eth_start_point_ema <- min(which(complete.cases(eth_returns_emas_long_short_close[, c(1:ncol(eth_returns_emas_long_short_close))])))
eth_end_point_ema <- nrow(eth_returns_emas_long_short_close)

charts.PerformanceSummary(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,])
table.AnnualizedReturns(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,])
maxDrawdown(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,])
SharpeRatio.annualized(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,])

#net only

charts.PerformanceSummary(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,c(1,7:11)])
table.AnnualizedReturns(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,c(1,7:11)])
maxDrawdown(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,c(1,7:11)])
SharpeRatio.annualized(eth_returns_emas_long_short_close[eth_start_point_ema:eth_end_point_ema,c(1,7:11)])

for (k in 7:11){
  eth_performance_df <- eth_performance_df |> add_row(strategy = paste('ema_', colnames(eth_returns_emas_long_short_close)[k],sep = ''),
                                                      start_date = index(eth_returns_emas_long_short_close)[eth_start_point_ema],
                                                      end_date = index(eth_returns_emas_long_short_close)[eth_end_point_ema],
                                                      ann_return = table.AnnualizedReturns(eth_returns_emas_long_short_close[,k])[1,1],
                                                      ann_vol = table.AnnualizedReturns(eth_returns_emas_long_short_close[,k])[2,1],
                                                      sharpe = table.AnnualizedReturns(eth_returns_emas_long_short_close[,k])[3,1],
                                                      max_dd = maxDrawdown(eth_returns_emas_long_short_close[,k]))
}


#----------------------------------------------------------------------------------------------------------
#equal weighted EMAs signals
eth_emas_equal_weight <- xts::merge.xts(
  eth_emas_long_short_close[, "eth_return"],
  do.call(xts::merge.xts, eth_emas_position_list_close))

eth_emas_equal_weight$agg_position <- rowMeans(eth_emas_equal_weight[,c(2:6)])
eth_emas_equal_weight$gross <- eth_emas_equal_weight$eth_return * eth_emas_equal_weight$agg_position

eth_emas_equal_weight$turnover <- abs(eth_emas_equal_weight$agg_position - xts::lag.xts(eth_emas_equal_weight$agg_position, k =1))

eth_emas_equal_weight$net <- eth_emas_equal_weight$gross - eth_emas_equal_weight$turnover * t_cost

eth_start_point_equal_weight_ema <- min(which(complete.cases(eth_emas_equal_weight[, c(1,8,10)])))
eth_end_point_equal_weight_ema <- nrow(eth_emas_equal_weight)

charts.PerformanceSummary(eth_emas_equal_weight[eth_start_point_equal_weight_ema:eth_end_point_equal_weight_ema,c(1,8,10)])
table.AnnualizedReturns(eth_emas_equal_weight[eth_start_point_equal_weight_ema:eth_end_point_equal_weight_ema,c(1,8,10)])
maxDrawdown(eth_emas_equal_weight[eth_start_point_equal_weight_ema:eth_end_point_equal_weight_ema,c(1,8,10)])
SharpeRatio.annualized(eth_emas_equal_weight[eth_start_point_equal_weight_ema:eth_end_point_equal_weight_ema,c(1,8,10)])

eth_performance_df <- eth_performance_df |> add_row(strategy = 'emas_equal_weight_gross',
                                                    start_date = index(eth_emas_equal_weight)[eth_start_point_equal_weight_ema],
                                                    end_date = index(eth_emas_equal_weight)[eth_end_point_equal_weight_ema],
                                                    ann_return = table.AnnualizedReturns(eth_emas_equal_weight[,8])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_emas_equal_weight[,8])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_emas_equal_weight[,8])[3,1],
                                                    max_dd = maxDrawdown(eth_emas_equal_weight[,8]))

eth_performance_df <- eth_performance_df |> add_row(strategy = 'emas_equal_weight_net',
                                                    start_date = index(eth_emas_equal_weight)[eth_start_point_equal_weight_ema],
                                                    end_date = index(eth_emas_equal_weight)[eth_end_point_equal_weight_ema],
                                                    ann_return = table.AnnualizedReturns(eth_emas_equal_weight[,10])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_emas_equal_weight[,10])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_emas_equal_weight[,10])[3,1],
                                                    max_dd = maxDrawdown(eth_emas_equal_weight[,10]))


#----------------------------------------------------------------------------------------------------------
#inverse vol weighted EMAs
eth_roll_period_ema <- 60

eth_emas_inverse_vol <- eth_emas_equal_weight[,c(1:6)]

#calc return stream of each ema signal
eth_emas_inverse_vol$return_7 <- eth_emas_inverse_vol$position_ema_close_7 * eth_emas_inverse_vol$eth_return
eth_emas_inverse_vol$return_30 <- eth_emas_inverse_vol$position_ema_close_30 * eth_emas_inverse_vol$eth_return
eth_emas_inverse_vol$return_90 <- eth_emas_inverse_vol$position_ema_close_90 * eth_emas_inverse_vol$eth_return
eth_emas_inverse_vol$return_180 <- eth_emas_inverse_vol$position_ema_close_180 * eth_emas_inverse_vol$eth_return
eth_emas_inverse_vol$return_365 <- eth_emas_inverse_vol$position_ema_close_365 * eth_emas_inverse_vol$eth_return

#calc rolling vol of each return stream
eth_emas_inverse_vol$vol_7 <- rollapply(eth_emas_inverse_vol$return_7, width = eth_roll_period_ema,
                                        FUN = sd, align = 'right', fill=NA)
eth_emas_inverse_vol$vol_7 <- lag(eth_emas_inverse_vol$vol_7, 1)

eth_emas_inverse_vol$vol_30 <- rollapply(eth_emas_inverse_vol$return_30, width = eth_roll_period_ema,
                                         FUN = sd, align = 'right', fill=NA)
eth_emas_inverse_vol$vol_30 <- lag(eth_emas_inverse_vol$vol_30, 1)

eth_emas_inverse_vol$vol_90 <- rollapply(eth_emas_inverse_vol$return_90, width = eth_roll_period_ema,
                                         FUN = sd, align = 'right', fill=NA)
eth_emas_inverse_vol$vol_90 <- lag(eth_emas_inverse_vol$vol_90, 1)

eth_emas_inverse_vol$vol_180 <- rollapply(eth_emas_inverse_vol$return_180, width = eth_roll_period_ema,
                                          FUN = sd, align = 'right', fill=NA)
eth_emas_inverse_vol$vol_180 <- lag(eth_emas_inverse_vol$vol_180, 1)

eth_emas_inverse_vol$vol_365 <- rollapply(eth_emas_inverse_vol$return_365, width = eth_roll_period_ema,
                                          FUN = sd, align = 'right', fill=NA)
eth_emas_inverse_vol$vol_365 <- lag(eth_emas_inverse_vol$vol_365, 1)

#calculate inverse vol
eth_emas_inverse_vol$inv_vol_7 <- 1/eth_emas_inverse_vol$vol_7
eth_emas_inverse_vol$inv_vol_30 <- 1/eth_emas_inverse_vol$vol_30
eth_emas_inverse_vol$inv_vol_90 <- 1/eth_emas_inverse_vol$vol_90
eth_emas_inverse_vol$inv_vol_180 <- 1/eth_emas_inverse_vol$vol_180
eth_emas_inverse_vol$inv_vol_365 <- 1/eth_emas_inverse_vol$vol_365

#calculate signal weight
eth_inv_vol_sums_cols_ema <- eth_emas_inverse_vol[,c('inv_vol_7','inv_vol_30','inv_vol_90',
                                                     'inv_vol_180','inv_vol_365')]

eth_sum_inv_vols_ema <- rowSums(eth_inv_vol_sums_cols_ema,na.rm = F)

eth_emas_inverse_vol$weight_7 <- eth_emas_inverse_vol$inv_vol_7/eth_sum_inv_vols_ema
eth_emas_inverse_vol$weight_30 <- eth_emas_inverse_vol$inv_vol_30/eth_sum_inv_vols_ema
eth_emas_inverse_vol$weight_90 <- eth_emas_inverse_vol$inv_vol_90/eth_sum_inv_vols_ema
eth_emas_inverse_vol$weight_180 <- eth_emas_inverse_vol$inv_vol_180/eth_sum_inv_vols_ema
eth_emas_inverse_vol$weight_365 <- eth_emas_inverse_vol$inv_vol_365/eth_sum_inv_vols_ema

#aggregated position
eth_emas_inverse_vol$agg_position <- eth_emas_inverse_vol$position_ema_close_7 * eth_emas_inverse_vol$weight_7 + 
  eth_emas_inverse_vol$position_ema_close_30 * eth_emas_inverse_vol$weight_30 +
  eth_emas_inverse_vol$position_ema_close_90 * eth_emas_inverse_vol$weight_90 +
  eth_emas_inverse_vol$position_ema_close_180 * eth_emas_inverse_vol$weight_180 +
  eth_emas_inverse_vol$position_ema_close_365 * eth_emas_inverse_vol$weight_365

#gross return
eth_emas_inverse_vol$gross <- eth_emas_inverse_vol$eth_return * eth_emas_inverse_vol$agg_position

#turnover
eth_emas_inverse_vol$turnover <- abs(eth_emas_inverse_vol$agg_position - xts::lag.xts(eth_emas_inverse_vol$agg_position, k =1))

#net
eth_emas_inverse_vol$net <- eth_emas_inverse_vol$gross - eth_emas_inverse_vol$turnover * t_cost

eth_start_point_inv_vol_ema <- min(which(complete.cases(eth_emas_inverse_vol[, c(1,28,30)])))
eth_end_point_inv_vol_ema <- nrow(eth_emas_inverse_vol)

charts.PerformanceSummary(eth_emas_inverse_vol[eth_start_point_inv_vol_ema:eth_end_point_inv_vol_ema,c(1,28,30)])
table.AnnualizedReturns(eth_emas_inverse_vol[eth_start_point_inv_vol_ema:eth_end_point_inv_vol_ema,c(1,28,30)])
maxDrawdown(eth_emas_inverse_vol[eth_start_point_inv_vol_ema:eth_end_point_inv_vol_ema,c(1,28,30)])
SharpeRatio.annualized(eth_emas_inverse_vol[eth_start_point_inv_vol_ema:eth_end_point_inv_vol_ema,c(1,28,30)])

eth_performance_df <- eth_performance_df |> add_row(strategy = 'emas_inv_vol_gross',
                                                    start_date = index(eth_emas_inverse_vol)[eth_start_point_inv_vol_ema],
                                                    end_date = index(eth_emas_inverse_vol)[eth_end_point_inv_vol_ema],
                                                    ann_return = table.AnnualizedReturns(eth_emas_inverse_vol[,28])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_emas_inverse_vol[,28])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_emas_inverse_vol[,28])[3,1],
                                                    max_dd = maxDrawdown(eth_emas_inverse_vol[,28]))

eth_performance_df <- eth_performance_df |> add_row(strategy = 'emas_inv_vol_net',
                                                    start_date = index(eth_emas_inverse_vol)[eth_start_point_inv_vol_ema],
                                                    end_date = index(eth_emas_inverse_vol)[eth_end_point_inv_vol_ema],
                                                    ann_return = table.AnnualizedReturns(eth_emas_inverse_vol[,30])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_emas_inverse_vol[,30])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_emas_inverse_vol[,30])[3,1],
                                                    max_dd = maxDrawdown(eth_emas_inverse_vol[,30]))

#----------------------------------------------------------------------------------------------------------
#MACD model
eth_macd_close <- eth_xts[,c('close','macd','macd_signal')]
eth_macd_close$eth_return <- dailyReturn(eth_macd_close$close,type = 'arithmetic')


eth_macd_close$macd_signal <- ifelse(eth_macd_close$macd > eth_macd_close$macd_signal, 1, -1)
eth_macd_close$position <- xts::lag.xts(eth_macd_close$macd_signal, k = 1)


eth_macd_close$gross <- eth_macd_close$eth_return * eth_macd_close$position

eth_macd_close$turnover <- abs(eth_macd_close$position - xts::lag.xts(eth_macd_close$position, k =1))


eth_macd_close$net <- eth_macd_close$gross - eth_macd_close$turnover * t_cost

eth_start_point_macd <- min(which(complete.cases(eth_macd_close[, c(4,6,8)])))
eth_end_point_macd <- nrow(eth_macd_close)

charts.PerformanceSummary(eth_macd_close[eth_start_point_macd:eth_end_point_macd,c(4,6,8)])
table.AnnualizedReturns(eth_macd_close[eth_start_point_macd:eth_end_point_macd,c(4,6,8)])
maxDrawdown(eth_macd_close[eth_start_point_macd:eth_end_point_macd,c(4,6,8)])
SharpeRatio.annualized(eth_macd_close[eth_start_point_macd:eth_end_point_macd,c(4,6,8)])

eth_performance_df <- eth_performance_df |> add_row(strategy = 'macd_gross',
                                                    start_date = index(eth_macd_close)[eth_start_point_macd],
                                                    end_date = index(eth_macd_close)[eth_end_point_macd],
                                                    ann_return = table.AnnualizedReturns(eth_macd_close[,6])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_macd_close[,6])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_macd_close[,6])[3,1],
                                                    max_dd = maxDrawdown(eth_macd_close[,6]))

eth_performance_df <- eth_performance_df |> add_row(strategy = 'macd_net',
                                                    start_date = index(eth_macd_close)[eth_start_point_macd],
                                                    end_date = index(eth_macd_close)[eth_end_point_macd],
                                                    ann_return = table.AnnualizedReturns(eth_macd_close[,8])[1,1],
                                                    ann_vol = table.AnnualizedReturns(eth_macd_close[,8])[2,1],
                                                    sharpe = table.AnnualizedReturns(eth_macd_close[,8])[3,1],
                                                    max_dd = maxDrawdown(eth_macd_close[,8]))


#----------------------------------------------------------------------------------------------------------
#combine returns for summary table and analysis
eth_combined_performance_xts <- merge(eth_returns_smas_long_short_close,eth_returns_emas_long_short_close)
eth_combined_performance_xts <- merge(eth_combined_performance_xts, eth_smas_equal_weight[,c(8,10)])
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "gross"] <- "gross_smas_equal_weight"
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "net"] <- "net_smas_equal_weight"

#inverse vol weighted smas
eth_combined_performance_xts <- merge(eth_combined_performance_xts, eth_smas_inverse_vol[,c(28,30)])
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "gross"] <- "gross_smas_inv_vol"
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "net"] <- "net_smas_inv_vol"


#inverse vol weighted emas
eth_combined_performance_xts <- merge(eth_combined_performance_xts, eth_emas_equal_weight[,c(8,10)])
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "gross"] <- "gross_emas_equal_weight"
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "net"] <- "net_emas_equal_weight"

eth_combined_performance_xts <- merge(eth_combined_performance_xts, eth_emas_inverse_vol[,c(28,30)])
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "gross"] <- "gross_emas_inv_vol"
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "net"] <- "net_emas_inv_vol"


#macd
eth_combined_performance_xts <- merge(eth_combined_performance_xts, eth_macd_close[,c(6,8)])
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "gross"] <- "gross_macd"
colnames(eth_combined_performance_xts)[colnames(eth_combined_performance_xts) == "net"] <- "net_macd"


eth_combined_performance_xts$eth_return.1 <- NULL

eth_combined_performance_xts <- na.omit(eth_combined_performance_xts)

#xts of net returns only
eth_combined_net_performance_xts <- eth_combined_performance_xts[, c(
  "eth_return",
  grep("^net", colnames(eth_combined_performance_xts), value = TRUE)
)]


charts.PerformanceSummary(eth_combined_net_performance_xts)
table.AnnualizedReturns(eth_combined_net_performance_xts)
maxDrawdown(eth_combined_net_performance_xts)
SharpeRatio.annualized(eth_combined_net_performance_xts)


#scaled smas equal weight and inv_vol weight
eth_combined_performance_xts_scaled <- eth_combined_performance_xts[,c('eth_return','gross_smas_equal_weight',
                                                                       'net_smas_equal_weight', 'gross_smas_inv_vol',
                                                                               'net_smas_inv_vol')]


eth_smas_equal_weight_scale <- StdDev.annualized(eth_combined_performance_xts_scaled$eth_return)[1]/StdDev.annualized(eth_combined_performance_xts_scaled$gross_smas_equal_weight)[1]
eth_inv_vol_smas_scale <- StdDev.annualized(eth_combined_performance_xts_scaled$eth_return)[1]/StdDev.annualized(eth_combined_performance_xts_scaled$gross_smas_inv_vol)[1]


eth_combined_performance_xts_scaled$gross_smas_equal_weight_scaled <- eth_combined_performance_xts_scaled$gross_smas_equal_weight * eth_smas_equal_weight_scale
eth_combined_performance_xts_scaled$gross_smas_inv_vol_scaled <- eth_combined_performance_xts_scaled$gross_smas_inv_vol * eth_inv_vol_smas_scale

eth_combined_performance_xts_scaled$net_smas_equal_weight_scaled <- eth_combined_performance_xts_scaled$net_smas_equal_weight * eth_smas_equal_weight_scale
eth_combined_performance_xts_scaled$net_smas_inv_vol_scaled <- eth_combined_performance_xts_scaled$net_smas_inv_vol * eth_inv_vol_smas_scale

charts.PerformanceSummary(eth_combined_performance_xts_scaled)
table.AnnualizedReturns(eth_combined_performance_xts_scaled)
maxDrawdown(eth_combined_performance_xts_scaled)
SharpeRatio.annualized(eth_combined_performance_xts_scaled)

table.AnnualizedReturns(eth_combined_performance_xts_scaled)
SharpeRatio.annualized(eth_combined_performance_xts_scaled)




#summary dataframe to be used for report
eth_net_performance_df <- data.frame(
  strategy = character(),
  ann_return = double(),
  ann_vol = double(),
  sharpe = double(),
  max_dd = double()
)

#input each strategy and performance metrics into the dataframe
eth_net_performance_df <- do.call(
  rbind,
  lapply(colnames(eth_combined_net_performance_xts), function(strategy_name) {
    
    eth_ret_xts <- eth_combined_net_performance_xts[, strategy_name, drop = FALSE]
    eth_ret_xts <- na.omit(eth_ret_xts)
    
    eth_ann_tbl <- table.AnnualizedReturns(eth_ret_xts)
    eth_max_dd  <- maxDrawdown(eth_ret_xts)
    
    data.frame(
      strategy   = strategy_name,
      ann_return = as.numeric(eth_ann_tbl["Annualized Return", 1]),
      ann_vol    = as.numeric(eth_ann_tbl["Annualized Std Dev", 1]),
      sharpe     = as.numeric(eth_ann_tbl["Annualized Sharpe (Rf=0%)", 1]),
      max_dd     = as.numeric(eth_max_dd),
      row.names  = NULL
    )
  })
)


eth_net_performance_df


#format for Latex
eth_net_performance_df_latex <- eth_net_performance_df %>%
  mutate(
    strategy   = gsub("_", "\\\\_", strategy),
    ann_return = sprintf("%.1f\\%%", 100 * ann_return),
    ann_vol    = sprintf("%.1f\\%%", 100 * ann_vol),
    sharpe     = sprintf("%.2f", sharpe),
    max_dd     = sprintf("%.1f\\%%", 100 * max_dd)
  )

eth_latex_table <- knitr::kable(
  eth_net_performance_df_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r", "r"),
  col.names = c("Strategy", "Ann. Return", "Ann. Vol", "Sharpe", "Max DD"),
  caption = "Net performance of Ethereum Trend Following Strategies",
  label = "tab:net_performance_eth",
  escape = FALSE
)

cat(eth_latex_table)
