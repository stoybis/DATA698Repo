setwd("~/Desktop/CUNY_MSDS/DATA698/crypto")

library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(lubridate)
library(xts)
library(TTR)
library(caret)
library(corrplot)
library(MLmetrics)
library(knitr)



price_history <- read.csv('btc_eth_price_history.csv')

#-----------------------------------------------------------------------------------------------------------
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

btc_xts$btc_return <-  dailyReturn(btc_xts$close,type = 'arithmetic')

#-----------------------------------------------------------------------------------------------------------
#add continuous features
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

#MACD
btc_xts$macd <- MACD(Cl(btc_xts))[,1]
btc_xts$macd_signal <- MACD(Cl(btc_xts))[,2]

#RSI
btc_xts$rsi <- RSI(Cl(btc_xts))

#ATR
btc_xts$atr <-ATR(btc_xts[,c('high','low','close')])[,2]

#Donchian
btc_xts$donch_high <- DonchianChannel(btc_xts[,c('high','low')],n = 20)[,1]
btc_xts$donch_mid <- DonchianChannel(btc_xts[,c('high','low')],n = 20)[,2]
btc_xts$donch_low <- DonchianChannel(btc_xts[,c('high','low')],n = 20)[,3]

#-----------------------------------------------------------------------------------------------------------

#add categorical features
#SMA
btc_xts$sma_1w_flag <- ifelse(btc_xts$close > btc_xts$sma_1w, 1, -1)
btc_xts$sma_1m_flag  <- ifelse(btc_xts$close > btc_xts$sma_1m, 1, -1)
btc_xts$sma_3m_flag  <- ifelse(btc_xts$close > btc_xts$sma_3m, 1, -1)
btc_xts$sma_6m_flag  <- ifelse(btc_xts$close > btc_xts$sma_6m, 1, -1)
btc_xts$sma_1y_flag  <- ifelse(btc_xts$close > btc_xts$sma_1y, 1, -1)

#EMA
btc_xts$ema_1w_flag <- ifelse(btc_xts$close > btc_xts$ema_1m, 1, -1)
btc_xts$ema_1m_flag  <- ifelse(btc_xts$close > btc_xts$ema_1m, 1, -1)
btc_xts$ema_3m_flag  <- ifelse(btc_xts$close > btc_xts$ema_3m, 1, -1)
btc_xts$ema_6m_flag  <- ifelse(btc_xts$close > btc_xts$ema_6m, 1, -1)
btc_xts$ema_1y_flag  <- ifelse(btc_xts$close > btc_xts$ema_1y, 1, -1)

#MACD Flag
btc_xts$macd_flag <- ifelse(btc_xts$macd > btc_xts$macd_signal, 1, -1)
btc_xts$macd_diff <- btc_xts$macd - btc_xts$macd_signal

#Donchian Flag
donch_high_lag <- lag.xts(btc_xts$donch_high, 1)
donch_low_lag  <- lag.xts(btc_xts$donch_low, 1)


btc_xts$donchian_flag <- ifelse(btc_xts$close > donch_high_lag, 1,
                                ifelse(btc_xts$close < donch_low_lag, -1, NA))

btc_xts$donchian_flag <- zoo::na.locf(btc_xts$donchian_flag, na.rm = FALSE)
#btc_xts$donchian_flag[is.na(btc_xts$donchian_flag)] <- 0

#-----------------------------------------------------------------------------------------------------------
#close vs feature as percent
#SMA
btc_xts$sma_1w_percentage <- btc_xts$close / btc_xts$sma_1w
btc_xts$sma_1m_percentage <- btc_xts$close / btc_xts$sma_1m
btc_xts$sma_3m_percentage <- btc_xts$close / btc_xts$sma_3m
btc_xts$sma_6m_percentage <- btc_xts$close / btc_xts$sma_6m
btc_xts$sma_1y_percentage <- btc_xts$close / btc_xts$sma_1y

#EMA
btc_xts$ema_1w_percentage <- btc_xts$close / btc_xts$ema_1w
btc_xts$ema_1m_percentage <- btc_xts$close / btc_xts$ema_1m
btc_xts$ema_3m_percentage <- btc_xts$close / btc_xts$ema_3m
btc_xts$ema_6m_percentage <- btc_xts$close / btc_xts$ema_6m
btc_xts$ema_1y_percentage <- btc_xts$close / btc_xts$ema_1y

#ATR
btc_xts$atr_percentage <- btc_xts$atr / btc_xts$close

#Donchian
btc_xts$donch_width <- (btc_xts$donch_high - btc_xts$donch_low)/btc_xts$close
btc_xts$donch_position <- (btc_xts$close - btc_xts$donch_low)/(btc_xts$donch_high - btc_xts$donch_low)

#-----------------------------------------------------------------------------------------------------------
#forward 7 day return 

btc_xts$fwd_7d_return <- (lag.xts(btc_xts$close, -7) / btc_xts$close) - 1

  
#-----------------------------------------------------------------------------------------------------------
#new xts for modelling - lags, features, etc
#train - test split
#train control - walk forward cross validation

#extract feature columns 
#feature_cols <- c(24:45,19)
# feature_cols <- c(
#   "sma_1w_flag", "sma_1m_flag", "sma_3m_flag", "sma_6m_flag", "sma_1y_flag",
#   "ema_1w_flag", "ema_1m_flag", "ema_3m_flag", "ema_6m_flag", "ema_1y_flag",
#   "macd_flag", "macd_diff", "donchian_flag",
#   "sma_1w_percentage", "sma_1m_percentage", "sma_3m_percentage",
#   "sma_6m_percentage", "sma_1y_percentage",
#   "ema_1w_percentage", "ema_1m_percentage", "ema_3m_percentage", "ema_6m_percentage",
#   "rsi"
# )

#including both flags and percentages results in multi-collinearity 
#try dropping the flags and keeping the percentages

feature_cols <- c(
  "macd_flag", "macd_diff", "donchian_flag",
  "sma_1w_percentage", "sma_1m_percentage", "sma_3m_percentage",
  "sma_6m_percentage", "sma_1y_percentage",
  "ema_1w_percentage", "ema_1m_percentage", "ema_3m_percentage", "ema_6m_percentage",
  "rsi"
)


X <- lag.xts(btc_xts[,feature_cols],k=1)
y <- btc_xts$fwd_7d_return

model_data <- merge.xts(X,y)

# remove NA rows
model_data <- na.omit(model_data)

#-----------------------------------------------------------------------------------------------------------
#data exploration

#correlation
corrplot(cor(model_data),method = 'number', type='lower')

#distribution
model_data_df <- data.frame(model_data)

model_data_df |> pivot_longer(cols=everything(), names_to = 'variable', values_to = 'value') |>
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~variable,scales='free')+theme_minimal()



#-----------------------------------------------------------------------------------------------------------
#train - test split
#use 2016-2022 to perform training with walk forward CV
#2023 validation set used to predict with each model and evaluate best performer
#select model based on 2023 metrics, retrain that model with walk forward CV over 2016-2023
#compare performance to trend model for 2024-2024

train_data <- model_data["/2022-12-31"]

validation_data <- model_data["2023-01-01/2023-12-31"] 

test_data <- model_data["2024-01-01/"]

model_retraining_data <- model_data["/2023-12-31"]

#train control
#index of the start of 2019
end_initial_train_period <- which(as.Date(zoo::index(train_data)) == as.Date("2019-01-01")) - 1

ctrl <- trainControl(
  method = "timeslice",
  initialWindow = end_initial_train_period,
  horizon = 365,          
  fixedWindow = FALSE,    
  skip = 364
)


#validation set metrics
validation_performance <- data.frame(model=character(),
                                     rmse=numeric(),
                                     mse = numeric(),
                                     mae=numeric(),
                                     r_squared = numeric())


#-----------------------------------------------------------------------------------------------------------
#linear model

model1 <- train(
  fwd_7d_return ~ ., 
  data = train_data,
  method = 'lm',
  trControl = ctrl,
  metric = 'RMSE',
  preProcess = c('center','scale')
)


#validation set performance
model1_pred <- predict(model1, newdata = validation_data)

validation_performance[nrow(validation_performance)+1,] <- c('linear_model',
                                                             round(RMSE(model1_pred,validation_data$fwd_7d_return),4),
                                                             round(MSE(model1_pred,validation_data$fwd_7d_return),4),
                                                             round(MAE(model1_pred,validation_data$fwd_7d_return),4),
                                                             round(R2_Score(model1_pred,validation_data$fwd_7d_return),4))


#-----------------------------------------------------------------------------------------------------------
#Lasso model
#lasso can take coefficients to zero - dropping unnecessary variables
#RIDGE does not take coefficients all the way to zero

modelLookup('glmnet')

#need tuning grid for Lambda hyper-parameter
#Lambda controls the penalty in the loss function
#alpha - 0 is ridge, 1 is lasso
lassoGrid <- expand.grid(
  alpha = 1, 
  lambda = 10^seq(-3, 0, length = 10)
)

set.seed(123)

model2 <- train(
  fwd_7d_return ~ ., 
  data = train_data,
  method = 'glmnet',
  trControl = ctrl,
  metric = 'RMSE',
  tuneGrid = lassoGrid,
  preProcess = c('center','scale')
)


#validation set performance
model2_pred <- predict(model2, newdata = validation_data)

validation_performance[nrow(validation_performance)+1,] <- c('lasso_model',
                                                             round(RMSE(model2_pred,validation_data$fwd_7d_return),4),
                                                             round(MSE(model2_pred,validation_data$fwd_7d_return),4),
                                                             round(MAE(model2_pred,validation_data$fwd_7d_return),4),
                                                             round(R2_Score(model2_pred,validation_data$fwd_7d_return),4))


#-----------------------------------------------------------------------------------------------------------
#random forest xgBoost

modelLookup('xgbTree')

#about two minutes to run
xgbGrid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(2, 3, 4),
  eta = c(0.05, 0.1),
  gamma = c(0, 0.1),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.8, 1)
)

set.seed(123)

model3 <- train(
  fwd_7d_return ~ ., 
  data = train_data,
  method = 'xgbTree',
  trControl = ctrl,
  metric = 'RMSE',
  tuneGrid = xgbGrid
)


#validation set performance
model3_pred <- predict(model3, newdata = validation_data)

validation_performance[nrow(validation_performance)+1,] <- c('xgbTree',
                                                             round(RMSE(model3_pred,validation_data$fwd_7d_return),4),
                                                             round(MSE(model3_pred,validation_data$fwd_7d_return),4),
                                                             round(MAE(model3_pred,validation_data$fwd_7d_return),4),
                                                             round(R2_Score(model3_pred,validation_data$fwd_7d_return),4))


#-----------------------------------------------------------------------------------------------------------
#SVM model
modelLookup('svmRadial')

#about two minutes to run
svmGrid <- expand.grid(
  sigma = c(0.01, 0.05),
  C = c(1, 10)
)

set.seed(123)

model4 <- train(
  fwd_7d_return ~ ., 
  data = train_data,
  method = 'svmRadial',
  trControl = ctrl,
  metric = 'RMSE',
  tuneGrid = svmGrid,
  preProcess = c('center','scale')
)

#validation set performance
model4_pred <- predict(model4, newdata = validation_data)

validation_performance[nrow(validation_performance)+1,] <- c('svm',
                                                             round(RMSE(model4_pred,validation_data$fwd_7d_return),4),
                                                             round(MSE(model4_pred,validation_data$fwd_7d_return),4),
                                                             round(MAE(model4_pred,validation_data$fwd_7d_return),4),
                                                             round(R2_Score(model4_pred,validation_data$fwd_7d_return),4))


#format validation performance table for Latex

validation_performance_latex <- validation_performance %>%
  mutate(
    model = gsub("_", "\\\\_", model),
    rmse = sprintf("%.4f", as.numeric(rmse)),
    mse = sprintf("%.4f", as.numeric(mse)),
    mae = sprintf("%.4f", as.numeric(mae)),
    r_squared = sprintf("%.2f", as.numeric(r_squared))
  )

latex_table <- knitr::kable(
  validation_performance_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r", "r"),
  col.names = c("Model", "RMSE", "MSE", "MAE", "$R^2$"),
  caption = "Validation performance of Bitcoin machine learning models",
  label = "tab:validation_performance",
  escape = FALSE
)

cat(latex_table)


#-----------------------------------------------------------------------------------------------------------
#retraining selected model and generating trading performance

# set.seed(123)
# 
# selected_model <- train(
#   fwd_7d_return ~ ., 
#   data = model_retraining_data,
#   method = 'xgbTree',
#   trControl = ctrl,
#   metric = 'RMSE',
#   tuneGrid = xgbGrid
# )
# 
# 
# selected_model_test_pred <- predict(selected_model, newdata = test_data)
# selected_model_test_pred <- xts(selected_model_test_pred, order.by = index(test_data))
# 
# selected_model_test_position <- ifelse(selected_model_test_pred > 0, 1, -1)
# selected_model_test_position <- xts(selected_model_test_position, order.by = index(test_data))
# 
# btc_return_test_period <- btc_xts["2024-01-01/", 'btc_return']
# 
# 
# test_set_performance_xts <- merge.xts(btc_return_test_period, selected_model_test_position)
# 
# test_set_performance_xts <- na.omit(test_set_performance_xts)
# 
# colnames(test_set_performance_xts) <- c('btc_return','position')
# 
# test_set_performance_xts$gross <- test_set_performance_xts$btc_return * test_set_performance_xts$position
# 
# test_set_performance_xts$turnover <- abs(test_set_performance_xts$position - xts::lag.xts(test_set_performance_xts$position, k =1))
# test_set_performance_xts$turnover <- test_set_performance_xts$turnover[is.na(turnover)] <- 0
# 
# 
# #transaction cost and performance tracking
# t_cost <- 0.001
# 
# test_set_performance_xts$net <- test_set_performance_xts$gross - test_set_performance_xts$turnover * t_cost
# 
# 
# charts.PerformanceSummary(test_set_performance_xts[,c(1,3,5)])
# table.AnnualizedReturns(test_set_performance_xts[,c(1,3,5)])
# maxDrawdown(test_set_performance_xts[,c(1,3,5)])
# SharpeRatio.annualized(test_set_performance_xts[,c(1,3,5)])


#-----------------------------------------------------------------------------------------------------------
#retraining selected model and generating trading performance
#svm
set.seed(123)

selected_model <- train(
  fwd_7d_return ~ ., 
  data = model_retraining_data,
  method = 'svmRadial',
  trControl = ctrl,
  metric = 'RMSE',
  tuneGrid = svmGrid,
  preProcess = c('center','scale')
)


selected_model_test_pred <- predict(selected_model, newdata = test_data)
selected_model_test_pred <- xts(selected_model_test_pred, order.by = index(test_data))

selected_model_test_position <- ifelse(selected_model_test_pred > 0, 1, -1)
selected_model_test_position <- xts(selected_model_test_position, order.by = index(test_data))

btc_return_test_period <- btc_xts["2024-01-01/", 'btc_return']


test_set_performance_xts <- merge.xts(btc_return_test_period, selected_model_test_position)

test_set_performance_xts <- na.omit(test_set_performance_xts)

colnames(test_set_performance_xts) <- c('btc_return','position')

test_set_performance_xts$gross <- test_set_performance_xts$btc_return * test_set_performance_xts$position

test_set_performance_xts$turnover <- abs(test_set_performance_xts$position - xts::lag.xts(test_set_performance_xts$position, k =1))
test_set_performance_xts$turnover[is.na(test_set_performance_xts$turnover)] <- 0


#transaction cost and performance tracking
t_cost <- 0.001

test_set_performance_xts$net <- test_set_performance_xts$gross - test_set_performance_xts$turnover * t_cost


charts.PerformanceSummary(test_set_performance_xts[,c(1,3,5)],main = 'Performance Comparison of SVM Model vs Buy and Hold Bitcoin')
table.AnnualizedReturns(test_set_performance_xts[,c(1,3,5)])
maxDrawdown(test_set_performance_xts[,c(1,3,5)])
SharpeRatio.annualized(test_set_performance_xts[,c(1,3,5)])


#-----------------------------------------------------------------------------------------------------------
#trend following - inverse vol SMA

#calculuate return for single SMA strategies
smas_long_short_close <- btc_xts[,c('close','sma_1w','sma_1m','sma_3m','sma_6m','sma_1y')]
smas_long_short_close$btc_return <- dailyReturn(smas_long_short_close$close,type = 'arithmetic')

smas_signals_list_close <- list()
smas_position_list_close <- list()
smas_gross_list_close <- list()
smas_turnover_list_close <- list()
smas_net_list_close <- list()

smas <- c(7,30,90,180,365)

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

#equal weighted SMAs
smas_equal_weight <- xts::merge.xts(
  smas_long_short_close[, "btc_return"],
  do.call(xts::merge.xts, smas_position_list_close))

smas_equal_weight$agg_position <- rowMeans(smas_equal_weight[,c(2:6)])
smas_equal_weight$gross <- smas_equal_weight$btc_return * smas_equal_weight$agg_position

smas_equal_weight$turnover <- abs(smas_equal_weight$agg_position - xts::lag.xts(smas_equal_weight$agg_position, k =1))
#turnover <- abs(position - xts::lag.xts(position, k = 1))
#smas_equal_weight$turnover <- smas_equal_weight$turnover[is.na(turnover)] <- 0

smas_equal_weight$net <- smas_equal_weight$gross - smas_equal_weight$turnover * t_cost

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


#-----------------------------------------------------------------------------------------------------------
#compare selected model to inverse vol weighted SMA approach

final_comparison_xts <- merge.xts(test_set_performance_xts, smas_inverse_vol[,c('agg_position','gross','net')],join = 'inner')
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "gross.1"] <- "gross_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "net.1"] <- "net_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "agg_position"] <- "position_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "net"] <- "net_ML_model"


charts.PerformanceSummary(final_comparison_xts[,c(1,3,5,7,8)])
table.AnnualizedReturns(final_comparison_xts[,c(1,3,5,7,8)])
maxDrawdown(final_comparison_xts[,c(1,3,5,7,8)])
SharpeRatio.annualized(final_comparison_xts[,c(1,3,5,7,8)])


#performance_data_frame
performance_df <- data.frame(
  strategy = character(),
  ann_return = double(),
  ann_vol = double(),
  sharpe = double(),
  max_dd = double()
)

return_cols <- c("btc_return", "net_ML_model", "net_sma_inv_vol")

performance_df <- do.call(
  rbind,
  lapply(return_cols, function(strategy_name) {
    
    ret_xts <- final_comparison_xts[, strategy_name, drop = FALSE]
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

performance_df



performance_df_latex <- performance_df %>%
  mutate(
    strategy   = gsub("_", "\\\\_", strategy),
    ann_return = sprintf("%.1f\\%%", 100 * as.numeric(ann_return)),
    ann_vol    = sprintf("%.1f\\%%", 100 * as.numeric(ann_vol)),
    sharpe     = sprintf("%.2f", as.numeric(sharpe)),
    max_dd     = sprintf("%.1f\\%%", 100 * as.numeric(max_dd))
  )

latex_table_performance <- knitr::kable(
  performance_df_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r", "r"),
  col.names = c("Strategy", "Ann. Return", "Ann. Vol", "Sharpe", "Max DD"),
  caption = "Performance comparison of selected strategies",
  label = "tab:performance_comparison",
  escape = FALSE
)

cat(latex_table_performance)

