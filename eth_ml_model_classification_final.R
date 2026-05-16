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
library(keras)
library(knitr)


url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/btc_eth_price_history.csv'
price_history <- read.csv(url)

#-----------------------------------------------------------------------------------------------------------
#convert dates to dttm
price_history$timestamp <- ymd_hms(price_history$timestamp)
price_history$time_close <- ymd_hms(price_history$time_close)
price_history$time_high <- ymd_hms(price_history$time_high)
price_history$time_low <- ymd_hms(price_history$time_low)

price_history$time_open <- ymd(price_history$time_open)

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

eth_xts$eth_return <-  dailyReturn(eth_xts$close,type = 'arithmetic')

#-----------------------------------------------------------------------------------------------------------
#add continuous features
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

#MACD
eth_xts$macd <- MACD(Cl(eth_xts))[,1]
eth_xts$macd_signal <- MACD(Cl(eth_xts))[,2]

#RSI
eth_xts$rsi <- RSI(Cl(eth_xts))

#ATR
eth_xts$atr <-ATR(eth_xts[,c('high','low','close')])[,2]

#Donchian
eth_xts$donch_high <- DonchianChannel(eth_xts[,c('high','low')],n = 20)[,1]
eth_xts$donch_mid <- DonchianChannel(eth_xts[,c('high','low')],n = 20)[,2]
eth_xts$donch_low <- DonchianChannel(eth_xts[,c('high','low')],n = 20)[,3]

#-----------------------------------------------------------------------------------------------------------

#add categorical features
#SMA
eth_xts$sma_1w_flag <- ifelse(eth_xts$close > eth_xts$sma_1w, 1, -1)
eth_xts$sma_1m_flag  <- ifelse(eth_xts$close > eth_xts$sma_1m, 1, -1)
eth_xts$sma_3m_flag  <- ifelse(eth_xts$close > eth_xts$sma_3m, 1, -1)
eth_xts$sma_6m_flag  <- ifelse(eth_xts$close > eth_xts$sma_6m, 1, -1)
eth_xts$sma_1y_flag  <- ifelse(eth_xts$close > eth_xts$sma_1y, 1, -1)

#EMA
eth_xts$ema_1w_flag <- ifelse(eth_xts$close > eth_xts$ema_1m, 1, -1)
eth_xts$ema_1m_flag  <- ifelse(eth_xts$close > eth_xts$ema_1m, 1, -1)
eth_xts$ema_3m_flag  <- ifelse(eth_xts$close > eth_xts$ema_3m, 1, -1)
eth_xts$ema_6m_flag  <- ifelse(eth_xts$close > eth_xts$ema_6m, 1, -1)
eth_xts$ema_1y_flag  <- ifelse(eth_xts$close > eth_xts$ema_1y, 1, -1)

#MACD Flag
eth_xts$macd_flag <- ifelse(eth_xts$macd > eth_xts$macd_signal, 1, -1)
eth_xts$macd_diff <- eth_xts$macd - eth_xts$macd_signal

#Donchian Flag
donch_high_lag <- lag.xts(eth_xts$donch_high, 1)
donch_low_lag  <- lag.xts(eth_xts$donch_low, 1)


eth_xts$donchian_flag <- ifelse(eth_xts$close > donch_high_lag, 1,
                                ifelse(eth_xts$close < donch_low_lag, -1, NA))

eth_xts$donchian_flag <- zoo::na.locf(eth_xts$donchian_flag, na.rm = FALSE)
#eth_xts$donchian_flag[is.na(eth_xts$donchian_flag)] <- 0

#-----------------------------------------------------------------------------------------------------------
#close vs feature as percent
#SMA
eth_xts$sma_1w_percentage <- eth_xts$close / eth_xts$sma_1w
eth_xts$sma_1m_percentage <- eth_xts$close / eth_xts$sma_1m
eth_xts$sma_3m_percentage <- eth_xts$close / eth_xts$sma_3m
eth_xts$sma_6m_percentage <- eth_xts$close / eth_xts$sma_6m
eth_xts$sma_1y_percentage <- eth_xts$close / eth_xts$sma_1y

#EMA
eth_xts$ema_1w_percentage <- eth_xts$close / eth_xts$ema_1w
eth_xts$ema_1m_percentage <- eth_xts$close / eth_xts$ema_1m
eth_xts$ema_3m_percentage <- eth_xts$close / eth_xts$ema_3m
eth_xts$ema_6m_percentage <- eth_xts$close / eth_xts$ema_6m
eth_xts$ema_1y_percentage <- eth_xts$close / eth_xts$ema_1y

#ATR
eth_xts$atr_percentage <- eth_xts$atr / eth_xts$close

#Donchian
eth_xts$donch_width <- (eth_xts$donch_high - eth_xts$donch_low)/eth_xts$close
eth_xts$donch_position <- (eth_xts$close - eth_xts$donch_low)/(eth_xts$donch_high - eth_xts$donch_low)

#-----------------------------------------------------------------------------------------------------------
#forward 7 day return 

eth_xts$fwd_7d_return <- (lag.xts(eth_xts$close, -7) / eth_xts$close) - 1

#positive or negative
eth_xts$fwd_7d_return_direction <- ifelse(eth_xts$fwd_7d_return > 0, 1, 0)




#-----------------------------------------------------------------------------------------------------------
#new xts for modelling - lags, features, etc
#train - test split
#train control - walk forward cross validation

#extract feature columns 


feature_cols <- c(
  "macd_flag", "macd_diff", "donchian_flag",
  "sma_1w_percentage", "sma_1m_percentage", "sma_3m_percentage",
  "sma_6m_percentage", "sma_1y_percentage",
  "ema_1w_percentage", "ema_1m_percentage", "ema_3m_percentage", "ema_6m_percentage",
  "rsi"
)


X <- lag.xts(eth_xts[,feature_cols],k=1)
y <- eth_xts$fwd_7d_return_direction

model_data <- merge.xts(X,y)

# remove NA rows
model_data <- na.omit(model_data)

#convert to dataframe
#for classification, need to convert to dataframe
#and convert direction to a factor

model_data_df <- data.frame(date = as.Date(index(model_data)), coredata(model_data))

model_data_df$fwd_7d_return_direction <- factor(
  model_data_df$fwd_7d_return_direction,
  levels = c(0, 1),
  labels = c("Neg", "Pos")
)

#-----------------------------------------------------------------------------------------------------------
#data exploration

#correlation
corrplot(cor(model_data),method = 'number', type='lower')



#-----------------------------------------------------------------------------------------------------------
#train - test split
#use 2016-2022 to perform training with walk forward CV
#2023 validation set used to predict with each model and evaluate best performer
#select model based on 2023 metrics, retrain that model with walk forward CV over 2016-2023
#compare performance to trend model for 2024-2025


train_data <- subset(model_data_df, date <= "2022-12-31")

validation_data <- subset(model_data_df,date >= "2023-01-01" & date <= "2023-12-31")

test_data <- subset(model_data_df, date >= "2024-01-01")

model_retraining_data <- subset(model_data_df, date <= "2023-12-31")


end_initial_train_period <- which(train_data$date == as.Date("2019-01-01")) - 1

ctrl <- trainControl(
  method = "timeslice",
  initialWindow = end_initial_train_period,
  horizon = 365,
  fixedWindow = FALSE,
  skip = 364,
  classProbs = TRUE
)

#validation set metrics
validation_performance <- data.frame(model=character(),
                                     accuracy=numeric(),
                                     no_info_rate = numeric())


#-----------------------------------------------------------------------------------------------------------
#linear model

model1 <- train(
  fwd_7d_return_direction ~ ., 
  data = train_data,
  method = 'glm',
  trControl = ctrl,
  metric = 'Accuracy',
  preProcess = c('center','scale')
)


#validation set performance
model1_pred <- predict(model1, newdata = validation_data)

model1_pred_metrics <- confusionMatrix(model1_pred, reference = validation_data$fwd_7d_return_direction,  
                                       positive = 'Pos')

validation_performance[nrow(validation_performance)+1,] <- c('linear_model',
                                                             round(model1_pred_metrics$overall[1],4),
                                                             round(model1_pred_metrics$overall[5],4))


#-----------------------------------------------------------------------------------------------------------
#gradient boosted trees 

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

model2 <- train(
  fwd_7d_return_direction ~ ., 
  data = train_data,
  method = 'xgbTree',
  trControl = ctrl,
  metric = 'Accuracy',
  tuneGrid = xgbGrid
)


#validation set performance
model2_pred <- predict(model2, newdata = validation_data)

model2_pred_metrics <- confusionMatrix(model2_pred, reference = validation_data$fwd_7d_return_direction,  
                                       positive = 'Pos')

validation_performance[nrow(validation_performance)+1,] <- c('xgbTree',
                                                             round(model2_pred_metrics$overall[1],4),
                                                             round(model2_pred_metrics$overall[5],4))




#-----------------------------------------------------------------------------------------------------------
#SVM

modelLookup('svmRadial')

#about two minutes to run
svmGrid <- expand.grid(
  sigma = c(0.01, 0.05),
  C = c(1, 10)
)

set.seed(123)

model3 <- train(
  fwd_7d_return_direction ~ ., 
  data = train_data,
  method = 'svmRadial',
  trControl = ctrl,
  metric = 'Accuracy',
  tuneGrid = svmGrid,
  preProcess = c('center','scale')
)


#validation set performance
model3_pred <- predict(model3, newdata = validation_data)

model3_pred_metrics <- confusionMatrix(model3_pred, reference = validation_data$fwd_7d_return_direction,  
                                       positive = 'Pos')

validation_performance[nrow(validation_performance)+1,] <- c('svmRadial',
                                                             round(model3_pred_metrics$overall[1],4),
                                                             round(model3_pred_metrics$overall[5],4))


validation_performance_latex <- validation_performance %>%
  mutate(
    model = gsub("_", "\\\\_", model),
    accuracy = sprintf("%.4f", as.numeric(accuracy)),
    no_info_rate = sprintf("%.4f", as.numeric(no_info_rate))
  )

latex_table <- knitr::kable(
  validation_performance_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r"),
  col.names = c("Model", "Accuracy", "No Information Rate"),
  caption = "Validation classification performance of machine learning models",
  label = "tab:validation_classification",
  escape = FALSE
)

cat(latex_table)



#-----------------------------------------------------------------------------------------------------------
#retraining selected model and generating trading performance
#glm
set.seed(123)

selected_model <- train(
  fwd_7d_return_direction ~ ., 
  data = model_retraining_data,
  method = 'glm',
  trControl = ctrl,
  metric = 'Accuracy',
  preProcess = c('center','scale')
)




selected_model_test_pred <- predict(selected_model, newdata = test_data)


selected_model_test_position <- ifelse(selected_model_test_pred == 'Pos', 1, -1)
selected_model_test_position <- xts(selected_model_test_position, order.by = test_data$date)

eth_return_test_period <- eth_xts["2024-01-01/", 'eth_return']
index(eth_return_test_period) <- as.Date(index(eth_return_test_period))



test_set_performance_xts <- merge.xts(eth_return_test_period, selected_model_test_position)

test_set_performance_xts <- na.omit(test_set_performance_xts)

test_set_performance_xts$gross <-  test_set_performance_xts$eth_return * test_set_performance_xts$selected_model_test_position


#transaction cost and performance tracking
t_cost <- 0.001

test_set_performance_xts$turnover <- abs(test_set_performance_xts$selected_model_test_position - xts::lag.xts(test_set_performance_xts$selected_model_test_position, k =1))

#replace NA in first row for turnover with the position
test_set_performance_xts$turnover[is.na(test_set_performance_xts$turnover)] <-
  abs(test_set_performance_xts$selected_model_test_position[is.na(test_set_performance_xts$turnover)])


test_set_performance_xts$net <- test_set_performance_xts$gross - test_set_performance_xts$turnover * t_cost

charts.PerformanceSummary(test_set_performance_xts[,c(1,3,5)],main = 'Performance Comparison of SVM Model vs Buy and Hold Ethereum')

table.AnnualizedReturns(test_set_performance_xts[,c(1,3,5)])
maxDrawdown(test_set_performance_xts[,c(1,3,5)])
SharpeRatio.annualized(test_set_performance_xts[,c(1,3,5)])


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


#-----------------------------------------------------------------------------------------------------------
#trend following - inverse vol SMA

#calculuate return for single SMA strategies
smas_long_short_close <- eth_xts[,c('close','sma_1w','sma_1m','sma_3m','sma_6m','sma_1y')]
smas_long_short_close$eth_return <- dailyReturn(smas_long_short_close$close,type = 'arithmetic')

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
  
  gross <- smas_long_short_close$eth_return * position
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
  smas_long_short_close[, "eth_return"],
  do.call(xts::merge.xts, smas_position_list_close))

smas_equal_weight$agg_position <- rowMeans(smas_equal_weight[,c(2:6)])
smas_equal_weight$gross <- smas_equal_weight$eth_return * smas_equal_weight$agg_position

smas_equal_weight$turnover <- abs(smas_equal_weight$agg_position - xts::lag.xts(smas_equal_weight$agg_position, k =1))

smas_equal_weight$net <- smas_equal_weight$gross - smas_equal_weight$turnover * t_cost

#inverse vol weighted SMAs
roll_period <- 60

smas_inverse_vol <- smas_equal_weight[,c(1:6)]

#calc return stream of each SMA signal
smas_inverse_vol$return_7 <- smas_inverse_vol$position_close_7 * smas_inverse_vol$eth_return
smas_inverse_vol$return_30 <- smas_inverse_vol$position_close_30 * smas_inverse_vol$eth_return
smas_inverse_vol$return_90 <- smas_inverse_vol$position_close_90 * smas_inverse_vol$eth_return
smas_inverse_vol$return_180 <- smas_inverse_vol$position_close_180 * smas_inverse_vol$eth_return
smas_inverse_vol$return_365 <- smas_inverse_vol$position_close_365 * smas_inverse_vol$eth_return

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
smas_inverse_vol$gross <- smas_inverse_vol$eth_return * smas_inverse_vol$agg_position

#turnover
smas_inverse_vol$turnover <- abs(smas_inverse_vol$agg_position - xts::lag.xts(smas_inverse_vol$agg_position, k =1))

#net
smas_inverse_vol$net <- smas_inverse_vol$gross - smas_inverse_vol$turnover * t_cost

#convert index to date
index(smas_inverse_vol) <- as_date(index(smas_inverse_vol))

#-----------------------------------------------------------------------------------------------------------
#compare selected model to inverse vol weighted SMA approach

final_comparison_xts <- merge.xts(test_set_performance_xts, smas_inverse_vol[,c('agg_position','gross','net')],join = 'inner')
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "gross.1"] <- "gross_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "net.1"] <- "net_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "agg_position"] <- "position_sma_inv_vol"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "gross"] <- "net_ML_classification_model"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "net"] <- "net_ML_classification_model"


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

return_cols <- c("eth_return", "net_ML_classification_model", "net_sma_inv_vol")

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
  caption = "Performance comparison of selected Ethereum strategies",
  label = "tab:performance_comparison",
  escape = FALSE
)

cat(latex_table_performance)


#





#-----------------------------------------------------------------------------------------------------------
#adjust based on probabilities 
#long, neutral, or short

selected_model_test_pred_prob <- predict(selected_model, newdata = test_data, type = 'prob')


selected_model_test_position_prob <- ifelse(selected_model_test_pred_prob$Pos > 0.58,  1, 
                                            ifelse(selected_model_test_pred_prob$Pos < 0.45, -1, 0))

selected_model_test_position_prob <- xts(selected_model_test_position_prob, order.by = test_data$date)

eth_return_test_period <- eth_xts["2024-01-01/", 'eth_return']
index(eth_return_test_period) <- as.Date(index(eth_return_test_period))



test_set_performance_xts_prob <- merge.xts(eth_return_test_period, selected_model_test_position_prob)

test_set_performance_xts_prob <- na.omit(test_set_performance_xts_prob)

test_set_performance_xts_prob$gross <-  test_set_performance_xts_prob$eth_return * test_set_performance_xts_prob$selected_model_test_position_prob


#transaction cost and performance tracking
t_cost <- 0.001

test_set_performance_xts_prob$turnover <- abs(test_set_performance_xts_prob$selected_model_test_position_prob - xts::lag.xts(test_set_performance_xts_prob$selected_model_test_position_prob, k =1))

#replace NA in first row for turnover with the position
test_set_performance_xts_prob$turnover[is.na(test_set_performance_xts_prob$turnover)] <-
  abs(test_set_performance_xts_prob$selected_model_test_position_prob[is.na(test_set_performance_xts_prob$turnover)])


test_set_performance_xts_prob$net <- test_set_performance_xts_prob$gross - test_set_performance_xts_prob$turnover * t_cost

charts.PerformanceSummary(test_set_performance_xts_prob[,c(1,3,5)],main = 'Performance Comparison of SVM Probability Model vs Buy and Hold Ethereum')

table.AnnualizedReturns(test_set_performance_xts_prob[,c(1,3,5)])
maxDrawdown(test_set_performance_xts_prob[,c(1,3,5)])
SharpeRatio.annualized(test_set_performance_xts_prob[,c(1,3,5)])


final_comparison_xts <- merge.xts(final_comparison_xts, test_set_performance_xts_prob[,c('selected_model_test_position_prob','gross','net')],join = 'inner')
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "gross"] <- "gross_ml_prob_model"
colnames(final_comparison_xts)[colnames(final_comparison_xts) == "net"] <- "net_ml_prob_model"



charts.PerformanceSummary(final_comparison_xts[,c(1,3,5,7,8,10,11)])
table.AnnualizedReturns(final_comparison_xts[,c(1,3,5,7,8,10,11)])
maxDrawdown(final_comparison_xts[,c(1,3,5,7,8,10,11)])
SharpeRatio.annualized(final_comparison_xts[,c(1,3,5,7,8,10,11)])



return_cols <- c("eth_return", "net_ML_classification_model", "net_sma_inv_vol","net_ml_prob_model")

performance_df2 <- do.call(
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

performance_df2



performance_df2_latex <- performance_df2 %>%
  mutate(
    strategy   = gsub("_", "\\\\_", strategy),
    ann_return = sprintf("%.1f\\%%", 100 * as.numeric(ann_return)),
    ann_vol    = sprintf("%.1f\\%%", 100 * as.numeric(ann_vol)),
    sharpe     = sprintf("%.2f", as.numeric(sharpe)),
    max_dd     = sprintf("%.1f\\%%", 100 * as.numeric(max_dd))
  )

latex_table_performance2 <- knitr::kable(
  performance_df2_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r", "r"),
  col.names = c("Strategy", "Ann. Return", "Ann. Vol", "Sharpe", "Max DD"),
  caption = "Performance comparison of selected strategies",
  label = "tab:performance_comparison",
  escape = FALSE
)

cat(latex_table_performance2)


#-----------------------------------------------------------------------------------------------------------
#export returns for trading cost and other analysis

eth_classification_and_trend_xts <- merge.xts(test_set_performance_xts, test_set_performance_xts_prob,join = 'inner')
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "selected_model_test_position"] <- "eth_ml_class_model_position"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "gross"] <- "eth_ml_class_model_gross"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "turnover"] <- "eth_ml_class_model_turnover"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "net"] <- "eth_ml_class_model_net_10bp"

colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "selected_model_test_position_prob"] <- "eth_ml_class_prob_model_position"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "gross.1"] <- "eth_ml_class_prob_model_gross"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "turnover.1"] <- "eth_ml_class_prob_model_turnover"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "net.1"] <- "eth_ml_class_prob_model_net_10bp"

eth_classification_and_trend_xts$eth_return.1 <- NULL

eth_classification_and_trend_xts <- merge.xts(eth_classification_and_trend_xts, 
                                              smas_inverse_vol[,c('agg_position','gross', 'turnover', 'net')],join = 'inner')

colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "agg_position"] <- "eth_sma_inv_vol_agg_position"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "gross"] <- "eth_sma_inv_vol_agg_gross"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "turnover"] <- "eth_sma_inv_vol_agg_turnover"
colnames(eth_classification_and_trend_xts)[colnames(eth_classification_and_trend_xts) == "net"] <- "eth_sma_inv_vol_agg_net"

write.zoo(eth_classification_and_trend_xts, file = "eth_classification_and_trend_xts.csv", sep = ",", row.names = FALSE)




