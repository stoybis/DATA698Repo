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
library(PeerPerformance)
library(knitr)
library(kableExtra)
library(readr)

#-----------------------------------------------------------------------------------------------------------
#read in data


btc_class_url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/btc_classification_and_trend_xts.csv'
btc_reg_url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/btc_regression_and_trend_xts.csv'
eth_class_url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/eth_classification_and_trend_xts.csv'
eth_reg_url <- 'https://raw.githubusercontent.com/stoybis/DATA698Repo/refs/heads/main/eth_regression_and_trend_xts.csv'

btc_classification_portfolios <- read.csv(btc_class_url)
btc_regression_portfolios <- read.csv(btc_reg_url)

eth_classification_portfolios <- read.csv(eth_class_url)
eth_regression_portfolios <- read.csv(eth_reg_url)

#convert dates to same format
btc_classification_portfolios$Index <- as.Date(btc_classification_portfolios$Index)
btc_regression_portfolios$Index <- as.Date(btc_regression_portfolios$Index)

eth_classification_portfolios$Index <- as.Date(eth_classification_portfolios$Index)
eth_regression_portfolios$Index <- as.Date(eth_regression_portfolios$Index)

#convert to xts for analysis with finance packages
btc_classification_portfolios <- as.xts(btc_classification_portfolios)
btc_regression_portfolios <- as.xts(btc_regression_portfolios)

eth_classification_portfolios <- as.xts(eth_classification_portfolios)
eth_regression_portfolios <- as.xts(eth_regression_portfolios)

#-----------------------------------------------------------------------------------------------------------
#data already include net performance assuming 10bps transaction fee
#need to calculate performance net of 25bps and 50bps

additional_t_costs <- c(0.0025, 0.005)

#  net <- gross - turnover * t_cost
#need to calculate for classification model, classification_prob model, sma_inv_vol model
#regression model and sma_inv_vol model



portfolio_list <- list(
  btc_classification_portfolios = btc_classification_portfolios,
  btc_regression_portfolios     = btc_regression_portfolios,
  eth_classification_portfolios = eth_classification_portfolios,
  eth_regression_portfolios = eth_regression_portfolios
)

for (portfolio_name in names(portfolio_list)) {
  
  portfolio_xts <- portfolio_list[[portfolio_name]]
  
  gross_cols <- grep("_gross$", colnames(portfolio_xts), value = TRUE)
  
  for (gross_col in gross_cols) {
    
    base_name <- sub("_gross$", "", gross_col)
    turnover_col <- paste0(base_name, "_turnover")
    
    if (!turnover_col %in% colnames(portfolio_xts)) {
      next
    }
    
    for (t_cost in additional_t_costs) {
      
      bp_label <- paste0(t_cost * 10000, "bp")
      net_col <- paste0(base_name, "_net_", bp_label)
      
      new_net_return <- portfolio_xts[, gross_col] - portfolio_xts[, turnover_col] * t_cost
      
      colnames(new_net_return) <- net_col
      
      portfolio_xts <- merge(portfolio_xts, new_net_return)
    }
  }
  
  portfolio_list[[portfolio_name]] <- portfolio_xts
}

btc_classification_portfolios <- portfolio_list$btc_classification_portfolios
btc_regression_portfolios <- portfolio_list$btc_regression_portfolios
eth_classification_portfolios <- portfolio_list$eth_classification_portfolios
eth_regression_portfolios <- portfolio_list$eth_regression_portfolios


sum(btc_classification_portfolios$ml_class_prob_model_net_25bp==(btc_classification_portfolios$ml_class_prob_model_gross - btc_classification_portfolios$ml_class_model_turnover * 0.0025))

#-----------------------------------------------------------------------------------------------------------
#net returns analysis

net_return_portfolios <- btc_classification_portfolios[, grep("net", names(btc_classification_portfolios))]

net_return_portfolios <- merge(net_return_portfolios, btc_regression_portfolios[, grep("net", names(btc_regression_portfolios))])

colnames(net_return_portfolios)[1:9] <- paste0("btc_", colnames(net_return_portfolios)[1:9]) 


net_return_portfolios <- merge(net_return_portfolios, eth_classification_portfolios[, grep("net", names(eth_classification_portfolios))])

net_return_portfolios <- merge(net_return_portfolios, eth_regression_portfolios[, grep("net", names(eth_regression_portfolios))])

net_return_portfolios$eth_sma_inv_vol_agg_net.1 <- NULL
net_return_portfolios$eth_sma_inv_vol_agg_net_25bp.1 <- NULL
net_return_portfolios$eth_sma_inv_vol_agg_net_50bp.1 <- NULL
net_return_portfolios$btc_sma_inv_vol_agg_net.1 <- NULL
net_return_portfolios$btc_sma_inv_vol_agg_net_25bp.1 <- NULL
net_return_portfolios$btc_sma_inv_vol_agg_net_50bp.1 <- NULL

colnames(net_return_portfolios)[colnames(net_return_portfolios) == "btc_sma_inv_vol_agg_net"] <- "btc_sma_inv_vol_agg_net_10bp"
colnames(net_return_portfolios)[colnames(net_return_portfolios) == "eth_sma_inv_vol_agg_net"] <- "eth_sma_inv_vol_agg_net_10bp"

net_return_portfolios_sharpe <- data.frame(t(SharpeRatio.annualized(net_return_portfolios)))
net_return_portfolios_sharpe <- rownames_to_column(net_return_portfolios_sharpe)
colnames(net_return_portfolios_sharpe) <- c('Strategy','Sharpe')

net_return_portfolios_sharpe$Transaction_cost <- str_sub(net_return_portfolios_sharpe$Strategy, -4,-1)
net_return_portfolios_sharpe$Transaction_cost <- as.factor(net_return_portfolios_sharpe$Transaction_cost)
net_return_portfolios_sharpe$Strategy <- str_sub(net_return_portfolios_sharpe$Strategy, end=-6)
net_return_portfolios_sharpe$Asset <- toupper(str_sub(net_return_portfolios_sharpe$Strategy, end=3))

net_return_portfolios_sharpe$Strategy <- str_sub(net_return_portfolios_sharpe$Strategy, start =5)
net_return_portfolios_sharpe$Strategy <- str_sub(net_return_portfolios_sharpe$Strategy, end =-5)


net_return_portfolios_sharpe |>
  mutate(Transaction_cost = factor(
      Transaction_cost,
      levels = c("10bp", "25bp", "50bp"))) |>
  ggplot(aes(x = Transaction_cost, y = Sharpe,
      group = Strategy, color = Strategy)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0,
    linetype = "solid") +
  facet_wrap(~Asset)+ labs(x = "Transaction Cost", y = "Sharpe Ratio", color = "Strategy") +
  theme(legend.position = "bottom") + theme_classic() +
  ggtitle('Sharpe Ratios of Different Strategies Across Transaction Cost Levels')
 
#-----------------------------------------------------------------------------------------------------------
#performance charts
net_returns_perf_charts <- net_return_portfolios
net_returns_perf_charts <- merge(net_returns_perf_charts, btc_regression_portfolios$btc_return)
net_returns_perf_charts <- merge(net_returns_perf_charts, eth_regression_portfolios$eth_return)         

#BTC
btc_net_returns_perf_charts <- net_returns_perf_charts[,c(25,1,2,3,10)]

btc_net_returns_perf_charts$sma_inv_vol_scaled <- sd(btc_net_returns_perf_charts$btc_return)/sd(btc_net_returns_perf_charts$btc_sma_inv_vol_agg_net_10bp) * btc_net_returns_perf_charts$btc_sma_inv_vol_agg_net_10bp


chart.CumReturns(btc_net_returns_perf_charts[,c(1,2,3,4,5,6)],legend.loc = 'topleft',
                 main = 'Performance of Bitcoin strategies during test period')

#ETH
eth_net_returns_perf_charts <- net_returns_perf_charts[,c(26,13,14,15,22)]

eth_net_returns_perf_charts$sma_inv_vol_scaled <- sd(eth_net_returns_perf_charts$eth_return)/sd(eth_net_returns_perf_charts$eth_sma_inv_vol_agg_net_10bp) * eth_net_returns_perf_charts$eth_sma_inv_vol_agg_net_10bp


chart.CumReturns(eth_net_returns_perf_charts[,c(1,2,3,4,5,6)], legend.loc = 'bottomleft',
                 main = 'Performance of Ethereum strategies during test period')

#-----------------------------------------------------------------------------------------------------------
#Sharpe ratio tests

btc_reg_v_trend <- sharpeTesting(net_returns_perf_charts$btc_ml_model_net_10bp, net_returns_perf_charts$btc_sma_inv_vol_agg_net_10bp, 
                                control = list(type = 2, nBoot = 499))

btc_class_v_trend <- sharpeTesting(net_returns_perf_charts$btc_ml_class_model_net_10bp, net_returns_perf_charts$btc_sma_inv_vol_agg_net_10bp, 
                                   control = list(type = 2, nBoot = 499))

btc_class_prob_v_trend <- sharpeTesting(net_returns_perf_charts$btc_ml_class_prob_model_net_10bp, net_returns_perf_charts$btc_sma_inv_vol_agg_net_10bp, 
                                   control = list(type = 2, nBoot = 499))

eth_reg_v_trend <- sharpeTesting(net_returns_perf_charts$eth_ml_model_net_10bp, net_returns_perf_charts$eth_sma_inv_vol_agg_net_10bp, 
                                 control = list(type = 2, nBoot = 499))

eth_class_v_trend <- sharpeTesting(net_returns_perf_charts$eth_ml_class_model_net_10bp, net_returns_perf_charts$eth_sma_inv_vol_agg_net_10bp, 
                                 control = list(type = 2, nBoot = 499))

eth_class_prob_v_trend <- sharpeTesting(net_returns_perf_charts$eth_ml_class_prob_model_net_10bp, net_returns_perf_charts$eth_sma_inv_vol_agg_net_10bp, 
                                   control = list(type = 2, nBoot = 499))




sharpe_test_summary <- data.frame(
  Comparison = character(),
  Sharpe_Difference = numeric(),
  Test_Statistic = numeric(),
  p_value = numeric()
)

sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('BTC ML Regression v Trend',
                                                             btc_reg_v_trend[[3]],
                                                             btc_reg_v_trend[[4]],
                                                             btc_reg_v_trend[[5]])

sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('BTC ML Classification v Trend',
                                                             btc_class_v_trend[[3]],
                                                             btc_class_v_trend[[4]],
                                                             btc_class_v_trend[[5]])

sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('BTC ML Classification Prob v Trend',
                                                             btc_class_prob_v_trend[[3]],
                                                             btc_class_prob_v_trend[[4]],
                                                             btc_class_prob_v_trend[[5]])


sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('ETH ML Regression v Trend',
                                                             eth_reg_v_trend[[3]],
                                                             eth_reg_v_trend[[4]],
                                                             eth_reg_v_trend[[5]])


sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('ETH ML Classification v Trend',
                                                             eth_class_v_trend[[3]],
                                                             eth_class_v_trend[[4]],
                                                             eth_class_v_trend[[5]])

sharpe_test_summary[nrow(sharpe_test_summary) + 1, ] <- list('ETH ML Classification Prob v Trend',
                                                             eth_class_prob_v_trend[[3]],
                                                             eth_class_prob_v_trend[[4]],
                                                             eth_class_prob_v_trend[[5]])




sharpe_test_summary_latex <- sharpe_test_summary |>
  mutate(Comparison = gsub("_", "\\\\_", Comparison),
    Sharpe_Difference = sprintf("%.3f", as.numeric(Sharpe_Difference)),
    Test_Statistic = sprintf("%.2f", as.numeric(Test_Statistic)),
    p_value = sprintf("%.3f", as.numeric(p_value)))


latex_table <- knitr::kable(
  sharpe_test_summary_latex,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "r", "r", "r"),
  col.names = c(
    "Comparison",
    "Sharpe Difference",
    "Test Statistic",
    "P-Value"),
  caption = "Ledoit--Wolf Sharpe ratio difference tests",
  label = "tab:sharpe_ratio_tests",
  escape = FALSE
)

cat(latex_table)
