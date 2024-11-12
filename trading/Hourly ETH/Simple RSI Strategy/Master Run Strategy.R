
rm(list = ls())

########################################################################

source('Master Packages.R')
source(paste0(getwd(), "/trading/Hourly ETH/01 Load Data.R"))

title_var = "Simple RSI Strategy"
this_project_path = "trading/Hourly ETH"

########################################################################

      #################################
      ## PREPARE AND DEFINE STRATEGY ##
      #################################

## redefine contents of this function for all strategies
## each strategy requires new indicators --> requires new variables to be defined 
starting_data_prep <- function(data_in){
  N = 12 
  return(
    data_in %>% 
    mutate(rsi = TTR::RSI(close, n = N) 
           ) %>% 
    slice(N:nrow(.)) %>% 
    select(datetime, close, rsi)    
  )
}
## again, this will and should be different for each strategy 
trade_decision_fucntion <- function(day_data){
  RSI_LOW = 20 
  RSI_HIGH = 100 - RSI_LOW 
  
  return(
    case_when(day_data$rsi < RSI_LOW ~ "buy", 
              day_data$rsi > RSI_HIGH ~ "sell", 
              
              T ~ '')
    )
}

      ######################
      ## PREPARE DATASETS ##
      ######################
# this part shall remain the same for all strategies: 
# this is our default approach 
all_data_f <- function(N_synth_datasets){
  ##########################
  ## upward trending markets 
  upward_1 <- 
    df %>% 
    filter(datetime >= as.Date("2016-12-15") & 
             datetime <= as.Date("2018-01-15")) %>% 
    select(datetime, close)
  
  upward_2 <- 
    df %>% 
    filter(datetime >= as.Date("2018-12-10") & 
             datetime <= as.Date("2019-07-01")) %>% 
    select(datetime, close)
  
  upward_3 <- 
    df %>% 
    filter(datetime >= as.Date("2020-06-15") & 
             datetime <= as.Date("2021-05-10")) %>% 
    select(datetime, close)
  
  upward_4 <- 
    df %>% 
    filter(datetime >= as.Date("2022-11-15")& 
             datetime <= as.Date("2024-03-11") ) %>% 
    select(datetime, close)
  
  ## sample volatility 
  upward_vol <- 
    generate_volatility_distribution(
      sample_from_data = list(upward_1, 
                              upward_2, 
                              upward_3, 
                              upward_4
                              )
      )
  
  ## testing synthetic data 
  set.seed(125)
  upward_test_datasets <- 
    replicate(
      N_synth_datasets, 
      generate_generic_dateset(
      volatility_distr = upward_vol, 
      time_unit_length = "hour", 
      N_time_units = 24 * 263, # one year  
      starting_price = 1000,  
      start_date_time = as.POSIXct(x = "2100-01-01 00:00:00", tz = "America/Chicago")), 
      simplify = FALSE
      )
  
  names(upward_test_datasets) <- c(paste0("synth_up_", 1:length(upward_test_datasets)))
  ############################
  ## downward trending markets
  downward_1 <- 
    df %>% 
    filter(datetime >= as.Date("2018-01-15") & 
             datetime <= as.Date("2019-01-01") ) %>% 
    select(datetime, close)
  
  downward_2 <- 
    df %>% 
    filter(datetime >= as.Date("2021-11-01") & 
             datetime <= as.Date("2022-11-01") ) %>% 
    select(datetime, close)
  
  ##
  downward_vol <- 
    generate_volatility_distribution(
      sample_from_data = list(downward_1, 
                              downward_2)
      )
  ## testing synthetic data 
  set.seed(127)
  downward_test_datasets <- 
    replicate(
      N_synth_datasets, 
      generate_generic_dateset(
      volatility_distr = downward_vol, 
      time_unit_length = "hour", 
      N_time_units = 24 * 263, # one year  
      starting_price = 1000,  
      start_date_time = as.POSIXct(x = "2100-01-01 00:00:00", tz = "America/Chicago")), 
      simplify = FALSE
      )
  names(downward_test_datasets) <- c(paste0("synth_down_", 1:length(downward_test_datasets)))
  
  ##################
  # finalize resutls 
  observed <- list(upward_1, 
                   upward_2, 
                   upward_3, 
                   upward_4, 
                   
                   downward_1, 
                   downward_2)
  
  names(observed) <- c('upward_1', 
                   'upward_2', 
                   'upward_3', 
                   'upward_4', 
                   
                   'downward_1', 
                   'downward_2')
  
  synth <- c(upward_test_datasets, downward_test_datasets)
  all_data <- c(observed, synth)
  
  return(all_data)
  }
all_data <- all_data_f(N_synth_datasets = 100)

##############################
## RUN STRATEGY

## we have a list of N: one per each iteration(either observed or synthetic data)
##    within each N: 
##    store stats on the strategy 
##    store tracking of market and strategy roi over time 

N <- length(all_data)
master_results <- vector(mode = "list", length = N)
  # Fill each entry with two empty lists
for (i in seq_along(master_results)) {
  master_results[[i]] <- list(list(),
                              list(),
                              list()
                              )
  
  names(master_results)[i] <- names(all_data)[i]
  names(master_results[[i]]) <- c("strategy_stats", "time", "trades_stats")
}

for(i in 1:N){
  if(i == 1 | i %% 50 == 0){print(paste0(i, " of ", N))}
  
  hist <- starting_data_prep(data_in = all_data[[i]])
  
  d1 <- assign_decisions(
    data_in = hist, 
    ############# CHANGE DECISION FUCNTION ################
    ############# FOR EACH NEW STRATEGY    ################
    decision_function = trade_decision_fucntion, 
    ########################################################
    stop_loss = 2.5/100
    )
  
  d2 <- evaluate_strat(data_in = d1, budget = 1000, flat_buy_percent = 0.05, dynamic = T)
  
  master_results[[i]][["time"]] <- d2
  master_results[[i]][["strategy_stats"]] <- stats_strategy(d2)
  master_results[[i]][["trades_stats"]] <- trade_level_stats(d1)
}

#################################################

write_rds(x=master_results, 
          file = paste0("./",this_project_path ,"/" , title_var, "/results_of_strat.rds")
          )

source(
  paste0(getwd(), "/",this_project_path, "/", title_var, "/Quantro Results Summary.R")
  )

###### 
# remove by products of rendering quatro document 

# TURNS OUT that removing these folder completely destroys HTML file presentation. 
# so, they must be kept here 

# remove_folder <- paste0( title_var, " Summary_files")
# remove_fodler2 <- gsub(pattern = " ", x = remove_folder, replacement = "-")
# 
# unlink(
#   paste0("./", this_project_path, "/",title_var,"/", remove_folder)
#   , recursive = T
# )
# 
# unlink(
#   paste0("./", this_project_path, "/",title_var,"/", remove_fodler2)
#   , recursive = T
# )

unlink(
  paste0("./", this_project_path, "/",title_var,"/", title_var , " Summary.qmd")
  , recursive = T
)
