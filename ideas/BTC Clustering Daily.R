
source("Master Packages.R")


if(file.exists('keys.R') == T){rm(list = ls())}

if(file.exists('keys.R') == T){
  source("keys.R")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key, 
             "AWS_DEFAULT_REGION" =  aws_region)
  
  print("Connected to AWS using local keys")
}

##############
# Older function to make data 


estimate_slope <- function(DATA, DAYS){
    
    DATA$coeff_p <- NA
    
    for(i in DAYS:nrow(DATA)){
    coeff_p <- round(summary(lm(log(price) ~ n, data = DATA[i:(i-DAYS), ]))$coefficients[2,1] * 100,2)
    
    DATA$coeff_p[i] <- coeff_p
    }
    return(DATA)
  }

modify_raw_data <- function(DATA, ASSET_NAME){
  
  n_obs <- nrow(DATA)
  
  DATA$n <- seq(from = 1, to = n_obs, by = 1)
 
  #calculate new ATH based on closing price. This is not an entirely accurate method becuase 
  # ATH != closing price, but 
  # a) most data viz. are done with closing price, so we need to be consistent there
  # b) ATH ~ closing price for days where ATH is established, so closing price approximates ATH 
  
  
  DATA$new_ath_flag <- 0
  
  for( i in 2:nrow(DATA)){
    DATA$new_ath_flag[i] <- ifelse(DATA$price[i] > max(DATA$price[1:(i-1)]), 1, 0)
  }
  
  # record most recent ATH based on closing price 
  DATA$recent_ath <- min(DATA$price)
  
  for(i in 2:nrow(DATA)){
    DATA$recent_ath[i] <- max(DATA$price[1:i])
  }
  
  #calculate % that the current price is down from the most recent ATH 
  DATA$p_down_from_ATH <- with(DATA, price/recent_ath - 1)
  
  #get 20 week MA
  DATA$week_ma_20<- frollmean(DATA$price, 20 * 7)
  # get extension fo price from the 20 week moving average, we use this as one of the detection tools 
  DATA$price_ma_ratio <- with(DATA, price / week_ma_20)
  #get rate of change of the 20 week moving average. Essentially, this is volatility of 20 week moving average 
  DATA$ma_roc <- with(DATA, week_ma_20 / lag(week_ma_20))
  
  # get ATH of a 20 week moving average 
  DATA$new_ath_ma <- 0
    
  for( i in 2:nrow(DATA)){
      DATA$new_ath_ma[i] <- ifelse(DATA$week_ma_20[i] > 
                                         max(DATA$week_ma_20[1:(i-1)], na.rm = T), 1, DATA$new_ath_ma[i])
    }
  
  #store asset name 
  DATA$asset <- ASSET_NAME
  
  #volatility
  DATA$volatility <- NA
  DATA$volatility <- with(DATA, price/lag(price))
  DATA$volatility[1] <- 1
  
  ## define transformed volatility: basically in terms of % change 
  DATA$transformed_volatility <- DATA$volatility - 1
  
  DATA$volatility_type <- 
    case_when(
      sign(DATA$transformed_volatility) == 1 ~ "Positive volatility", 
      sign(DATA$transformed_volatility) == 0 ~ "Zero volatility",
      TRUE ~ "Negative volatility"
    )
  #calculate consequtive datys of the same direction volatility 
   ### consequtive days: consequtive day number that volatity is the same direction: positive or negative
      # basically would like to see how many days in a row can volatility go up and down 
      
      DATA$consequtive_days <- 1
      
      #############
      # NOTE: 
      # "sign" is a bad function here because it doesn't work with 0, so it needs to be fixed. 
      
      for(i in 2:nrow(DATA)){
      
        # if volatility is positive or negative both today and yesterday add one day to consequtive streak, 
          # othewise set streak to 1 
        DATA$consequtive_days[i] <- 
          ifelse(
            DATA$volatility_type[i] ==  
              DATA$volatility_type[i-1] , DATA$consequtive_days[i-1] + 1, 1
          )
      }
      
      ## assign ID number to each volatility sequesnce. Easier to identify on the plot 
      DATA$seq_id <- 1
          
          for(i in 2:nrow(DATA)){
            
            ## if previous day volatility is the same sign, i.e. also positive, negative, or zero
            # then keep the same sequence id
            # otherwise add 1 
            
            DATA$seq_id[i] <- 
              ifelse(
                DATA$volatility_type[i] ==  
                    DATA$volatility_type[i-1] ,
                
                DATA$seq_id[i-1],
                DATA$seq_id[i-1] + 1
              )
          }

   DATA$rsi <- RSI(DATA$price, days = 14)

   DATA <- estimate_slope(DATA, DAYS = 15)
   
  ## RETURN FINAL DATA SET 
  return(DATA)
}

## risk metric for BITCOIN

BTC_risk_metric <- 
  function(DATA, 
           DAY_MA,
            POWER,
            AVG_VOLATILITY_TIMEFRAME,
            Y2_f, 
            Y1_f, 
            X2_f, 
            X1_f,
            
            Y2_f_l,
            Y1_f_l,
            X2_f_l,
            X1_f_l,
        
            POWER_TR
           ){
    
    DATA$week_ma_50 <- frollmean(DATA$price, DAY_MA * 7)
    
    #first row will have its own price as MA 
    DATA[1,]$week_ma_50 <- DATA[1,]$price
    
    max_n <- max(DATA[is.na(DATA$week_ma_50), ]$n)
    
    for(i in 2:max_n){
      DATA[i,]$week_ma_50 <- frollmean(DATA[1:(i),]$price, n = i)[i]
    }
    
    ##STEP 1: ratio of price to Week MA 
    DATA$price_to_50_week_MA <- with(DATA, price / week_ma_50)
    
    ##STEP 2: add time components 
    DATA$price_to_50_week_MA_w_time <- with(DATA, price_to_50_week_MA * (n^(1/POWER)))
    
    ##STEP 3: roling volatility and squishing raw risk metric 
        
    DATA$volatility_roll <- frollmean(DATA$volatility, AVG_VOLATILITY_TIMEFRAME)
    
    max_n <- max(DATA[is.na(DATA$volatility_roll), ]$n)
    DATA$volatility_roll[1] <- 1 
    
    for(i in 2:max_n){
      DATA[i,]$volatility_roll <- frollmean(DATA[1:(i),]$volatility, n = i)[i]
    }
    DATA$volatility_roll[1] <- 1 
        
    DATA$price_to_50_week_MA_w_time_w_vol <- 
      with(DATA, price_to_50_week_MA_w_time * volatility_roll)
    
    DATA$price_to_50_week_MA_w_time_w_vol <- 
      sqrt(DATA$price_to_50_week_MA_w_time_w_vol) ^ POWER_TR
    
    ## RAW RISK DIMINISHING SLOPE
     # slope <- (14.359433 - 15.297704) / (2491 - 1024)
     
     slope <- (Y2_f - Y1_f) / (X2_f - X1_f)

    #### intercept = y - slope * x 
    # intercept <- 14.65 - slope * 1024
    
    intercept <- Y1_f - slope * X1_f
    
    ####add slope to the entire data set 
  #  a <- 0.1
    # DATA$raw_risk_slope_a <- DATA$n * (slope - (a/1000)) + (intercept - 0.25 + 3*a)
    
    DATA$raw_risk_slope_a <-  DATA$n * slope + intercept

    DATA$price_to_50_week_MA_w_time_w_vol <- 
      with(DATA, 
           case_when(
             price_to_50_week_MA_w_time_w_vol >= raw_risk_slope_a ~ raw_risk_slope_a,
             TRUE ~ price_to_50_week_MA_w_time_w_vol
           ))
    
    #DATA$scaled_risk <- DATA$price_to_50_week_MA_w_time_w_vol/DATA$raw_risk_slope_a
    ## finally, calculate scaled risk 
    
    low_data_x <- data.frame(
      n = c(X1_f_l,X2_f_l), 
      y = c(Y1_f_l, Y2_f_l)
    )
    
    model <- lm(y ~ sqrt(n), data = low_data_x)
    
    DATA$raw_lower_slope_a <- predict(model, DATA)
    
    high_data_x <- data.frame(
      n = c(X1_f,X2_f), 
      y = c(Y1_f, Y2_f)
    )
    
    model <- lm(y ~ sqrt(n), data = high_data_x)
    
    DATA$raw_risk_slope_a <- predict(model, DATA)

    DATA$scale_risk_1 <- 
      with(DATA, 
           (price_to_50_week_MA_w_time_w_vol-raw_lower_slope_a)/(raw_risk_slope_a-raw_lower_slope_a)
           )
    
    DATA$scaled_risk <- 
      with(DATA,
           (scale_risk_1 - min(scale_risk_1))/
             (max(scale_risk_1) - min(scale_risk_1))
           )
    
    DATA <- DATA %>% select(-scale_risk_1, -raw_lower_slope_a)
    
    # DATA <- DATA %>% select(-raw_risk_slope_a, -price_to_50_week_MA_w_time_w_vol, -n, 
    #                           - volatility_roll, -week_ma, -price_to_50_week_MA, -price_to_50_week_MA_w_time)
    ##FINISHED: Store data 
    return(DATA)
    
  }

###############
# Collect Data# 
###############

#BTC
{
  tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
  save_object(object = "s3://crypto-data-shiny/all_available_BTC.csv", file = tempfile)
  all_data <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)

  all_data_EDA <- all_data %>% filter(
                                        # !(datetime >= as.Date("2014-02-13") & datetime <= as.Date("2014-02-25") ) & 
                                          # datetime >= as.Date("2010-10-26"))
                                          datetime >= as.Date("2010-12-01"))
  
  all_data_EDA <- modify_raw_data(DATA = all_data_EDA, ASSET_NAME = "BTC/USD")

  all_data_EDA <- BTC_risk_metric(DATA = all_data_EDA, 
                                    DAY_MA = 50, 
                                    POWER = 2, 
                                    AVG_VOLATILITY_TIMEFRAME = 30,
                                  
                                    Y2_f = 16.7, 
                                    Y1_f = 18.5, 
                                    X2_f = 2634, 
                                    X1_f = 1156,
                                    POWER_TR = 1,
                                  
                                    Y2_f_l = 3,
                                    Y1_f_l = 2,
                                    X2_f_l = 1555,
                                    X1_f_l = 414)
    
  all_data_EDA$market_stage <-
    as.factor(with(all_data_EDA,

                   case_when(
                     datetime < as.Date("2011-01-05") |
                       (datetime >= as.Date("2011-11-02") & datetime <= as.Date("2013-01-04")) |
                       (datetime >= as.Date("2015-01-02") & datetime <= as.Date("2016-08-14")) |
                       (datetime >= as.Date("2019-01-15") & datetime <= as.Date("2019-05-15")) |
                       (datetime >= as.Date("2019-07-16") & datetime <= as.Date("2020-09-30")) ~ "Accumulation",

                     (datetime >= as.Date("2011-01-05") & datetime <= as.Date("2011-06-15")) |
                       (datetime >= as.Date("2013-01-04") & datetime <= as.Date("2014-02-01")) |
                       (datetime >= as.Date("2016-08-15") & datetime <= as.Date("2018-01-15")) |
                       (datetime >= as.Date("2019-01-15") & datetime <= as.Date("2019-05-15")) |
                       (datetime >= as.Date("2019-05-16") & datetime <= as.Date("2019-07-15")) ~ "Bull",

                     datetime >= as.Date("2020-10-01") ~ "Current and Unknown",
                     TRUE ~ "Bear"
                   )
     ))
  
  all_data_EDA$ms <-
  with(all_data_EDA,
       case_when(
         datetime <= as.Date('2011-11-18') ~ 1,
         datetime >= as.Date('2011-11-19') & datetime <= as.Date('2015-01-14') ~ 2,
         datetime >= as.Date('2015-01-15') & datetime <= as.Date('2018-12-15') ~ 3,
         datetime >= as.Date('2018-12-16') ~ 4
       ))
    # to define market stages we need to find the date of the first and the last date of each market cycle:
  ms_dates <-
    all_data_EDA %>%
    group_by(ms) %>%
    summarise(first_ms_date = min(datetime),
              last_ms_date = max(datetime) )

  # to define market stages we need to find the date of the first and the last date of ATH's in each market cycle:
  ms_ath_dates <-
    all_data_EDA %>%
    filter(new_ath_flag == 1) %>%
    group_by(ms) %>%
    summarise(first_ath_date = min(datetime),
              last_ath_date = max(datetime),
              first_ath = min(price),
              last_ath = max(price))

  # join these dates to the data set and define market stages
  all_data_EDA <- merge(all_data_EDA, ms_dates, by = "ms")
  all_data_EDA <- merge(all_data_EDA, ms_ath_dates %>% select(ms, first_ath_date, last_ath_date), by = "ms")

  # define market stages now
  # define market stages now

  all_data_EDA$market_stage <- "str"

  all_data_EDA[all_data_EDA$ms %in% c(1,2,3), ]$market_stage <-
    with(all_data_EDA[all_data_EDA$ms %in% c(1,2,3), ],
         case_when(
           datetime >= first_ms_date & datetime < first_ath_date ~ "Accumulation",
           datetime >= first_ath_date & datetime <= last_ath_date ~ "Bull market",
           datetime > last_ath_date & datetime <= last_ms_date ~ "Bear market"
         ))
   
  all_data_EDA[all_data_EDA$ms == 4, ]$market_stage <-
    with(all_data_EDA[all_data_EDA$ms == 4, ],
         case_when(
           datetime >= first_ms_date & datetime < first_ath_date ~ "Accumulation",
           datetime >= first_ath_date & datetime <= last_ms_date ~ "Bull market"
         )
         )

  all_data_EDA[all_data_EDA$ms == 1 & all_data_EDA$market_stage == "Accumulation", ]$market_stage <- "Bull market"
  
  all_data_EDA <- all_data_EDA %>% 
    select(-first_ath_date, -first_ms_date, -last_ath_date, -last_ms_date, -raw_risk_slope_a, -week_ma_50, -ms)
  
  all_data_EDA$week_ma_20 <- frollmean(all_data_EDA$price, 140)
  all_data_EDA$ratio_to_20_week_ma <- all_data_EDA$price / all_data_EDA$week_ma_20
  
  all_data_EDA$datetime <- as.Date(all_data_EDA$datetime, format="%Y-%m-%d")
  
  ## these data set are used to finalize ALT coins data sets 
  btc_20_week_flag <-
    all_data_EDA %>%

    select(datetime, price, week_ma_20) %>%

    mutate(
      market_stage = case_when(
        price > week_ma_20~ "BTC > 20 week MA",
        TRUE ~ "BTC < 20 week MA"
      )) %>% drop_na() %>% select(-price, - week_ma_20)

  btc_prices <-
    all_data_EDA %>%
    select(datetime, price) %>%
    rename(btc_price = price)
  
  
}


working_data <- all_data_EDA

list_ma <- c(7, 20, 50, 100)
list_rsi <- c(14, 50, 100)

for(i in 1:length(list_ma)){
  
  ma_name <- paste0("ma_", list_ma[i],"_ratio")
  
  working_data <- 
    working_data %>% 
    
    mutate(
      !!ma_name :=  price/frollmean(price, n = 7* list_ma[i])
    )
}

for(i in 1:length(list_rsi)){
  
  rsi_name <- paste0("rsi_", list_rsi[i])
  
  working_data <- 
    working_data %>% 
    
    mutate(
      !!rsi_name :=  RSI(price, n = list_rsi[i])
    )
}

View(head(working_data))

############################################
# Select features that we use for clustering

selecting <- 
  c("datetime", "price", "scaled_risk", 
    paste0("ma_", list_ma,"_ratio"),
    paste0("rsi_", list_rsi))


cluster_df <- 
  working_data %>% 
  select(all_of(selecting)) %>% na.omit()

cor(cluster_df %>% select(-datetime, -price) %>% scale())

cor(cluster_df %>% select(-datetime, -price) %>% scale()) %>% as.vector() %>% 
  unique() %>% summary()

cor(cluster_df %>% select(-datetime, -price) %>% scale()) %>% as.vector() %>% 
  unique() %>% hist()

# Scale and center the data
scaled_data <- scale(cluster_df %>% select(-datetime, -price))

# Function to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(scaled_data, k, iter.max = 1000)$tot.withinss
}

# Compute and plot within-cluster sum of squares for different number of clusters
k_values <- 3:20
wss_values <- map_dbl(k_values, wss)

# Create a data frame for plotting
elbow_df <- tibble(
  k = k_values,
  wss = wss_values
)

# Elbow plot
optimal_k <- 10

ggplot(elbow_df, aes(x = k, y = wss)) +
  theme_classic() + 
  geom_line() +
  geom_point() +
  ggtitle("Elbow Plot to Determine Optimal Number of Clusters") +
  xlab("Number of Clusters (k)") +
  ylab("Total Within-Cluster Sum of Squares") +
  geom_vline(xintercept = optimal_k, color = "red")
  

# Determine the optimal number of clusters (e.g., k = 3 from the elbow plot)

# Perform K-means clustering with the optimal number of clusters
kmeans_result <- kmeans(scaled_data, centers = optimal_k, iter.max = 1000)

# Add cluster results to the original data
clustered_data <- cluster_df %>% 
  mutate(Cluster = kmeans_result$cluster)

clust = optimal_k

ggplot(data = clustered_data, 
       aes(x = datetime, y = price)) + 
  theme_classic() + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(data = clustered_data %>% filter(Cluster == clust), aes(x = datetime, y = price, color = as.factor(Cluster) )) + 
  scale_y_continuous(transform = 'log10') + 
  theme(legend.position = "bottom")

#######
# For points within a cluster, define forecasting period 
# this will also be the clean period 

clusters = clustered_data$Cluster %>% unique() %>% sort()

ns <- means <- meds <- sds <- ps <- mean_pos <- c()

clean_forecast_period <- 15
clean_forecast_period <- clean_forecast_period + 1 
pred_period <- clean_forecast_period

####################################################################################
####################################################################################

for(i in 1:length(clusters)){
  
  print(i)
  
  clustered_data_cluster_filt = clustered_data %>% filter(Cluster == i)
  
  clustered_data_cluster_filt$seq_id = NA
  clustered_data_cluster_filt$seq_id[1] <- 1
  
  for(i in 2:nrow(clustered_data_cluster_filt)){
    
    as.numeric(
      clustered_data_cluster_filt$datetime[i] - 
      clustered_data_cluster_filt$datetime[i-1]) -> date_diff_to_anchor
    
    clustered_data_cluster_filt$seq_id[i] <- ifelse(date_diff_to_anchor > clean_forecast_period, 
                                                    clustered_data_cluster_filt$seq_id[i-1]+1, 
                                                    clustered_data_cluster_filt$seq_id[i-1])
    
  }
  
  clustered_data_cluster_filt <- 
    clustered_data_cluster_filt %>% 
      group_by(seq_id) %>% 
      slice_head(n=1) %>% 
      ungroup() %>% 
    
      mutate(lag = as.numeric(datetime - lag(datetime, n = 1)))
  
  clustered_data_cluster_filt %>% 
    select(datetime) %>% 
    slice(rep(1:n(), each = clean_forecast_period)) %>% 
    group_by(datetime) %>% 
    mutate(plus = (1:n()) - 1) %>% 
    ungroup() %>% 
    mutate(d = datetime + plus) %>% 
    select(-plus) %>% 
    rename(datetime_group = datetime, 
           datetime = d ) -> dates
  
  forecasting_data <- 
    inner_join(
      x = dates, 
      y = clustered_data, 
      by = "datetime"
    ) %>% 
    
    inner_join(
      x = . , 
      y = clustered_data %>% filter(datetime %in% dates$datetime_group) %>% 
            select(datetime, price) %>% 
            rename(anchor_price = price, 
                   datetime_group = datetime), 
      by = "datetime_group"
    ) %>% 
    
    group_by(datetime_group) %>% 
    mutate(price_ratio = price / anchor_price, 
           n = 1:n())
  
  forecasting_data %>% 
    group_by(datetime_group) %>% 
    slice_tail(n = 1) %>% 
    ungroup() -> terminal_stats
  
  
  N_1 <- nrow(terminal_stats)
  Mean_1 <- mean(terminal_stats$price_ratio)
  Meds_1 <- median(terminal_stats$price_ratio)
  Sd_1 <- sd(terminal_stats$price_ratio)
  Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% nrow()) / 
                                   nrow(terminal_stats)
  Mean_Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% summarise(mean(price_ratio))) %>% unlist()
 
  ns = c(ns, N_1)
  means = c(means, Mean_1)
  meds = c(meds, Meds_1)
  sds = c(sds, Sd_1)
  ps = c(ps, Pos_1)
  mean_pos = c(mean_pos, Mean_Pos_1)
}    

results = 
  data.frame(
    clusters, 
    ns, 
    means, 
    meds, 
    sds, 
    ps, 
    mean_pos
  ) %>% 
  arrange(-means)

forecasting_data <- 
  function(j){
    clustered_data_cluster_filt = clustered_data %>% filter(Cluster == j)
  
    clustered_data_cluster_filt$seq_id = NA
    clustered_data_cluster_filt$seq_id[1] <- 1
    
  for(i in 2:nrow(clustered_data_cluster_filt)){
    
    as.numeric(
      clustered_data_cluster_filt$datetime[i] - 
      clustered_data_cluster_filt$datetime[i-1]) -> date_diff_to_anchor
  
    clustered_data_cluster_filt$seq_id[i] <- ifelse(date_diff_to_anchor > clean_forecast_period, 
                                                    clustered_data_cluster_filt$seq_id[i-1]+1, 
                                                    clustered_data_cluster_filt$seq_id[i-1])
  }
  
  clustered_data_cluster_filt <- 
    clustered_data_cluster_filt %>% 
      group_by(seq_id) %>% 
      slice_head(n=1) %>% 
      ungroup() %>% 
    
      mutate(lag = as.numeric(datetime - lag(datetime, n = 1)))
  
  clustered_data_cluster_filt %>% 
    select(datetime) %>% 
    slice(rep(1:n(), each = clean_forecast_period)) %>% 
    group_by(datetime) %>% 
    mutate(plus = (1:n()) - 1) %>% 
    ungroup() %>% 
    mutate(d = datetime + plus) %>% 
    select(-plus) %>% 
    rename(datetime_group = datetime, 
           datetime = d ) -> dates
  
  forecasting_data <- 
    inner_join(
      x = dates, 
      y = clustered_data, 
      by = "datetime"
    ) %>% 
    
    inner_join(
      x = . , 
      y = clustered_data %>% filter(datetime %in% dates$datetime_group) %>% 
            select(datetime, price) %>% 
            rename(anchor_price = price, 
                   datetime_group = datetime), 
      by = "datetime_group"
    ) %>% 
    
    group_by(datetime_group) %>% 
    mutate(price_ratio = price / anchor_price, 
           n = 1:n())
  
  return(forecasting_data)
  }

plot_cluster <- 
  function(data, j){
  data %>% 
    group_by(datetime_group) %>% 
    slice_tail(n = 1) %>% 
    ungroup() -> terminal_stats
  
  
  N_1 <- nrow(terminal_stats)
  Mean_1 <- mean(terminal_stats$price_ratio)
  Med_1 <- median(terminal_stats$price_ratio)
  Sd_1 <- sd(terminal_stats$price_ratio)
   Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% nrow()) / 
                                     nrow(terminal_stats)
  Mean_Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% summarise(mean(price_ratio))) %>% unlist()
                                                           
  ggplot(data = data, 
         aes(x = n, y = price_ratio, group = datetime_group)) + 
    theme_classic() + 
    
    geom_line(color = "lightgrey") + 
    geom_smooth(aes(group = 1), color = "red", linewidth = 1) + 
    geom_hline(color = "black", yintercept = 1, linetype = "dashed") + 
    ggtitle(
      paste0(
        "Sequences used: ", N_1, 
        "\nMean Change: ", round(Mean_1, 4), 
        "\nMedian Change: ", round(Med_1, 4), 
        "\nS.D. Change: ", round(Sd_1, 4), 
        "\nP(Positive): ", round(Pos_1, 4), 
        "\nMean Change When Positive: ", round(Mean_Pos_1, 4))
      )
  }

plot_cluster_alt <- 
  function(data, j){
    
  data %>% 
    group_by(datetime_group) %>% 
    slice_tail(n = 1) %>% 
    ungroup() -> terminal_stats
  
  
  N_1 <- nrow(terminal_stats)
  Mean_1 <- mean(terminal_stats$price_ratio)
  Med_1 <- median(terminal_stats$price_ratio)
  Sd_1 <- sd(terminal_stats$price_ratio)
   Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% nrow()) / 
                                     nrow(terminal_stats)
  Mean_Pos_1 <- (terminal_stats %>% filter(price_ratio > 1) %>% summarise(mean(price_ratio))) %>% unlist()
                                                           
  ggplot(data = data, 
         aes(x = n, y = price_ratio, group = datetime_group)) + 
    theme_classic() + 
    
    geom_smooth(color = "lightgrey", se = F) + 
    geom_smooth(aes(group = 1), color = "red", linewidth = 1) + 
    geom_hline(color = "black", yintercept = 1, linetype = "dashed") + 
    ggtitle(
      paste0(
        "Sequences used: ", N_1, 
        "\nMean Change: ", round(Mean_1, 4), 
        "\nMedian Change: ", round(Med_1, 4), 
        "\nS.D. Change: ", round(Sd_1, 4), 
        "\nP(Positive): ", round(Pos_1, 4), 
        "\nMean Change When Positive: ", round(Mean_Pos_1, 4))
      )
  }

View(results %>% arrange(-means))

J = 4

d_data <- forecasting_data(j = J)

plot_cluster(data = d_data, j = J)

plot_cluster_alt(data = d_data, j = J)

d_data %>% 
  group_by(datetime_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio < 1) %>% select(datetime_group) %>% unlist() %>% 
  {as.Date(.)} -> neg_dates

d_data %>% 
  group_by(datetime_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio > 1) %>% select(datetime_group) %>% unlist() %>% 
  {as.Date(.)} -> pos_dates

ggplot(data = clustered_data, 
       aes(x = datetime, y = price)) + 
  theme_classic() + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(data = clustered_data %>% filter(datetime %in% neg_dates), 
             aes(x = datetime, y = price), 
             color = "red", 
             size = 2) + 
  geom_point(data = clustered_data %>% filter(datetime %in% pos_dates), 
             aes(x = datetime, y = price), 
             color = "blue", 
             size = 2) + 
  scale_y_continuous(transform = 'log10') + 
  theme(legend.position = "bottom")

######################################################

clustered_data %>% 
  filter(datetime == max(datetime)) %>% 
  select(Cluster) %>% unlist() -> today_J

d_data <- forecasting_data(j = today_J)

plot_cluster(data = d_data, j = today_J)

plot_cluster_alt(data = d_data, j = today_J)

d_data %>% 
  group_by(datetime_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio < 1) %>% select(datetime_group) %>% unlist() %>% 
  {as.Date(.)} -> neg_dates

d_data %>% 
  group_by(datetime_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio > 1) %>% select(datetime_group) %>% unlist() %>% 
  {as.Date(.)} -> pos_dates

ggplot(data = clustered_data, 
       aes(x = datetime, y = price)) + 
  theme_classic() + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(data = clustered_data %>% filter(datetime %in% neg_dates), 
             aes(x = datetime, y = price), 
             color = "red", 
             size = 2) + 
  geom_point(data = clustered_data %>% filter(datetime %in% pos_dates), 
             aes(x = datetime, y = price), 
             color = "blue", 
             size = 2) + 
  scale_y_continuous(transform = 'log10') + 
  theme(legend.position = "bottom")
