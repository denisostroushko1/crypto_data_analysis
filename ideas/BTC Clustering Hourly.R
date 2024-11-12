
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


###############
# Collect Data# 
###############

tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
save_object(object = "s3://crypto-data-shiny/hourly BTC.csv", file = tempfile)
all_data <- read.csv(tempfile)[,-1]

all_data$timestamp <- as.POSIXct(all_data$timestamp, format="%Y-%m-%d %H:%M:%S")

all_data <- 
  all_data %>% 
  mutate(
    timestamp = case_when(is.na(timestamp) ~ lag(timestamp) + 60^2, 
                          T ~ timestamp)
  ) %>% 
  rename(price = close)

#BTC

working_data <- all_data

list_ma <- c(10, 100, 200, 300, 400) 
list_rsi <-  c(10, 100, 200, 300, 400) 

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


############################################
# Select features that we use for clustering

selecting <- 
  c("timestamp", "price", 
    paste0("ma_", list_ma,"_ratio"),
    paste0("rsi_", list_rsi))


cluster_df <- 
  working_data %>% 
  select(all_of(selecting)) %>% na.omit()

cor(cluster_df %>% select(-timestamp, -price) %>% scale())

cor(cluster_df %>% select(-timestamp, -price) %>% scale()) %>% as.vector() %>% 
  unique() %>% summary()

cor(cluster_df %>% select(-timestamp, -price) %>% scale()) %>% as.vector() %>% 
  unique() %>% hist()

# Scale and center the data
scaled_data <- scale(cluster_df %>% select(-timestamp, -price))

# Function to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(scaled_data, k, iter.max = 10000)$tot.withinss
}

# Compute and plot within-cluster sum of squares for different number of clusters
k_values <- 3:100
wss_values <- map_dbl(k_values, wss)

# Create a data frame for plotting
elbow_df <- tibble(
  k = k_values,
  wss = wss_values
)

# Elbow plot
some_k <- 10

ggplot(elbow_df, aes(x = k, y = wss)) +
  theme_classic() + 
  geom_line() +
  geom_point() +
  ggtitle("Elbow Plot to Determine Optimal Number of Clusters") +
  xlab("Number of Clusters (k)") +
  ylab("Total Within-Cluster Sum of Squares") +
  geom_vline(xintercept = some_k, color = "red") + 
  scale_y_continuous(transform = "log10")
  
optimal_k <- 50

# Determine the optimal number of clusters (e.g., k = 3 from the elbow plot)

set.seed(81712)
# Perform K-means clustering with the optimal number of clusters
kmeans_result <- kmeans(scaled_data, centers = optimal_k, iter.max = 100000)

# Add cluster results to the original data
clustered_data <- cluster_df %>% 
  mutate(Cluster = kmeans_result$cluster)

clust = optimal_k

ggplot(data = clustered_data, 
       aes(x = timestamp, y = price)) + 
  theme_classic() + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(data = clustered_data %>% filter(Cluster == clust), 
             aes(x = timestamp, y = price, color = as.factor(Cluster) ), 
             size = 0.5) + 
  scale_y_continuous(transform = 'log10') + 
  theme(legend.position = "bottom")

#######
# For points within a cluster, define forecasting period 
# this will also be the clean period 

clusters = clustered_data$Cluster %>% unique() %>% sort()

ns <- means <- meds <- sds <- ps <- mean_pos <- c()

CAST = 8

clean_forecast_period <- CAST
clean_forecast_period <- clean_forecast_period + 1 
pred_period <- clean_forecast_period

####################################################################################
####################################################################################

for(i in 1:length(clusters)){
  
  print(i)
  
  clustered_data_cluster_filt = clustered_data %>% filter(Cluster == i)
  
  clustered_data_cluster_filt$seq_id = NA
  clustered_data_cluster_filt$seq_id[1] <- 1
  
  for(j in 2:nrow(clustered_data_cluster_filt)){
    
    as.numeric(
      clustered_data_cluster_filt$timestamp[j] - 
      clustered_data_cluster_filt$timestamp[j-1]) -> date_diff_to_anchor
    
    clustered_data_cluster_filt$seq_id[j] <- ifelse(date_diff_to_anchor > clean_forecast_period, 
                                                    clustered_data_cluster_filt$seq_id[j-1]+1, 
                                                    clustered_data_cluster_filt$seq_id[j-1])
    
  }
  
  clustered_data_cluster_filt <- 
    clustered_data_cluster_filt %>% 
      group_by(seq_id) %>% 
      slice_head(n=1) %>% 
      ungroup() %>% 
    
      mutate(lag = as.numeric(timestamp - lag(timestamp, n = 1)))
  
  clustered_data_cluster_filt %>% 
    select(timestamp) %>% 
    slice(rep(1:n(), each = clean_forecast_period)) %>% 
    group_by(timestamp) %>% 
    mutate(plus = (1:n()) - 1) %>% 
    ungroup() %>% 
    mutate(d = timestamp + 60^2 * plus) %>% 
    select(-plus) %>% 
    rename(timestamp_group = timestamp, 
           timestamp = d ) -> dates
  
  forecasting_data <- 
    inner_join(
      x = dates, 
      y = clustered_data, 
      by = "timestamp"
    ) %>% 
    
    inner_join(
      x = . , 
      y = clustered_data %>% filter(timestamp %in% dates$timestamp_group) %>% 
            select(timestamp, price) %>% 
            rename(anchor_price = price, 
                   timestamp_group = timestamp), 
      by = "timestamp_group"
    ) %>% 
    
    group_by(timestamp_group) %>% 
    mutate(price_ratio = price / anchor_price, 
           n = 1:n())
  
  forecasting_data %>% 
    group_by(timestamp_group) %>% 
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

summary(results$ps)

best_results <- results %>% filter(ps >= quantile(results$ps, .8))

View(best_results)
View(results)

forecasting_data <- 
  function(j){
    clustered_data_cluster_filt = clustered_data %>% filter(Cluster == j)
  
    clustered_data_cluster_filt$seq_id = NA
    clustered_data_cluster_filt$seq_id[1] <- 1
    
  for(i in 2:nrow(clustered_data_cluster_filt)){
    
    as.numeric(
      clustered_data_cluster_filt$timestamp[i] - 
      clustered_data_cluster_filt$timestamp[i-1]) -> date_diff_to_anchor
  
    clustered_data_cluster_filt$seq_id[i] <- ifelse(date_diff_to_anchor > clean_forecast_period, 
                                                    clustered_data_cluster_filt$seq_id[i-1]+1, 
                                                    clustered_data_cluster_filt$seq_id[i-1])
  }
  
  clustered_data_cluster_filt <- 
    clustered_data_cluster_filt %>% 
      group_by(seq_id) %>% 
      slice_head(n=1) %>% 
      ungroup() %>% 
    
      mutate(lag = as.numeric(timestamp - lag(timestamp, n = 1)))
  
  clustered_data_cluster_filt %>% 
    select(timestamp) %>% 
    slice(rep(1:n(), each = clean_forecast_period)) %>% 
    group_by(timestamp) %>% 
    mutate(plus = 60^2*(1:n()) - 60^2*1) %>% 
    ungroup() %>% 
    mutate(d = timestamp + plus) %>% 
    select(-plus) %>% 
    rename(timestamp_group = timestamp, 
           timestamp = d ) -> dates
  
  forecasting_data <- 
    inner_join(
      x = dates, 
      y = clustered_data, 
      by = "timestamp"
    ) %>% 
    
    inner_join(
      x = . , 
      y = clustered_data %>% filter(timestamp %in% dates$timestamp_group) %>% 
            select(timestamp, price) %>% 
            rename(anchor_price = price, 
                   timestamp_group = timestamp), 
      by = "timestamp_group"
    ) %>% 
    
    group_by(timestamp_group) %>% 
    mutate(price_ratio = price / anchor_price, 
           n = 1:n())
  
  return(forecasting_data)
  }

plot_cluster <- 
  function(data, j){
    
  data %>% 
    group_by(timestamp_group) %>% 
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
         aes(x = n, y = price_ratio, group = timestamp_group)) + 
    theme_classic() + 
    
    geom_line(color = "lightgrey") + 
    
    geom_smooth(aes(group = 1), color = "red", linewidth = 1) + 
    geom_smooth(aes(group = 1), color = "blue", linewidth = 1, method = "lm") +
    
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
    group_by(timestamp_group) %>% 
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
         aes(x = n, y = price_ratio, group = timestamp_group)) + 
    theme_classic() + 
    
    geom_smooth(color = "lightgrey", se = F) + 
    
    geom_smooth(aes(group = 1), color = "red", linewidth = 1) + 
    geom_smooth(aes(group = 1), color = "blue", linewidth = 1, method = "lm") +
    
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


J = 38

d_data <- forecasting_data(j = J)

plot_cluster(data = d_data, j = J)

plot_cluster_alt(data = d_data, j = J)

d_data %>% 
  group_by(timestamp_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio < 1) %>% select(timestamp_group)  -> neg_dates

d_data %>% 
  group_by(timestamp_group) %>% slice_tail(n = 1) %>% 
  ungroup() %>% 
  filter(price_ratio > 1) %>% select(timestamp_group)  -> pos_dates

ggplot(data = clustered_data, 
       aes(x = timestamp, y = price)) + 
  theme_classic() + 
  geom_line(aes(group = 1), color = "black") + 
  geom_point(data = clustered_data %>% filter(timestamp %in% neg_dates$timestamp_group), 
             aes(x = timestamp, y = price), 
             color = "red", 
             size = 2) + 
  geom_point(data = clustered_data %>% filter(timestamp %in% pos_dates$timestamp_group), 
             aes(x = timestamp, y = price), 
             color = "blue", 
             size = 2) + 
  scale_y_continuous(transform = 'log10') + 
  theme(legend.position = "bottom") + 
  labs(title = paste0("Red: price goes down next ", CAST, " hours\nBlue: price goes up"))

######################################################
