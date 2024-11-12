
source("Master Packages.R")


if(file.exists('keys.R') == T){rm(list = ls())}

if(file.exists('keys.R') == T){
  source("keys.R")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key, 
             "AWS_DEFAULT_REGION" =  aws_region)
  
  print("Connected to AWS using local keys")
}

tempfile <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
save_object(object = "s3://crypto-data-shiny/all_available_ETH.csv", file = tempfile)
all_data <- read.csv(tempfile) %>% select(all_of(c('datetime', 'price', 'open', 'high', 'low'))) %>% arrange(datetime)

all_data_EDA <- all_data %>% filter(
                                      # !(datetime >= as.Date("2014-02-13") & datetime <= as.Date("2014-02-25") ) & 
                                        # datetime >= as.Date("2010-10-26"))
                                        datetime >= as.Date("2010-12-01"))
all_data_EDA <- 
  all_data_EDA %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime) %>% 
  mutate(n = 1:n())


##############
# DATE FOR CUTOFF 
CUT <- as.Date("2023-01-01")

working_data <- all_data_EDA
# working_data <- working_data %>% filter(datetime <= CUT)

########################
# ISOLATING GLOBAL TREND 

# Find best power maximizing R^2 

powers <- seq(from = 2, to = 3, by = 0.1)
res <- c()

for(i in 1:length(powers)){
  working_data$time_pred <- (working_data$n)^(1/powers[i])
  m <- lm(log10(price) ~ time_pred, data = working_data)
  
  res <- c(res, summary(m)$r.squared)
}

best_power <- powers[which(res == max(res)) ]

working_data$time_pred <- (working_data$n)^(1/best_power)
m <- lm(log10(price) ~ time_pred, data = working_data)

working_data <- 
  working_data %>% 
  mutate(fitted = m$fitted.values, 
         adjusted = log10(price) - fitted) 

{
    ggplot(data = working_data, 
           aes(x = datetime, y = adjusted)
           ) + 
      theme_classic() + 
      geom_line()
  }

min_factor = min(working_data$adjusted)
working_data$adjusted <- working_data$adjusted - min_factor

{
    ggplot(data = working_data, 
           aes(x = datetime, y = adjusted)
           ) + 
      theme_classic() + 
      geom_line()  
}

{
  ggplot(data = working_data, 
       aes(x = datetime, y = log10(price))) + 
  theme_classic() + 
  geom_line() + 
  geom_line(data = working_data, aes(x = datetime, y = m$fitted.values), color = "blue")
}


### let E(Y) = A + cos(Time / B ) * exp(-k * Time)
###########

  
                  ##############
                  # VERSION 1 
if(T == F){
  ggplot(data = all_data_EDA, 
         aes(x = n, y = adjsuted)) + 
    # geom_point() + 
    geom_line() + 
    geom_smooth() + 
    
    geom_line(color = "red", 
              data = data.frame(n = all_data_EDA$n, 
                                # adjusted = 0.25 + cos(all_data_EDA$n / 200) * exp( - (0.0002 * all_data_EDA$n) ) 
                                adjusted = 0.25 + cos(all_data_EDA$n / 210) * exp( - (0.0002 * all_data_EDA$n) ) 
                                ), 
              aes(x = n, y = adjusted)
              ) 
  
  #################
  # hand_fit_function 
  hand_fit_function <- function(X, A, B, C){
    # 0.25 + cos(all_data_EDA$n / 210) * exp( - (0.0002 * all_data_EDA$n) ) 
    A + cos(X / B) * exp( - (C * X) ) 
  }
  
  A = 0.2
  B = 210
  C = 0.0002
  
  last_date <- max(all_data_EDA$datetime)
  last_n <- max(all_data_EDA$n)
  extend = 1000
  
  new_dates <- last_date + days(c(1:extend))
  new_ns <- last_n + c(1:extend)
  new_extends = hand_fit_function(X = new_ns, A = A, B = B, C = C)
  new_fitted = predict(object = m, newdata = data.frame(n = new_ns))
  
  #########
  project = 
    data.frame(
      datetime = new_dates, 
      n = new_ns, 
      fitted = new_fitted, 
      adjusted = new_extends, 
      price = NA
      )
  
  combined <- 
    rbind(
      all_data_EDA %>% 
        mutate(adjusted = hand_fit_function(n, A, B, C)) %>% 
        select(all_of(colnames(project))), 
      
      project) %>% 
    
    mutate(
      prediction = (fitted + adjusted)^(0.975)
    )
  
  ggplot(data = combined, 
         aes(x = datetime, y = log10(price))) + 
    theme_classic() + 
    geom_line() + 
    geom_line(data = combined, aes(x = datetime, y = fitted), color = "blue") + 
    geom_line(data = combined, aes(x = datetime, y = prediction), color = "red") + 
    
    geom_vline(linetype = "dashed", color = "darkgrey", size = 1, xintercept = as.Date("2025-05-01")) + 
    
    annotate("text", x=as.Date("2015-05-01"), y=5.5, 
             label= paste0("Projected price: ", 
                           round(
                             10^combined[combined$datetime == "2025-01-01", ]$prediction
                           ) %>% prettyNum(., big.mark = ",") %>% 
                             paste0("$", .)
                           )
             ) 
}

                  ##############
                  # VERSION 2 
{
  ggplot(data = working_data, 
         aes(x = n, y = adjusted)) + 
    theme_classic() + 
    geom_line() + 
 #  geom_smooth() + 
    
    geom_line(color = "blue", 
              data = data.frame(n = working_data$n, 
                                # adjusted = 0.25 + cos(all_data_EDA$n / 200) * exp( - (0.0002 * all_data_EDA$n) ) 
                                adjusted = 
                                  ( (1 + cos(working_data$n/175))^(1.5) ) * 
                                  exp(-0.00025 * working_data$n) + 
                                  working_data$n * 0.00003
                                ), 
              aes(x = n, y = adjusted)
              )  
    # 
    # geom_line(color = "red", 
    #           data = data.frame(n = working_data$n, 
    #                             # adjusted = 0.25 + cos(all_data_EDA$n / 200) * exp( - (0.0002 * all_data_EDA$n) ) 
    #                             adjusted = ( 1 + (cos(working_data$n/210)) ^ 1.15 )*exp(-0.00025 * working_data$n) 
    #                             ), 
    #           aes(x = n, y = adjusted)
    #           ) 
}

################
# version 2 of hand fit fucntion 

hand_fit_function2 <- function(X, A, B, C, D, E){
  # 
  # ( (1 + cos(working_data$n/210))^(1.5) ) * 
  #                                 exp(-0.00025 * working_data$n) + 
  #                                 working_data$n * 0.00003

  ( (A + cos(X/B))^(C) ) * exp(-D * X) + X * E
  }


last_date <- max(working_data$datetime)
last_n <- max(working_data$n)
extend <- as.numeric(as.Date("2026-05-01") - max(working_data$datetime)  )

new_dates <- last_date + days(c(1:extend))
new_ns <- last_n + c(1:extend)
new_time_pred <- new_ns^(1/best_power)

new_extends = hand_fit_function2(X = new_ns, A = 1, B = 210, C = 1.15, D = 0.00021, E = 0.00003)
new_fitted = predict(object = m, newdata = data.frame(time_pred = new_time_pred))


#########
project = 
  data.frame(
    datetime = new_dates, 
    n = new_ns, 
    fitted = new_fitted, 
    adjusted = new_extends, 
    price = NA
    )

combined <- 
  rbind(
    working_data %>% 
      mutate(adjusted = hand_fit_function2(n, A = 1, B = 210, C = 1.15, D = 0.00021, E = 0.00003)) %>% 
      select(all_of(colnames(project))
             ), 
    
    project) %>% 
  mutate(
    prediction = (fitted + adjusted) + min_factor
  )


DATE = combined[combined$prediction == max(combined$prediction), ]$datetime
ATH = log10(max(all_data_EDA$price))

ggplot(data = combined, 
       aes(x = datetime, y = log10(price))) + 
  theme_classic() + 
  geom_line(data = all_data_EDA, 
            aes(x = datetime, y = log10(price))) + 
  geom_line(data = combined, aes(x = datetime, y = fitted), color = "blue") + 
  geom_line(data = combined, aes(x = datetime, y = prediction), color = "red") + 
  
  geom_hline(linetype = "dashed", color = "black", size = 0.25, yintercept = ATH) + 
  
  geom_vline(linetype = "dashed", color = "darkgrey", size = 1, xintercept = DATE) + 
  geom_vline(linetype = "dashed", color = "darkgreen", size = 0.5, xintercept = CUT) + 
  
  annotate("text", x=as.Date("2015-06-01"), y=5.5, 
           label= paste0("Projected price: ", 
                         round(
                           10^combined[combined$datetime == DATE, ]$prediction
                         ) %>% prettyNum(., big.mark = ",") %>% 
                           paste0("$", .), 
                         "\nProjected date: ", DATE
                         )
           ) 


