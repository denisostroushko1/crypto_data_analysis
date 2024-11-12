
source("Connect AWS.R")

tempfile_15 <- tempfile()  # temp filepath like /var/folders/vq/km5xms9179s_6vhpw5jxfrth0000gn/T//RtmpKgMGfZ/file4c6e2cfde13e
save_object(object = "s3://crypto-data-shiny/hourly ETH.csv", file = tempfile_15)
df <- read.csv(tempfile_15) %>% 
  mutate(timestamp = ifelse(nchar(timestamp) < max(nchar(timestamp)), paste0(timestamp, " 00:00:00"), timestamp), 
         datetime = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S"))
# 
