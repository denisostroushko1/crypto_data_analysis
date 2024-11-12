



# Define the Quarto content
qmd_content <- glue("
---
title: '{title_var}'
format: html
execute:
  echo: false
  warning: false
  message: false
---

```{{r}}
# empty block that is easy to copy 
```

```{{r}}

source(
  paste0(
    substr(
      getwd(), 
      1, 
      nchar(getwd()) - nchar(paste0('{this_project_path}', '/', '{title_var}'))
    ), 
   'Master Packages.R' 
  )
)
```


```{{r}}
res1 <- read_rds('results_of_strat.rds')
```

```{{r}}
colnames_ <- c(
'From', 
'To', 
'Market ROI', 
'Strat. ROI', 
'Max Market ROI', 
'Min Market ROI', 
'Max Strat ROI', 
'Min Strat ROI', 
'% Time Market ROI Pos.', 
'% Time Strat ROI Pos.', 
'Market-Strat Running ROI Correlation'
)
```


# Upward Market 1

```{{r}}
plot_timeframe(res1$upward_1$time)
```

```{{r}}
kable(res1$upward_1$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```


# Upward Market 2

```{{r}}
plot_timeframe(res1$upward_2$time)
```

```{{r}}
kable(res1$upward_2$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```


# Upward Market 3

```{{r}}
plot_timeframe(res1$upward_3$time)
```

```{{r}}
kable(res1$upward_3$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```

# Upward Market 4

```{{r}}
plot_timeframe(res1$upward_4$time)
```

```{{r}}
kable(res1$upward_4$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```

# Downward Market 1

```{{r}}
plot_timeframe(res1$downward_1$time)
```

```{{r}}
kable(res1$downward_1$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```

# Downward Market 2

```{{r}}
plot_timeframe(res1$downward_2$time)
```

```{{r}}
kable(res1$downward_2$strategy_stats %>% 
  
  mutate(from_datetime = as.Date(from_datetime), 
          to_datetime = as.Date(to_datetime), 
          time_u_strategy_roi_pos = scales::percent(time_u_strategy_roi_pos, accuracy = 0.01),
          time_u_market_roi_pos = scales::percent(time_u_market_roi_pos, accuracy = 0.01)
          
  ), 
  col.names = colnames_, 
  digits = 2, 
  align = 'c'
  
  )
```

# Upward Markets: Synthetic Data 

```{{r}}

up_synth_summary_1 <- lapply(res1[which(grepl('_up_', names(res1) ))], function(entry) entry$strategy_stats) %>% bind_rows()

```

```{{r}}
returns_hist(results_df = up_synth_summary_1)
```

```{{r}}
return_compare(results_df = up_synth_summary_1)

table_summary(up_synth_summary_1) %>% kable(., digits = 2)
```


# Downward Markets: Synthetic Data 

```{{r}}

down_synth_summary_1 <- lapply(res1[which(grepl('_down_', names(res1) ))], function(entry) entry$strategy_stats) %>% bind_rows()

```

```{{r}}
returns_hist(results_df = down_synth_summary_1)
```

```{{r}}
return_compare(results_df = down_synth_summary_1)

table_summary(down_synth_summary_1) %>% kable(., digits = 2)
```


")



# Write the content to a .qmd file
doc <- paste0("./trading/Hourly BTC/", title_var, "/", title_var, " Summary.qmd")
writeLines(qmd_content, doc)

# Render the document
quarto::quarto_render(doc)
