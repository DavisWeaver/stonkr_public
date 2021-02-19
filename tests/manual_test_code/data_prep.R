library(tidyverse)
library(lubridate)
library(tidyquant)
library(forecast)
library(zoo)
library(broom) #for model evaluation

#start with the raw data
df_sep_raw <- readr::read_csv("G:/My Drive/pkg/data/sharadar/SHARADAR_SEP.csv")

#df_aapl <- df_sep_raw %>% filter(ticker == "AAPL")
#df_vygr <- df_sep_raw %>% filter(ticker == "VYGR")
#df_trv <- df_sep_raw %>% filter(ticker == "TRV")

#save for test purposes
#save(df_aapl, file = "G:/My Drive/pkg/data/sharadar/AAPL_SEP.csv")
#save(df_vygr, file = "G:/My Drive/pkg/data/sharadar/VYGR_SEP.csv")
#save(df_trv, file = "G:/My Drive/pkg/data/sharadar/TRV_SEP.csv")

load(file = "G:/My Drive/pkg/data/sharadar/AAPL_SEP.csv")
load(file = "G:/My Drive/pkg/data/sharadar/VYGR_SEP.csv")
load(file = "G:/My Drive/pkg/data/sharadar/TRV_SEP.csv")

#time_vec
time_aapl <- as.Date(df_aapl$date)
time_test <- sample(time_aapl, size = 1)


#date_vec
#start forecasting
