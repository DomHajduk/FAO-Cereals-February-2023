library(tidyverse)
library(tsibble)
library(fable)
library(ggplot2)
library(GGally)
library(lubridate)
library(fpp3)
library(slider)
library(seasonal)

#Load CSV file with oil price data
Brent_prices <- read_csv("Europe BRENT.csv")

Brent_prices$Date <- 
  paste(Brent_prices$Date, "01", sep = "-") %>% 
  parse_date(Brent_prices$Date, format = "%b-%Y-%d")

#Prepare CSV file with FAO data
Indices <- read_csv("FAO Indices.csv")
Indices$Date <- 
  paste(Indices$Date, "01", sep = "-") %>% 
  parse_date(Indices$Date, format = "%F")
Indices <- Indices %>% 
  select(1:7)
Indices <- Indices %>% 
  left_join(Brent_prices, by = "Date")
  

#Add lags, streak counter
Indices <-
  Indices %>% 
  mutate(
    Cereals_1ago = lag(Cereals),
    Cereals_2ago = lag(Cereals, n = 2L),
    Cereals_rising = Cereals_1ago > Cereals_2ago
  )

Indices <-
  Indices %>% 
  mutate(
    Cereals_streak = sequence(rle(Cereals_rising)$lengths), 
    EndOfStreak = ifelse(lead(Cereals_rising)==Cereals_rising, "No", "Yes")
  )

Indices <- Indices %>% 
  select(-Cereals_1ago, -Cereals_2ago)

#A 9-month streak occurred only twice since 1990, and any streaks above 5 are a 28/388 occurence (cca 7,2%)  
#Indices %>% 
#  ggplot(aes(x = Date, y = Cereals_streak)) + 
#  geom_point(alpha = 0.5) +
#  scale_y_continuous(breaks = seq(0, 10, by = 1))

#Create a monthly Tsibble
Indices <- Indices %>% 
  mutate(
    Date = yearmonth(Date)
  )
Indices_ts <- as_tsibble(Indices, index = "Date")

#Plotting trend/season/cycle
#autoplot(Indices_ts, Cereals) + 
#  labs(title = "FAO Cereals Price Index",
#       subtitle = "January 1990 - May 2022",
#       y = "Index (100 = 2014-2016)")
# Relatively strong trend, but weak seasonality - if anything, around the 24-36 months mark
#Indices_ts %>% 
#  ACF(Cereals, lag_max = 120) %>% 
#  autoplot() +
#  labs(title = "Autocorrelation of cereal prices")
# X-11 Decomposition
#Cereals_dcmp <- Indices_ts %>% 
#  model(x11 = X_13ARIMA_SEATS(Cereals ~ x11())) %>% 
#  components()
#autoplot(Cereals_dcmp) + 
#  labs(title = "Decomposition of Cereals price index using X-11")

#Indices_ts %>%
#  ggplot(aes(x = Brent_spot_EU, y = Cereals)) + 
#  geom_point(alpha = 0.66) + 
#  geom_smooth()

# ETS_model <- Indices_ts %>% 
#  model(ETS(Cereals))
# components(ETS_model) %>% 
#  autoplot() +
#  labs(title = "ETS Components Cereals")
#ETS_model %>% 
#  forecast(h = 9) %>% 
#  autoplot(Indices_ts) +
#  labs(title = "FAO Cereals Price Index")

ARIMA <- Indices_ts %>% 
  model(ARIMA(Cereals, stepwise=FALSE, greedy=FALSE, approximation=FALSE)) %>% 
  report(ARIMA)

ARIMA %>% 
  forecast(h=8) %>% 
  autoplot(Indices_ts) +
  labs(title = "Forecast of FAO Cereals Price Index", subtitle = "ARIMA (1,1,0)(0,0,2)") + 
  coord_cartesian(xlim = as.numeric(as.Date(c("2010-01-01", "2023-02-01"))))

ARIMA %>% 
  gg_tsresiduals()

  

