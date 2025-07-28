library(tidyverse)
library(lubridate)
library(openair)
library(readxl)
library(fable)
library(tsibble)


install.packages("readxl")
library(readxl)


data<-read_excel('/Volumes/Surya/Github projects/R programming/aqi_final.xlsx')

data

head(data)

tail(data)

summary(data)

str(data)

install.packages("openair")

library(openair)

timePlot(data, pollutant = c("AQI"), avg.time = "month")

#monthly means for each year
install.packages("lubridate")
library(lubridate)


#month wise data
data$month<-floor_date(data$date,'month')

data$month

install.packages("dplyr")
library(dplyr)

#mean of air quality index for the following year

aqi_mean<-data %>% group_by(month) %>% summarize(AQI = mean(AQI))

aqi_mean

install.packages('tsibble')
library(tsibble)

aqi_monthly <- aqi_mean %>%
  mutate(Date = yearmonth(as.character(month))) %>%
  as_tsibble(index = Date)


aqi_monthly


aqi_models<-aqi_monthy %>% model(ARIMA=ARIMA(AQI),
                                 ETS=ETS(AQI~ season(c("A"))))%>% 
  mutate(AVERAGE=(ARIMA+ETS)/2)

library(tsibble)
library(fable)
library(dplyr)

install.packages('feasts')
library(feasts)


aqi_models <- aqi_monthly %>%
  model(
    ARIMA = ARIMA(AQI),
    ETS = ETS(AQI ~ season("A"))
  )


aqi_models

#forecasting for 12 months
aqi_forecasts <- aqi_models %>%
  forecast(h = "12 months")

aqi_forecasts

#aqi forecasts wide
library(dplyr)
library(tidyr)

aqi_forecasts_wide <- aqi_forecasts %>%
  as_tibble() %>%           # drop tsibble class and attrs
  pivot_wider(names_from = .model, values_from = .mean) %>%
  mutate(AVERAGE = (ARIMA + ETS) / 2)


aqi_forecasts_wide


aqi_forecasts_wide <- aqi_forecasts_wide %>%
  mutate(AVERAGE = (ARIMA + ETS) / 2)

#average forecast

aqi_forecasts_wide

install.packages('ggplot2')
library(ggplot2)
aqi_forecasts %>%
  autoplot(aqi_monthly, level = 95) +      # historical + forecasts
  facet_wrap(vars(.model)) +                 # separate plots per model
  labs(title = "AQI Forecasts from ARIMA and ETS Models",
       y = "AQI",
       x = "Date") +
  theme_minimal()

