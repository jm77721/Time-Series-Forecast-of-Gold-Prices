library(fpp3)
library(readr)
library(zoo)
library(urca)
library(GGally)
library(tsibble)

#####################################
data <- read.csv("gold.csv")
#Remove two days of data in September 2022 
data
data2 <- data[!(data$Date == '2022-09-01' | data$Date == '2022-09-02'), ]

#Create tsibble with one data point a month, the average of the daily values
gold <- tibble(data2) %>%
  mutate(month = yearmonth(Date)) %>%
  group_by(month) %>%
  summarize(open = mean(Open), high= mean(High), low = mean(Low), 
            close = mean(Close), volume = sum(Volume)) %>%  
  as_tsibble(index = month)

view(gold)

#look at correlation of all variables over entire time period (1/2000- 8/2022)
gold %>% 
  GGally::ggpairs(columns = 2:6)

#look at correlation of all variables over entire time period (1/2020- 8/2022)
gold %>% 
  filter(month > yearmonth('2019 Dec')) %>%
  GGally::ggpairs(columns = 2:6)

#autoplot close
gold %>% autoplot(close)

#split into training and testing

g_train <- gold %>%
  filter(month < yearmonth('2020 Jun'))
g_train %>% autoplot(close)

g_test <- gold %>%
  filter(month >= yearmonth('2020 Jun'))
g_test %>% autoplot(close)

#############################################################################
##############Exponential Smoothing Model ###################################

#Find the best ETC model
fit <- g_train %>%
  model(ets = ETS(close))
fit

#Best model is Multiplicative Error, Additive Trend, No seasonal

fit %>%
  forecast(h = 27) %>%
  autoplot(gold) +
  labs( y = '$USD', title = 'Close Price of Gold')
#RSME
accuracy(fit)
#Residuals
fit %>% gg_tsresiduals()
#Decomposition
components(fit) %>% autoplot()
#AICC
report(fit)

##Try added a damped trend:  Multiplicative Error, damped Additive Trend, No seasonal
fit <- g_train %>%
  model(m = ETS(close ~ error("M") + trend("Ad") + season("N"))
  )

fit %>%
  forecast(h = 27) %>%
  autoplot(gold) +
  labs( y = '$USD', title = 'Close Price of Gold')
#RSME
accuracy(fit)
#Residuals
fit %>% gg_tsresiduals()
#Decomposition
components(fit) %>% autoplot()
#AICC
report(fit)

#############################################################################
##############ARIMA Model ###################################

fit2 <- g_train %>%
  model(arima = ARIMA(close, stepwise = FALSE, approx = FALSE))

report(fit2)
#best ARIMA model: ARIMA(0,1,1) w/ drift
fit2 %>%
  forecast(h = 27) %>%
  autoplot(gold) +
  labs( y = '$USD', title = 'Close Price of Gold')

#KPSS test
gold|>features(close, unitroot_kpss)

#RSME
accuracy(fit2)

#ARIMA Forecast
fit2 %>%
  forecast(h = 27) %>%
  accuracy(gold)

#Residuals
fit2 %>% gg_tsresiduals()
gold %>%
  gg_tsdisplay(close, plot_type='partial')

#AICC
report(fit2)

augment(fit2) %>% features(.innov, ljung_box, lag=24, dof=4)

#models commonly used by the census bureau
cb<-g_train %>%
  model(arima011011 = ARIMA(log(close) ~ pdq(0,1,1)+PDQ(0,1,1)),
        arima012011 = ARIMA(log(close) ~ pdq(0,1,2)+PDQ(0,1,1)),
        arima210011 = ARIMA(log(close) ~ pdq(2,1,0)+PDQ(0,1,1)),
        arima212011 = ARIMA(close ~ pdq(2,1,2)+PDQ(0,1,1)))
report(cb)

#RSME
accuracy(cb)

#Best census bureau model
cb %>%
  forecast(h = 27) %>%
  autoplot(gold) +
  labs( y = '$USD', title = 'Close Price of Gold')

#RSME
cb %>%
  forecast(h = 27) %>%
  accuracy(gold)
#each of the models has a greater RSME than the best model designated by R

## ARIMA vs ETS
b2m <- g_train %>%
  model(ets = ETS(close),
        arima = ARIMA(close, stepwise = FALSE, approx = FALSE))
b2m
b2m %>%
  forecast(h = 27) %>%
  autoplot(gold) +
  labs( y = '$USD', title = 'Close Price of Gold')
b2m %>%
  forecast(h = 27) %>%
  accuracy(gold)
#The ARIMA model has the better RMSE


               