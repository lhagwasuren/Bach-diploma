rm(list = ls())
library(boot)
library(tsDyn)
library(forecast)
library(fGarch)

# AR(2) -------------------------------------------------------------------
set.seed(3)

n = 100 # 50 #100 # 300
nsim = 100
ar = c(.5,.45) # stationary, unit roottei oiroltsoo
h = 1 # 6, 12,
bl <- round(n/3)  # block lenght 
order = c(2,0,0)         ### table eer report hiih

e <- rnorm(n, mean = 0, sd = 1)  # sd up or down 100,15,1
y = arima.sim( list(order= order, ar = ar),innov = e ,n = n)

bagging = function(series){
  model = arima(series , order = c(2,0,0), ar = ar)
  forecast(model,h=h)$mean}

# fcast = data.frame( bag = rep(NA, nsim),
#                     original = rep(NA , nsim),
#                     real = rep(NA, nsim))
# 

y_train = list()
y_real = list()
for(i in 1:nsim){
  e = rnorm(n , mean = 0, sd = 1)
  y = arima.sim( list(order= order, ar = ar),innov = e ,n = n)
  y_train[[i]] = y[-length(y)]
  y_real[[i]] = y[length(y)]
  }

# bag = lapply(y, tsboot(y ,statistic = bagging, R = 100,l = bl, sim = "fixed" ))

library(tseries)

# y_sim = lapply(y_train, tsbootstrap, b = 500, type= 'block', nb=100)

models = lapply(y_train, auto.arima,max.order = 4)

f = lapply(models, FUN = forecast, h=1)

error = c()
for(i in 1:nsim){
  error[i] = (y_real[[i]] - f[[i]]$mean )^2
}

MSE = mean(error)
y_sim = list()

for(i in 1:nsim){
  a = a
  }

f_bag = rep(NA,nsim)

for(i in 1:nsim){
  a = tsbootstrap( y_train[[i]], b = round(n/5) , type = 'block', nb = 100)
  model = list()
  for(j in 1:nsim){
    model[[j]] = auto.arima( a[,j], max.order = 4)
  }
  fcast[i] = lapply(model, forecast, h=1)
  fcast_bag = c()
  for(k in 1:100){
    fcast_bag[k] = fcast[[k]]$mean }
  f_bag[i] = mean(fcast_bag)
}

a = tsbootstrap(y_train[[1]], b =500 , type = 'block', nb = 100)


model = list()
for( i in 1:100){
  model[[i]] = auto.arima(a[,i],max.order = 4)
  }

fcast = lapply(model,forecast, h=1)

fcast_bag = c()
for(i in 1:100){
  fcast_bag[i] = fcast[[i]]$mean }

fcast_bag = mean(fcast_bag)

for(i in 1:100){
model = lapply(y_sim[[1]] , auto.arima, max.order = 4)
f_b = lapply(model, forecast , h=1)
}


head(a)
plot(y)

for(i in 1:nsim){
  bag = tsboot( y[[i]], bagging, R = 100,l = bl, sim = "fixed" )
  fcast$bag[i] = mean ( bag$t )
  fcast$original[i] = bag$t0 }

MSE_bag = mean( (fcast$real - fcast$bag)^2 ) 
MSE_arima = mean( (fcast$real -fcast$original)^2 )



train_ar = y[-length(y)]
test_ar = y[length(y)]

ar_bag = rep(NA, nsim)

bagging = function(series){
  model = auto.arima(series,max.order = 4)
  forecast(model,h=h)$mean}

bag_ar = tsboot(y, bagging, R = nsim,l = bl, sim = "fixed")
forecast_bag = mean(bag_ar$t)
forecast_arima = bag_ar$t0

error_bag = test_ar - forecast_bag
error_arima  = test_ar - forecast_arima

error_bag < error_arima

knitr::kable(cbind(error_arima, error_bag))

# TAR ---------------------------------------------------------------------

data(m.unrate)
ser_tar = as.ts( diff(m.unrate))

train = ser_tar[-length(ser_tar)]
test = ser_tar[length(ser_tar)]

(model = auto.arima(train, max.order = 4))

n = length(ser_tar)
nsim = 1000
bl <- round(n/3)

bag_tar = tsboot(train, bagging, R = 100,l = bl, sim = "fixed")
forecast_bag = mean(bag_tar$t)
forecast_arima = bag_tar$t0

error_bag = test - forecast_bag
error_arima  = test - forecast_arima

error_bag < as.numeric(error_arima)

knitr::kable(cbind(error_arima, error_bag))


# GARCH -------------------------------------------------------------------

set.seed(53)
spec = garchSpec(model = list(alpha = c(0.12, 0.04), 
                              beta = c(0.08,.05)))
garch_series = garchSim(spec, n = 1000)
train = garch_series[-length(garch_series)]
test = garch_series[length(garch_series)]

(model = auto.arima(garch_series, max.order = 4))

bag_garch = tsboot(train, bagging, R = 100,l = 100, sim = "fixed")
forecast_bag = mean(bag_garch$t)
forecast_arima = bag_garch$t0

error_bag = test - forecast_bag
error_arima  = test - forecast_arima

error_bag < as.numeric(error_arima)

knitr::kable(cbind(error_arima, error_bag))
















