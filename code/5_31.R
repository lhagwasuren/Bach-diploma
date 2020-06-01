rm(list = ls())

library(tseries)
library(forecast)
library(fGarch)

set.seed(3)

bagging = function(series ,h){
  model = forecast::Arima(series, order = c(2,0,0))
  fcast = forecast::forecast(model, h = h)$mean
  return(fcast)}

# AR(2) -------------------------------------------------------------------

n = 200 #50 #100 #300
nsim = 100 

ar = c(.5,.45) # stationary, unit roottei oiroltsoo

h = 1 # 6, 12  
# h = 6
# h = 12

# h= 1, 6, 12
# ceof # unit,stationary, white noise
# e = 1, 10, 20
# evaluate ## bias, MSE
# simualtion lenght 1000
# bagging 100, 200,500 
# block = 1/5, 1/10, 1/4

nb = 50
b = round( n/10 )
order = c(2,0,0)         ### table eer report hiih
e <- rnorm(n+h,0,1)  # sd up or down 100,15,1

train = vector(mode = 'list', length = nsim)
real = vector(mode = 'list', length = h)
for(i in 1:nsim){
  ar.sim = arima.sim( list(order= order, ar = ar),innov = e
                      ,n = n + h )
  train[[i]] = head( ar.sim, n)
  real[[i]] = tail(ar.sim, h)}


va = lapply(train , tsbootstrap ,nb = nb,  b = b,
            type = 'block', statistic = bagging, h = h)

ba = tsbootstrap(train[[1]], nb=10, b=b, type ='block'
                 ,statistic = bagging, h=h)
mean(ba$statistic)
ba$orig.statistic


bag = c()
org = c()
rel = c()
for( i in 1:nsim ){
  bag[i] = mean(va[[i]]$statistic)
  org[i] = va[[i]]$orig.statistic
  rel[i] = real[[i]] }


MSE_bag = mean ((rel - bag)^2 )
MSE_orig = mean ((rel - org)^2 )

MSE_bag < MSE_orig

knitr::kable(cbind(h = h,nsim, nb , MSE_orig, MSE_bag))