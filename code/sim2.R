rm(list = ls())

library(tseries)
library(forecast)
library(fGarch)

set.seed(3)

bagging = function(series ,h){
  model = forecast::Arima(series, order = c(2,0,0),)
  fcast = forecast::forecast(model, h = h)$mean
  return(fcast)
}

# AR(2) -------------------------------------------------------------------

n = 100 # 50 #100 # 300
nsim = 100 
ar = c(.5,.45) # stationary, unit roottei oiroltsoo
h = 1 # 6, 12  
h6 = 6
h12 = 12

order = c(2,0,0)         ### table eer report hiih
# e <- rnorm(n)  # sd up or down 100,15,1

train = vector(mode = 'list', length = nsim)
real = vector(mode = 'list', length = h)
for(i in 1:nsim){
  ar.sim = arima.sim( list(order= order, ar = ar),innov = rnorm(n + h)
                      ,n = n + h )
  train[[i]] = head( ar.sim, n)
  real[[i]] = tail(ar.sim, h)}

nb = 100

va = lapply(train , tsbootstrap ,nb = nb,  b = round( n/3), 
             type = 'block', statistic = bagging, h= h)

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

# TAR(2) ------------------------------------------------------------------

### y[t-1] > g == y[t] = a0 + a1 * y[t-1] + a2 * y[t-2] + e[t]
### y[t-1]<= g == y[t] = b0 + b1 * y[t-1] + b2 * y[t-2] + e[t]

tar2.sim = function(y0, e , n, p1, p2, th){
  y = rep(0 ,n)
  a0 = p1[1]; a1 = p1[2]; a2 = p1[3] 
  b0 = p2[1]; b1 = p2[2]; b2 = p2[3]
  for(t in 3:n){
    if(y[t-1] > th) y[t] = a0 + a1 * y[t-1] + a2 * y[t-2] + e[t]
    else y[t] = b0 + b1* y[t-1] + b2 * y[t-2] + e[t]
  }
  return(y)
}

n = 100
nsim = 100
h=1
train = vector(mode = 'list', length = nsim)
real = rep(NA , nsim)

for(i in 1:nsim){
  tar.sim = tar2.sim(0, rnorm(n = n + h), n = n+1, 
                         c(0.1,.09,.07), 
                         c(0.2 , .05 , .04), th = 0)
  train[[i]] = head( tar.sim, n)
  real[i] = tail(tar.sim,h) }


nb = 100
va = lapply(train , tsbootstrap ,nb = nb,  b = round( n/3),
            type = 'block', statistic = bagging, h= h)

bag = c()
org = c()
rel = c()
for( i in 1:nsim ){
  bag[i] = mean(va[[i]]$statistic)
  org[i] = va[[i]]$orig.statistic
  rel[i] = real[i] }


MSE_bag = mean ( (rel - bag)^2 )
MSE_orig = mean( (rel - org)^2 )

MSE_bag < MSE_orig

knitr::kable(cbind(h = h,nsim, nb , MSE_orig, MSE_bag))

# nb = 10 
# va = lapply(train , tsbootstrap ,nb = nb,  b = round( n/3), 
#              type = 'block', statistic = bagging)
# 
# ba = rep(NA, nsim)
# or = rep(NA, nsim)
# for( i in 1:nsim){
#   ba[i] = mean( va[[i]]$statistic )
#   or[i] = va[[i]]$orig.statistic
# }
# 
# MSE_bag = mean( (real - ba)^2 )
# MSE_orig = mean( (real - or)^2 )
# 
# MSE_bag < MSE_orig
# 
# knitr::kable(cbind(nsim, nb , MSE_orig, MSE_bag))



# GARCH(2,2) --------------------------------------------------------------

spec = garchSpec(model = list(alpha = c(0.12, 0.04),
                              beta = c(0.08,.05)),
                 cond.dist = 'norm')

train = vector(mode = 'list', length = nsim)
real = rep(NA , nsim)

for(i in 1:nsim){
  garch.sim = garchSim(spec, n = n + 1, extended = F)$garch
  train[[i]] = head( tar.sim, n)
  real[i] = tail(tar.sim,h) }


nb = 100
va = lapply(train , tsbootstrap ,nb = nb,  b = round( n/3),
            type = 'block', statistic = bagging, h= h)

bag = c()
org = c()
rel = c()
for( i in 1:nsim ){
  bag[i] = mean(va[[i]]$statistic)
  org[i] = va[[i]]$orig.statistic
  rel[i] = real[i] }

MSE_bag = mean ( (rel - bag)^2 )
MSE_orig = mean( (rel - org)^2 )

MSE_bag < MSE_orig

knitr::kable(cbind(h = h,nsim, nb , MSE_orig, MSE_bag))


