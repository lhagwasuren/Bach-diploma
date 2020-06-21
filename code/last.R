
library(tseries)
library(fGarch)
library(forecast)

fitting = function(series ,h){
  model = ar.ols(series,order.max = 4)
  fcast = predict(model, n.ahead = h)$pred
  return(fcast) }


n  <-  100
h <- 12 
nsim  <-  50  
ar  <- c(.5,.45) 
nb  <- 50           

sd <- 1  

b <- round( n^(1/3)) 


order <- c(2,0,0)   

set.seed(3)
errors_sim <- matrix(nrow = nsim, ncol = h)
errors_bag <- matrix(nrow = nsim, ncol = h)

tar2.sim = function(y0, e , n, p1, p2, th){
  y = rep(0 ,n)
  a0 = p1[1]; a1 = p1[2]; a2 = p1[3] 
  b0 = p2[1]; b1 = p2[2]; b2 = p2[3]
  for(t in 3:n){
    if(y[t-1] > th) y[t] = a0 + a1 * y[t-1] + a2 * y[t-2] + e[t]
    else y[t] = b0 + b1* y[t-1] + b2 * y[t-2] + e[t]
  }
  return(y)}

for(i in 1:nsim){
  tar.sim = tar2.sim(0, e = arima.sim( list(order= order, ar = ar),
                                       innov = rnorm(n+h, 0, sd)  ,  # t dist,
                                       n = n + h), 
                     n = n+h, 
                     c(0.1,.09,.07), 
                     c(0.2 , .05 , .04), th = 0)
  train <- head( tar.sim, n)
  test <- tail(tar.sim, h)
  errors_sim[i,] <- fitting(train, h) - test
  errors_bag[i,] <- colMeans(tsbootstrap(train, nb = nb, b=n/10, type = 'block',
                                         statistic = fitting, h= h)$statistic) - test}

MSE_ar <- colMeans(errors_sim^2)
MSE_bag <- colMeans(errors_bag^2)

knitr::kable(cbind( h=1:12, MSE = MSE_ar*10^6, BAG = MSE_bag*10^6 ,nsim,n,nb))