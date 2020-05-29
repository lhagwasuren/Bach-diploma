rm(list = ls())

library(forecast)
library(tsDyn)

# AR(2)
n = 1500
e <- rnorm(n)

set.seed(62)

y_t <- arima.sim(list(ar=c(0.5, .45)), n = n, innov = e )

y = window(y_t , end = 1499)

model1 = arima(y ,order = c(2,0,0) , include.mean = F)
pred = forecast(model1,h = 1)$mean

nsim = 1000
b_sim = data.frame(ar1 = rep(NA, nsim), ar2 = rep(NA, nsim))

for(i in 1:nsim){
  y <- sample(y)
  model = arima(y , order = c(2,0,0), include.mean = F)
  b_sim[i,] = coef(model)
  }

b_sim

bag = colMeans(b_sim)
hist(b_sim$ar1)

model_bag = arima(y, order = c(2,0,0), fixed = bag, include.mean = F)
pred_bag = forecast(model_bag, h=1)$mean

pred
pred_bag

real = as.numeric(window(y_t, start = 1500))

(real - pred)^2 < (real - pred_bag)^2



