library(boot)

set.seed(63)

bagging = function(series){
  model = arima( series , order = c(2,0,0) , 
                 include.mean = F )
  coef(model)
  }

order = c(2,0,0)
nsim = 100
m = 100
arc = c(.5,.45)
h = 1
n = 1500
e <- rnorm(n, mean = 0, sd = 1)
y_t = arima.sim( list(order= order, ar = arc),innov = e ,n = n+1)
y = y_t[-length(y_t)]

res <- 2000
block_length <- round(n/3)

manaus_fixed <- tsboot(y, bagging,R = res, 
                       l = block_length, sim = "fixed")
manaus_fixed$t
colMeans(manaus_fixed$t)

coef(arima(y , order = c(2,0,0), include.mean = F))


