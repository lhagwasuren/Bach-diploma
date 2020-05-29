set.seed(3)

ord = c(2,0,0)
ar = c(.5,.4)
n = 1000

y = arima.sim( list(ord, ar),n)

# order = c(1,0,0)
model = arima(y, ord, include.mean = F)
b = model$coef
y_1 = forecast(model, h=1)


nsim = 500
b_sim = data.frame(ar1 = rep( NA, nsim), ar2 = rep( NA, nsim ))

for(i in 1:nsim){
  y = arima.sim( list(order = ord, ar = ar),n = n)
  b_sim[i,] = coef(arima(y,ord,include.mean = F))
}

colMeans(b_sim)
hist(b_sim)
b_bag = mean(b_sim)

var(b_sim) * 1000
1-0.5^2

model_bag = arima(y,order = ord, include.mean = F, fixed = b_bag)

AIC(model)
AIC(model_bag)

y_2 = forecast(model_bag, h=1)

y_1001 = y[length(y)] * .5

(y_1001 - y_1$mean)^2 > (y_1001 - y_2$mean)^2



