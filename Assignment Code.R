#Loading necessary packages
library("ggplot2")
library("forecast")

#Importing required data for last digit = 2
data = read.csv("assign_data_23.csv")

##Question (a)
#Setting up time-series data
ts.data = ts(data$x, start = data$X[1], end = data$X[length(data$X)])
#Plotting time-series data and editing the titles and the axes
autoplot(ts.data) + 
  labs(title = "Weekly Price of Asset",
       x = "Week",
       y = "Price of Asset")

##Question (b)
#Calculating the mean of the data
mean_x = mean(data$x); print(mean_x)
#Calculating and plotting the ACF of the data up to a lag of 19
acf_x = acf(data$x, lag.max = 19, plot = T, main = "Autocorrelation function of the data up to lag = 19")


##Question (c)
#Creating a 4x4 matrix 
Qc = matrix(nrow = 4, ncol = 4)
n = length(data$x)

#Fitting an ARMA(p,q) model and computing BIC values
for (p in 1:4) {
  for (q in 1:4) {
    arima.model.c = arima(ts.data, order = c(p, 0, q), optim.control = list(maxit = 1000))
    
    BIC.c = log(arima.model.c$sigma2) + log(n) * ((p + q)/n)
    Qc[p,q] = BIC.c
  }
}
print(round(Qc, 4))

##Question (d)
#Creating a 4x4 matrix
Qd = matrix(nrow = 4, ncol = 4)

#Fitting an ARIMA(p,1,q) model and computing BIC values
for (p in 1:4) {
  for (q in 1:4) {
    arima.model.d = arima(ts.data, order = c(p, 1, q), optim.control = list(maxit = 1000))
    
    BIC.d = log(arima.model.d$sigma2) + log(n) * ((p + q)/n)
    Qd[p,q] = BIC.d
  }
}
print(round(Qd,4))

##Question (e)
#Combination of p,d,q that results in lowest BIC values
if(min(c(Qc, Qd)) %in% Qc){
  model.order.index = which(Qc == min(Qc), arr.ind = T)
  model.order.index = append(model.order.index, 0, after = 1 )
} else {
  model.order.index = which(Qd == min(Qd), arr.ind = T)
  model.order.index = append(model.order.index, 1, after = 1 )
}
print(model.order.index)

#ARMA(1,0,1) model fits best since smallest BIC value
arima_estimate = arima(ts.data, order = model.order.index, optim.control = list(maxit = 1000))

##Question (f)
#Simulating next 26 time points with seed 1234
set.seed(1234)
sim1 = simulate(arima_estimate, nsim = 26)

#Simulating next 26 time points with seed 1235
set.seed(1235)
sim2 = simulate(arima_estimate, nsim = 26)

#Plotting the two realisations of the simulation model
autoplot(cbind(sim1, sim2)) +
  labs(title = "Simulation of 2 realisations of the next 26 time points",
       x = "Week",
       y = "Price of Asset")

##Question (g)
#Forecasting 52 weeks following the ARMA(p,q) model
arima_forecast = forecast(arima_estimate, h = 52)

#Plotting the forecast results showing the prediction interval and mean
autoplot(arima_forecast, legend = T) +
  labs(title = "Forecasting 52 data points with prediction mean and intervals",
       x = "Week",
       y = "Price of Asset") +
  #Showing ONLY forecasted period
  xlim(1000, NA) +
  #Lines indicating 6 weeks, 6 months(26 weeks), 1 year
  geom_vline(xintercept = c(1006, 1026, 1052), linetype = 2) +
  #Lines showing today's price
  geom_hline(yintercept = 33.81, linetype = 2)