data <- read.csv("/mnt/data/time-series-analysis/NYSE.csv")
data <- data[2800:3200, ]
ts_data <- ts(data$CLOSING.PRICE, frequency = 30)
ts_data <- log(ts_data)
plot(ts_data)
fit <- stl(ts_data, s.window = "period")
plot(fit)
ts_data.dif1 <- diff(ts_data, diff = 1)
# ts_data.dif <- diff(ts_data.dif)
plot(ts_data.dif1)
acf(ts_data.dif1)
pacf(ts_data.dif1)
# x.fit <- decompose(ts_data, type = "mult")
# plot(x.fit$figure, type = "o")
# monthly_ts_data <- aggregate(ts_data, nfrequency = 12, FUN = mean)
