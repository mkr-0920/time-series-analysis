library("timetk")
library("forecast")
data <- read.csv("/mnt/data/time-series-analysis/NYSE.csv")
data <- data[2901:3350, ]
data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")
data$CLOSING.PRICE <- 
ts_data <- ts(data$CLOSING.PRICE, frequency = 1)
# 取对数
ts_data.log <- log(ts_data)
plot(ts_data)
# 差分
ts_data.log.dif <- diff(ts_data.log, diff = 1)
# ts_data.dif1 <- diff(ts_data.dif1, lag = 4)
plot(ts_data.log.dif)
acf(ts_data.log.dif)
pacf(ts_data.log.dif)
r1 <- arima(ts_data.dif1, c(2, 1, 0))
Box.test(r1$residuals, type = "Ljung-Box")
data %>%
  plot_time_series(DATE, CLOSING.PRICE, .interactive = F)
plot_seasonal_diagnostics(DATE, CLOSING.PRICE, .interactive = FALSE)

library(astsa)
rp <- sarima.for(ts_data, 10, 0, 1, 2, 0, 0, 0, 0)


splits <- data %>%
  time_series_split(assess = "1 months", cumulative = T)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(DATE, CLOSING.PRICE, .interactive = F)

model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(CLOSING.PRICE ~ DATE, training(splits))



calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table
calibration_table %>%
  modeltime_forecast(actual_data = date) %>%
  plot_modeltime_forecast(.interactive = FALSE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)
