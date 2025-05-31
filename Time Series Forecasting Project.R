install.packages("readr")
library(readr)
library(tidyverse)
library(lubridate)
toyota<- read.csv("TM_Data.csv")

# STEP 3: Inspect structure and preview data
str(toyota)
head(toyota)
tail(toyota)

# STEP 4: Check raw format of Date column
head(toyota$Date, 5)

# STEP 5: Convert the Date column to proper Date format
# Based on earlier inspection, the format is likely "1/4/2021 16:00:00"
# So we use the correct datetime format for POSIXct, then strip time

toyota$Date <- as.POSIXct(toyota$Date, format = "%m/%d/%Y %H:%M:%S")
toyota$Date <- as.Date(toyota$Date)

# STEP 6: Confirm conversion was successful
head(toyota$Date)
summary(toyota$Date)
sum(is.na(toyota$Date))  # Should return 0

# STEP 7: Sort data by Date
toyota <- toyota %>% arrange(Date)

# STEP 8: Check for missing values or duplicates
colSums(is.na(toyota))  # Ensure no NAs
duplicates <- toyota[duplicated(toyota), ]
summary(toyota)

# STEP 9: Check data types
data_types <- sapply(toyota, class)
print(data_types)

# STEP 10: Create a time series object for the 'Close' column
# Assume trading days ~252 per year

n <- length(toyota$Close)          # Total observations
years <- n %/% 252                 # Complete years
last_day <- n %% 252              # Remaining days in final year

end_year <- 2021 + years
end_period <- ifelse(last_day == 0, 252, last_day)

close.ts <- ts(toyota$Close, 
               start = c(2021, 1), 
               end = c(end_year, end_period), 
               frequency = 252)

# STEP 11: Plot time series
plot(close.ts,
     xlab = "Time",
     ylab = "Closing Price (USD)",
     main = "Toyota Daily Closing Stock Price (2021–2025)",
     col = "steelblue",
     bty = "l")


#Plotting Seasonality 

library(dplyr)
library(ggplot2)
library(lubridate)

# Extract year and month
toyota$Year <- year(toyota$Date)
toyota$Month <- month(toyota$Date, label = TRUE, abbr = TRUE)

# Compute average monthly closing prices
monthly_avg <- toyota %>%
  group_by(Year, Month) %>%
  summarise(Average_Close = mean(Close, na.rm = TRUE), .groups = 'drop')

# Plot seasonality by month across years
ggplot(monthly_avg, aes(x = Month, y = Average_Close, color = factor(Year), group = Year)) +
  geom_line(size = 1) +
  labs(title = "Monthly Seasonal Pattern of Toyota Closing Prices",
       x = "Month", y = "Average Close Price (USD)", color = "Year") +
  theme_minimal()


# Seasonality plot by year — faceted version
ggplot(toyota, aes(x = Month, y = Close, group = Year)) +
  stat_summary(fun = mean, geom = "line", color = "steelblue", size = 1) +
  facet_wrap(~ Year, ncol = 2) +
  labs(title = "Seasonal Pattern of Toyota Closing Prices by Year",
       x = "Month", y = "Average Closing Price (USD)") +
  theme_minimal()




# Extract Day of Month
toyota$Day <- day(toyota$Date)

# Compute average closing price by day of the month
daily_avg <- toyota %>%
  group_by(Day) %>%
  summarise(Average_Close = mean(Close, na.rm = TRUE), .groups = 'drop')

# Plot daily seasonality
ggplot(daily_avg, aes(x = Day, y = Average_Close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1) +
  scale_x_continuous(breaks = seq(1, 31, by = 2)) +
  labs(title = "Daily Seasonal Pattern of Toyota Closing Prices",
       x = "Day of the Month", y = "Average Close Price (USD)") +
  theme_minimal()




library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# Perform STL decomposition
stl_decomp <- stl(close.ts, s.window = "periodic")

# Extract individual components
trend_component <- stl_decomp$time.series[, "trend"]
seasonal_component <- stl_decomp$time.series[, "seasonal"]
residual_component <- stl_decomp$time.series[, "remainder"]
time_index <- time(close.ts)

# Plot Trend
plot(time_index, trend_component, type = "l", col = "blue", lwd = 1,
     main = "Trend Component of Toyota Stock Prices",
     xlab = "Time", ylab = "Trend")

# Plot Seasonal
plot(time_index, seasonal_component, type = "l", col = "darkgreen", lwd = 1,
     main = "Seasonal Component of Toyota Stock Prices",
     xlab = "Time", ylab = "Seasonality")

# Plot Residuals
plot(time_index, residual_component, type = "l", col = "red", lwd = 1,
     main = "Residual Component of Toyota Stock Prices",
     xlab = "Time", ylab = "Residuals")










# STL decomposition
stl_decomp <- stl(close.ts, s.window = "periodic")

# Extract components
components <- stl_decomp$time.series %>%
  as_tibble() %>%
  rename(Trend = trend, Seasonal = seasonal, Residual = remainder)

# Add Observed (original series)
components$Observed <- as.numeric(close.ts)

# Add Time index
components$Time <- as.numeric(time(close.ts))

# Reorder columns to desired order
components <- components %>%
  select(Time, Observed, Trend, Seasonal, Residual)

# Convert to long format for plotting
components_long <- components %>%
  pivot_longer(cols = -Time, names_to = "Component", values_to = "Value")

# Set factor order for custom facet layout
components_long$Component <- factor(components_long$Component,
                                    levels = c("Observed", "Trend", "Seasonal", "Residual"))

# Plot
ggplot(components_long, aes(x = Time, y = Value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Component, ncol = 1, scales = "free_y") +
  labs(title = "STL Decomposition of Toyota Stock Prices",
       x = "Time", y = NULL) +
  theme_minimal()


# First differencing
close.diff1 <- diff(close.ts, differences = 1)

# ADF test on differenced series
adf.test(close.diff1)

# Plot ACF for first-differenced Toyota stock prices
acf(close.diff1, main = "ACF of First Differenced Toyota Closing Prices")

pacf(close.diff1, main = "PACF of First Differenced Toyota Closing Prices")



# Define partition sizes
nForecast <- 252
nTotal <- length(close.ts)
nTrain <- nTotal - nForecast

# Step 2: Partition the time series
train.ts <- window(close.ts, start = c(2021, 1), 
                   end = c(2021 + (nTrain - 1) %/% 252, (nTrain - 1) %% 252 + 1))

forecast.ts <- window(close.ts, start = c(2021 + (nTrain) %/% 252, (nTrain %% 252) + 1))

# Step 3: Build combined data frame for plotting
partition_df <- data.frame(
  Time = time(close.ts),
  Close = as.numeric(close.ts),
  Set = c(rep("Training", length(train.ts)),
          rep("Forecast", length(forecast.ts)))
)

# Step 4: Plot the data partition
ggplot(partition_df, aes(x = Time, y = Close, color = Set)) +
  geom_line(size = 1) +
  labs(title = "Toyota Stock Price: Training vs Forecast Partition (1-Year Ahead)",
       x = "Time", y = "Closing Price (USD)", color = "Set") +
  scale_color_manual(values = c("Training" = "steelblue",
                                "Forecast" = "firebrick")) +
  theme_minimal()

library(forecast)

# ---------- Model 1: Naive ----------
naive.model <- naive(train.ts, h = length(forecast.ts))
(naive.acc <- accuracy(naive.model, forecast.ts))
checkresiduals(naive.model)

# ---------- (Model) 2: ETS ----------
ets.model <- ets(train.ts)
ets.forecast <- forecast(ets.model, h = length(forecast.ts))
(ets.acc <- accuracy(ets.forecast, forecast.ts))
checkresiduals(ets.model)

# ---------- Model 3: ARIMA ----------
rwf.model <- rwf(train.ts, drift = TRUE)
arima.forecast <- forecast(arima.model, h = length(forecast.ts))
(arima.acc <- accuracy(rwf.model, forecast.ts))
checkresiduals(rwf.model)

# ---------- Model 4: TSLM (trend + season) ----------
tslm.model <- tslm(train.ts ~ trend + season)
tslm.forecast <- forecast(tslm.model, h = length(forecast.ts))
(tslm.acc <- accuracy(tslm.forecast, forecast.ts))


# ---------- Combine accuracy metrics ----------
model.names <- c("Naive", "ETS", "ARIMA", "TSLM")
acc.matrix <- rbind(naive.acc[2, ], ets.acc[2, ], arima.acc[2, ], tslm.acc[2, ])
(rownames(acc.matrix) <- model.names)

# View comparison
round(acc.matrix[, c("RMSE", "MAE", "MAPE","MASE", "Theil's U")], 3)

# Plot all forecasts on one chart
plot(naive.model, main = "Forecast Comparison: Toyota Stock Price (2025)",
     ylab = "Closing Price", xlab = "Time", bty = "l")

# Add actual and model lines
lines(forecast.ts, col = "black", lwd = 2, lty = 2)           # Actual 2025 values
lines(ets.forecast$mean, col = "green", lwd = 2)              # ETS
lines(arima.forecast$mean, col = "purple", lwd = 2)           # ARIMA
lines(tslm.forecast$mean, col = "orange", lwd = 2)            # TSLM

# Add legend
legend("topleft", legend = c("Actual", "Naive", "ETS", "ARIMA", "TSLM"),
       col = c("black", "blue", "green", "purple", "orange"),
       lty = c(2, 1, 1, 1, 1), lwd = 2)

# Plot ARIMA forecast vs actuals
autoplot(arima.forecast) +
  autolayer(forecast.ts, series = "Actual", linetype = "dashed", color = "black") +
  labs(title = "ARIMA Forecast vs Actual: Toyota Stock Price (2025)",
       x = "Time", y = "Closing Price (USD)") +
  scale_color_manual(values = c("blue", "black")) +
  theme_minimal()




# Required libraries
install.packages("zoo")     # Run only once
library(forecast)
library(dplyr)
library(lubridate)
library(zoo)                # For as.yearmon()

# Step 1: Forecast 252 trading days into 2025 using ARIMA
future_forecast <- forecast(arima.model, h = 252)

# Step 2: Create trading day sequence starting from last date in dataset
start_date <- as.Date(max(toyota$Date)) + 1
forecast_dates <- seq.Date(from = start_date, by = "day", length.out = 400)

# Filter only weekdays (trading days)
forecast_dates <- forecast_dates[weekdays(forecast_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
forecast_dates <- forecast_dates[1:252]  # Ensure 252 trading days

# Step 3: Combine forecast with dates
forecast_df <- data.frame(
  Date = forecast_dates,
  Forecast = future_forecast$mean,
  Lo_80 = future_forecast$lower[, 1],
  Hi_80 = future_forecast$upper[, 1],
  Lo_95 = future_forecast$lower[, 2],
  Hi_95 = future_forecast$upper[, 2]
)

# Step 4: Add month-year column and summarize by month
forecast_df$Month <- as.yearmon(forecast_df$Date)

monthly_summary <- forecast_df %>%
  group_by(Month) %>%
  summarise(
    Forecast = round(mean(Forecast), 4),
    Lo_80 = round(mean(Lo_80), 4),
    Hi_80 = round(mean(Hi_80), 4),
    Lo_95 = round(mean(Lo_95), 4),
    Hi_95 = round(mean(Hi_95), 4),
    .groups = 'drop'
  )

# Format Month for presentation (e.g., "Jan 2025")
monthly_summary$Month <- format(as.Date(monthly_summary$Month), "%b %Y")

# View the final table
print(monthly_summary)

# Optional: Export to CSV
# write.csv(monthly_summary, "Toyota_2025_Monthly_Forecast.csv", row.names = FALSE)


# Convert Month back to a proper Date object for plotting
monthly_summary$MonthDate <- as.Date(paste0("01 ", monthly_summary$Month), format = "%d %b %Y")

# Plot using ggplot2
library(ggplot2)

ggplot(monthly_summary, aes(x = MonthDate, y = Forecast)) +
  geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95), fill = "lightblue", alpha = 0.4) +
  geom_ribbon(aes(ymin = Lo_80, ymax = Hi_80), fill = "skyblue", alpha = 0.6) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Monthly Forecast of Toyota Stock Prices for 2025 (ARIMA)",
       x = "Month", y = "Forecasted Closing Price (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





