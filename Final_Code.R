# Install and Load the required libraries
library(forecast)
install.packages("zoo")
library(zoo)

# Set the working directory
setwd("/Users/abithapasupuleti/Documents/673 Time Series/")

# Read the dataset
data <- read.csv("sales.csv")

# Convert the "Period" column to a proper date format
data$Date <- as.Date(paste0("01-", data$Period), format = "%d-%b-%Y")

# Create a time series object using the "Value" column
sales.ts <- ts(data$Value, start = c(2013, 1), end = c(2023, 12), frequency = 12)

# Print the time series object
sales.ts
str(data)

# Remove commas from the "Value" column and convert it to numeric
data$Value <- as.numeric(gsub(",", "", data$Value))

# Convert the "Period" column to Date format
data$Date <- as.Date(paste0("01-", data$Period), format = "%d-%b-%Y")

# Create the time series object
sales.ts <- ts(data$Value, start = c(2013, 1), end = c(2023, 12), frequency = 12)
sales.ts

# Plot the time series
plot(sales.ts, main = "Montly Sales", ylab = "Sales", xlab = "Years (2013-2023)")
axis(1, at = seq(2013, 2023, by = 1), labels = seq(2013, 2023, by = 1))


#VISUALIZATION OF DATA
plot(sales.ts, main = "Monthly Sales Over Time", xlab = "Year", ylab = "Sales")


#Seasonal and Trend Decomposition
sales.stl <- stl(sales.ts, s.window = "periodic")
plot(sales.stl)


#Histogram of sales data
hist(sales.ts, main = "Histogram of Sales Data", xlab = "Sales", ylab = "Frequency")


#ACF Function
autocor <- Acf(sales.ts, lag.max = 12, main = "Autocorrelation for Monthly Sales")
autocor


#BOX PLOT -
boxplot(split(sales.ts, cycle(sales.ts)), main = "Boxplot of Sales Data by Month")

##Develop data partition with the validation partition of 36 periods and the rest for the
#training partition.
nValid <- 36

valid_end_index <- length(sales.ts)
valid_start_index <- valid_end_index - nValid + 1
valid.ts <- window(sales.ts, start = c(2021, 1), end = c(2023, valid_start_index))
train.ts <- window(sales.ts, end = c(2020, 12))

train.ts
valid.ts

#Trailing MA 
## CREATE TRAILING MA FOR VARIOUS WINDOWS (NUMBER OF PERIODS).
## SHOW FIRST SIX AND LAST SIX VALUES OF TRAILING MA.
## IDENTIFY FORECAST ACCURACY FOR TRAILING MA FORECASTS.
## FORECAST USING TRAILING MA.


# Create trailing MA with window widths (number of periods) 
# of k = 3,6, 12 and 24.
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_3 <- rollmean(train.ts, k = 3, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")
ma.trailing_24 <- rollmean(train.ts, k = 24, align = "right")

# Show first six and last six values of trailing MA
head(ma.trailing_3, 6)
tail(ma.trailing_3, 6)
head(ma.trailing_6, 6)
tail(ma.trailing_6, 6)
head(ma.trailing_12, 6)
tail(ma.trailing_12, 6)
head(ma.trailing_24, 6)
tail(ma.trailing_24, 6)

# Create forecast for the validation data for the window widths 
# of k = 3, 6,12 and 24. 
ma.trail_3.pred <- forecast(ma.trailing_3, h = nValid, level = 0)
ma.trail_3.pred
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_6.pred
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred
ma.trail_24.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_24.pred

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_3.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_24.pred$mean, valid.ts), 3)


# Plot trailing MA with different window widths

plot(ma.trailing_3, col = "brown", main = "Trailing Moving Average", ylab = "Sales", xlab = "Time")
lines(ma.trailing_6, col = "blue")
lines(ma.trailing_12, col = "green")
lines(ma.trailing_24, col = "red")

legend("topleft", legend = c("k=3", "k=6", "k=12", "k=24"), col = c("brown", "blue", "green", "red"), lty = 1)

## GENERATE PLOT FOR PARTITION DATA AND TRAILING MA.

# Plot original data and forecast for training and validation partitions
# using trailing MA with window widths of k = 3 and k = 6.
  
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(5000, 20000), bty = "l", xaxt = "n", xlim = c(2013, 2023.25), main = "Trailing Moving Average")

axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )

lines(ma.trailing_3, col = "brown", lwd = 2, lty = 1)
lines(ma.trail_3.pred$mean, col = "brown", lwd = 2, lty = 2)

lines(ma.trailing_6, col = "blue", lwd = 2, lty = 1)
lines(ma.trail_6.pred$mean, col = "blue", lwd = 2, lty = 2)

legend(2013,18000, legend = c("Sales Data", "Trailing MA, k=3, Training Partition", "Trailing MA, k=3, Validation Partition", "Trailing MA, k=6, Training Partition", "Trailing MA, k=6, Validation Partition"), col = c("black", "brown", "brown", "blue", "blue"), lty = c(1, 1, 2, 1, 2), lwd =c(1, 2, 2, 2, 2), bty = "n")

# Fit the regression model with linear trend and seasonality
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

#Forecast_Valid
forecast_valid <- forecast(trend.seas, newdata = data.frame(sales = window(sales.ts, start = c(2021, 1), end = c(2023, 12))))
forecast_valid

trend.seas.res <- trend.seas$residuals
trend.seas.res

ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Calculate trend and seasonality predictions
trend.seas.pred <- forecast(trend.seas, h = length(valid.ts))

# Combine predictions for two-level forecast
frcst.two_level <- trend.seas.pred$mean + ma.trail.res.pred$mean

# Print the two-level forecast
print(frcst.two_level)

valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, ma.trail.res.pred$mean,  frcst.two_level), 3)
names(valid.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
valid.df

round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(frcst.two_level, valid.ts),3)

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for 12 future months.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred
# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

# Creating forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.frcst.two_level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.frcst.two_level

# Creating a table with regression forecast, trailing MA for residuals and total forecast for future 12 months.
future_12_periods.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                         tot.frcst.two_level), 3)
names(future_12_periods.df) <- c("Regre.frcst", "MA.Residuals.frcst", "two_level_combined.frcst")
future_12_periods.df

# Use accuracy() function to identify common accuracy measures.
round(accuracy(tot.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

# Use ets() function with model = "ZZZ", i.e., automated selection of error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
holtw <- ets(train.ts, model = "ZZZ")
holtw

# Use forecast() function to make predictions using this HW model with validation period (nValid). 
# Show predictions in tabular format.
holtw.predict <- forecast(holtw, h = nValid, level = 0)
holtw.predict

# Create Holt-Winter's exponential smoothing (HW) for entire data set. 
# Use ets() function with model = "ZZZ", to identify the best hw option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
holtw2 <- ets(sales.ts, model = "ZZZ")
holtw2

# Use forecast() function to make predictions using this HW model for 12 month into the future.
holtw2.predict <- forecast(holtw2, h = 12 , level = 0)
holtw2.predict

# Identify performance measures for HW forecast and compare it with others
round(accuracy(holtw.predict$mean, valid.ts), 3)
round(accuracy(holtw2.predict$fitted, sales.ts), 3)

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

# Plot ts data, linear trend and forecast for validation period.
plot(train.lin.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Linear Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Linear Regression for Training Data",
                               "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022.5, 18500, "Validation")
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)


## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

# Plot ts data, regression with quadratic trend and forecast for validation period.
plot(train.quad.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Quadratic Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Quadratic Trend for Training Data",
                               "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022.5, 18500, "Validation")
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow

# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quadratic (polynomial) trend.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

# Plot ts data, regression with seasonality and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Seasonality Model for Training Data",
                               "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022.5, 18500, "Validation")
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow

# Use accuracy() function to identify common accuracy measures
# for regression models with (1) linear trend, (2) quadratic (polynomial) trend,
# and (3) seasonality.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, regression with linear trend and seasonality forecast for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Linear Trend and Seasonality Model for Training Data",
                               "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022.5, 18500, "Validation")
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow



# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) quadratic  
# (polynomial) trend, (3) seasonality, and (4) linear trend and seasonality.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Plot ts data, regression with quadratic trend and seasonality and forecast for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Quadratic Trend and Seasonality Model for Training Data",
                               "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022.5, 18500, "Validation")
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow



# Use accuracy() function to identify common accuracy measures
# for all the above regression models
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

# Use tslm() function to create regression model with linear trend 
# and seasonality for the entire data
lin.season <- tslm(sales.ts ~ trend + season)
# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 12 future periods.
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Plot ts data, regression with quadratic trend and seasonality and forecast for validation period.
plot(lin.season.pred$mean, 
     xlab = "Time", ylab = "Sales", 
     ylim = c(8000, 19000), bty = "l",
     xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)))
lines(lin.season.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts)
legend(2013,17000, legend = c("Sales Time Series", 
                              "Linear Trend and Seasonality Model for Entire Data",
                              "Linear and Seasonality Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))
text(2017, 18500, "Data Set")  # Adjusted text position for clarity
text(2024.3, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures
round(accuracy(lin.season.pred$fitted, sales.ts),3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)


## TEST PREDICTABILITY OF ALOCOHOLIC BEVERAGES SALES.



# Use Arima() function to fit AR(1) model for Beverage sales.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
sales.ar1<- Arima(sales.ts, order = c(1,0,0))
summary(sales.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.6810
s.e. <- 0.0656
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# Create first differenced Beverage sales data using lag1.
diff.sales.ts <- diff(sales.ts, lag = 1)
diff.sales.ts

# Use Acf() function to identify autocorrealtion for first differenced 
# Beverage sales, and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.sales.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Beverage sales Data")

#-------------------------------------------------

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY. 
## USE Acf() FUNCTION TO IDENTIFY AUTOCORRELATION FOR RESIDUALS.
## PLOT RESIDUALS.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred


# Plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
# Plot ts data, regression with linear trend and seasonality forecast for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Linear Trend and Seasonality Model for Training Data",
                               "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022, 18500, "Validation")
text(2023.9, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2023, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Plot residuals of the predictions with trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-2000, 5000), bty = "l",
     xlim = c(2013, 2024.6), xaxt = "n",
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)))
lines( train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(-2500, 4500))
lines(c(2021, 2021), c(-2500, 4500))
text(2017, 4500, "Training")  # Adjusted text position for clarity
text(2021.5, 4500, "Validation")
text(2023.9, 4500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 4000, 2021, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 4000, 2023, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 4000, 2025, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Sales Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Sales Validation Residuals")

#ARIMA

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.season$fitted, 
                             train.lin.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Sales", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df


# Plot residuals of the predictions for training data before AR(1).
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-2000, 5000), bty = "l",
     xlim = c(2013, 2023), xaxt = "n",
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)))
lines( train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(-2500, 4500))
text(2017, 4500, "Training")  # Adjusted text position for clarity
arrows(2012.6, 4000, 2021, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 4000, 2021, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow

# Plot residuals of the residuals for training data after AR(1).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-2000, 5000), bty = "l",
     xlim = c(2013, 2024.6), xaxt = "n",
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)))
lines(train.lin.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(-2500, 4500))
text(2017, 4500, "Training")  # Adjusted text position for clarity
arrows(2012.6, 4000, 2021, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 4000, 2021, 4000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Sales Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Sales", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend and Seasonality + AR(1) for Residuals", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Regression for Training Data",
                               "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022, 18500, "Validation")
text(2023.9, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2023, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(1) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)


# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(sales.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
lin.season.pred <- forecast(lin.season, h = 12, level = 0)
lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(1) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.

plot(sales.ts, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend and Seasonality + AR(1) for Residuals", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(lin.season$fitted + residual.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Series for Training and Validation Periods", "Two-Level Forecast for Training and Validation Periods",
                               "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022, 18500, "Validation")
text(2023.9, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2023, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow





# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (linear trend and seasonality model 
#     + AR(1) model for residuals),
# (2) linear trend and seasonality model only, and
# (3) seasonal naive forecast. 
cat("Two-level model")
round(accuracy(lin.season$fitted + residual.ar1$fitted, sales.ts), 3)
cat("Linear trend and seasonality")
round(accuracy(lin.season$fitted, sales.ts), 3)
cat("Seasonal naive forecast")
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)


#------------------------------------------------------------

#ARIMA MODELS:


## FIT Seasonal ARIMA(2,1,2)(1,1,2) MODEL.

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(2,1,2), 
                          seasonal = c(1,1,2)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

# Use Acf() function to create autocorrelation chart of ARIMA(2,1,2)(1,1,2) 
# model residuals.
Acf(train.arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2)(1,1,2) Model Residuals")


# Plot ts data, Seasonal ARIMA model, and predictions for validation period.
plot(train.arima.seas.pred, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "ARIMA(2,1,2)(1,1,2)[12] Model", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.arima.seas.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Seasonal ARIMA Forecast for Training Period",
                               "Seasonal ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022, 18500, "Validation")
text(2023.9, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2023, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


# Plot ts data, AUTO ARIMA model, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Auto ARIMA Model", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Auto ARIMA Forecast for Training Period",
                               "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(8000, 19000))
lines(c(2021, 2021), c(8000, 19000))
text(2017, 18500, "Training")  # Adjusted text position for clarity
text(2022, 18500, "Validation")
text(2023.9, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2021, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2021, 18000, 2023, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Use accuracy() function to identify common accuracy measures 
# for validation period forecast:
 
# (5) ARIMA(2,1,2)(1,1,2) model; and 
# (6) Auto ARIMA model.
round(accuracy(train.ar2.pred$mean, valid.ts), 3)
round(accuracy(train.ma2.pred$mean, valid.ts), 3)
round(accuracy(train.arma2.pred$mean, valid.ts), 3)
round(accuracy(train.arima.pred$mean, valid.ts), 3)
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

## FIT SEASONAL ARIMA AND AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use arima() function to fit seasonal ARIMA(2,1,2)(1,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(sales.ts, order = c(2,1,2), 
                    seasonal = c(1,1,2)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 12 periods. 
arima.seas.pred <- forecast(arima.seas, h = 12, level = 0)
arima.seas.pred

# Use Acf() function to create autocorrelation chart of seasonal ARIMA 
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (2,1,2)(1,1,2) Model Residuals")

#------------------------
# Plot historical data, predictions for historical data, and Seasonal 
# ARIMA forecast for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Seasonal ARIMA(2,1,2)(1,1,2)[12] Model for Entire Data Set", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2024, 1), labels = format(seq(2013, 2024, 1)) )
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Seasonal ARIMA Forecast",
                               "Seasonal ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))

text(2017, 18500, "Entire Dataset")  # Adjusted text position for clarity

text(2024.5, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow


# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(sales.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

#---------------------------------
# Plot historical data, predictions for historical data, and Auto 
# ARIMA forecast for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales", ylim = c(8000, 19000), 
     bty = "l", xlim = c(2013, 2024.6), xaxt = "n",
     main = "Auto ARIMA Model for Entire Dataset", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2013, 2023, 1), labels = format(seq(2013, 2023, 1)) )
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2013, 17000, legend = c("Sales Time Series", "Auto ARIMA Forecast",
                               "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2024, 2024), c(8000, 19000))

text(2017, 18500, "Entire Dataset")  # Adjusted text position for clarity

text(2024.5, 18500, "Future")  # Adjusted text position for clarity
arrows(2012.6, 18000, 2024, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted starting point for training arrow
arrows(2024, 18000, 2025, 18000, code = 3, length = 0.1,
       lwd = 1, angle = 30)  # Adjusted ending point for future arrow




# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
# (1) Seasonal ARIMA (2,1,2)(1,1,2) Model,
# (2) Auto ARIMA Model,
# (3) Seasonal naive forecast, and
# (4) Naive forecast.
round(accuracy(arima.seas.pred$fitted, sales.ts), 3)
round(accuracy(auto.arima.pred$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)  