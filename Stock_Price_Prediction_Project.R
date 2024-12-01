
# Load necessary libraries
if (!require("quantmod")) install.packages("quantmod")
if (!require("randomForest")) install.packages("randomForest")
if (!require("Metrics")) install.packages("Metrics")
if (!require("ggplot2")) install.packages("ggplot2")

library(quantmod)
library(randomForest)
library(Metrics)
library(ggplot2)

# Step 1: Download historical stock price data
getSymbols("AAPL", src = "yahoo", from = "2015-01-01", to = "2023-12-01")
data <- Cl(AAPL)

# Step 2: Feature engineering - Add technical indicators
data <- data.frame(Date = index(data), Close = coredata(data))
data$SMA <- SMA(data$Close, n = 10)
data$RSI <- RSI(data$Close)

# Step 3: Remove NA values generated due to indicator calculations
data <- na.omit(data)

# Step 4: Split data into training and testing sets
set.seed(123)  # For reproducibility
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Step 5: Train a random forest model
model <- randomForest(Close ~ SMA + RSI, data = train_data)

# Step 6: Make predictions
predictions <- predict(model, test_data)

# Step 7: Evaluate the model
rmse_value <- rmse(test_data$Close, predictions)
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")

# Step 8: Visualize results
ggplot() +
  geom_line(data = test_data, aes(x = Date, y = Close), color = "blue") +
  geom_line(data = test_data, aes(x = Date, y = predictions), color = "red") +
  labs(title = "Stock Price Prediction", x = "Date", y = "Price") +
  theme_minimal()
