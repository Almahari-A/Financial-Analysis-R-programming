# Load necessary libraries
library(tidyverse)
library(lubridate)
library(quantmod)
library(moments)

# Load S&P 500 data
sp500_data <- read_csv("C:/Users/Ahmed Almahari/Desktop/a/SPX.csv")

# Load DGS10 data
dgs10_data <- read_csv("C:/Users/Ahmed Almahari/Desktop/a/DGS10 (1).csv")

# Convert DATE columns to Date type
sp500_data$DATE <- as.Date(sp500_data$DATE, format="%m/%d/%Y")
dgs10_data$DATE <- as.Date(dgs10_data$DATE, format="%Y-%m-%d")

# Select relevant columns and rename for clarity
sp500_data <- sp500_data %>%
  select(DATE, ADJ_CLOSE) %>%
  rename(StockPrice = ADJ_CLOSE)

dgs10_data <- dgs10_data %>%
  rename(InterestRate = DGS10)

# Ensure InterestRate is numeric
dgs10_data$InterestRate <- as.numeric(dgs10_data$InterestRate)

# Ensure both datasets are on the same frequency (monthly)
sp500_monthly <- sp500_data %>%
  mutate(year = year(DATE), month = month(DATE)) %>%
  group_by(year, month) %>%
  summarize(StockPrice = last(StockPrice, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DATE = make_date(year, month, 1)) %>%
  select(DATE, StockPrice)

dgs10_monthly <- dgs10_data %>%
  mutate(year = year(DATE), month = month(DATE)) %>%
  group_by(year, month) %>%
  summarize(InterestRate = last(InterestRate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DATE = make_date(year, month, 1)) %>%
  select(DATE, InterestRate)

# Merge datasets on DATE
monthly_data <- inner_join(sp500_monthly, dgs10_monthly, by = "DATE")

# Remove rows with missing values
monthly_data_clean <- na.omit(monthly_data)

# Inspect the merged data to ensure proper aggregation
print(head(monthly_data_clean))
summary(monthly_data_clean)

# Plot Stock Price Time Series
ggplot(monthly_data_clean, aes(x = DATE, y = StockPrice)) +
  geom_line() +
  labs(title = "Monthly S&P 500 Adjusted Closing Prices",
       x = "Date",
       y = "Stock Price")

# Plot Interest Rate Time Series with adjusted y-axis limits
ggplot(monthly_data_clean, aes(x = DATE, y = InterestRate)) +
  geom_line() +
  labs(title = "Monthly 10-Year Treasury Constant Maturity Rate",
       x = "Date",
       y = "Interest Rate") +
  scale_y_continuous(limits = c(min(monthly_data$InterestRate, na.rm = TRUE), max(monthly_data$InterestRate, na.rm = TRUE)))

# Plot ACF for Stock Prices
acf(monthly_data_clean$StockPrice, main="ACF of S&P 500 Adjusted Closing Prices")

# Plot ACF for Interest Rates
acf(monthly_data_clean$InterestRate, main="ACF of 10-Year Treasury Constant Maturity Rate")

# Load necessary library for ADF test
library(tseries)

# Conduct ADF test for S&P 500 Adjusted Closing Prices
adf_test_stock <- adf.test(monthly_data_clean$StockPrice, alternative = "stationary")

# Conduct ADF test for 10-Year Treasury Constant Maturity Rate
adf_test_interest <- adf.test(monthly_data_clean$InterestRate, alternative = "stationary")

# Print the results
print(adf_test_stock)
print(adf_test_interest)

# Differencing to achieve stationarity
monthly_data_diff <- monthly_data_clean %>%
  mutate(
    StockPrice_diff = c(NA, diff(StockPrice)),
    InterestRate_diff = c(NA, diff(InterestRate))
  ) %>%
  na.omit()

# Inspect the differenced data to ensure proper aggregation
print(head(monthly_data_diff))
summary(monthly_data_diff)

# Plot differenced Stock Price Time Series
ggplot(monthly_data_diff, aes(x = DATE, y = StockPrice_diff)) +
  geom_line() +
  labs(title = "Differenced Monthly S&P 500 Adjusted Closing Prices",
       x = "Date",
       y = "Differenced Stock Price")

# Plot differenced Interest Rate Time Series
ggplot(monthly_data_diff, aes(x = DATE, y = InterestRate_diff)) +
  geom_line() +
  labs(title = "Differenced Monthly 10-Year Treasury Constant Maturity Rate",
       x = "Date",
       y = "Differenced Interest Rate")

# Plot ACF for differenced Stock Prices
acf(monthly_data_diff$StockPrice_diff, main="ACF of Differenced S&P 500 Adjusted Closing Prices")

# Plot ACF for differenced Interest Rates
acf(monthly_data_diff$InterestRate_diff, main="ACF of Differenced 10-Year Treasury Constant Maturity Rate")

# Conduct ADF test for differenced S&P 500 Adjusted Closing Prices
adf_test_stock_diff <- adf.test(monthly_data_diff$StockPrice_diff, alternative = "stationary")

# Conduct ADF test for differenced 10-Year Treasury Constant Maturity Rate
adf_test_interest_diff <- adf.test(monthly_data_diff$InterestRate_diff, alternative = "stationary")

# Print the results of ADF tests
print(adf_test_stock_diff)
print(adf_test_interest_diff)

# Descriptive statistics for undifferenced data
descriptive_stats_undiff <- monthly_data_clean %>%
  summarize(
    StockPrice_Mean = mean(StockPrice, na.rm = TRUE),
    StockPrice_SD = sd(StockPrice, na.rm = TRUE),
    StockPrice_Skewness = skewness(StockPrice, na.rm = TRUE),
    StockPrice_Kurtosis = kurtosis(StockPrice, na.rm = TRUE),
    StockPrice_Median = median(StockPrice, na.rm = TRUE),
    StockPrice_Q1 = quantile(StockPrice, 0.25, na.rm = TRUE),
    StockPrice_Q3 = quantile(StockPrice, 0.75, na.rm = TRUE),
    InterestRate_Mean = mean(InterestRate, na.rm = TRUE),
    InterestRate_SD = sd(InterestRate, na.rm = TRUE),
    InterestRate_Skewness = skewness(InterestRate, na.rm = TRUE),
    InterestRate_Kurtosis = kurtosis(InterestRate, na.rm = TRUE),
    InterestRate_Median = median(InterestRate, na.rm = TRUE),
    InterestRate_Q1 = quantile(InterestRate, 0.25, na.rm = TRUE),
    InterestRate_Q3 = quantile(InterestRate, 0.75, na.rm = TRUE)
  )

print(descriptive_stats_undiff)


# Descriptive statistics for differenced data
descriptive_stats_diff <- monthly_data_diff %>%
  summarize(
    StockPrice_diff_Mean = mean(StockPrice_diff, na.rm = TRUE),
    StockPrice_diff_SD = sd(StockPrice_diff, na.rm = TRUE),
    StockPrice_diff_Skewness = skewness(StockPrice_diff, na.rm = TRUE),
    StockPrice_diff_Kurtosis = kurtosis(StockPrice_diff, na.rm = TRUE),
    StockPrice_diff_Median = median(StockPrice_diff, na.rm = TRUE),
    StockPrice_diff_Q1 = quantile(StockPrice_diff, 0.25, na.rm = TRUE),
    StockPrice_diff_Q3 = quantile(StockPrice_diff, 0.75, na.rm = TRUE),
    InterestRate_diff_Mean = mean(InterestRate_diff, na.rm = TRUE),
    InterestRate_diff_SD = sd(InterestRate_diff, na.rm = TRUE),
    InterestRate_diff_Skewness = skewness(InterestRate_diff, na.rm = TRUE),
    InterestRate_diff_Kurtosis = kurtosis(InterestRate_diff, na.rm = TRUE),
    InterestRate_diff_Median = median(InterestRate_diff, na.rm = TRUE),
    InterestRate_diff_Q1 = quantile(InterestRate_diff, 0.25, na.rm = TRUE),
    InterestRate_diff_Q3 = quantile(InterestRate_diff, 0.75, na.rm = TRUE)
  )

print(descriptive_stats_diff)

# Correlation test between differenced stock prices and differenced interest rates
cor_test_result <- cor.test(monthly_data_diff$StockPrice_diff, monthly_data_diff$InterestRate_diff, method = "pearson")

# Print the correlation test results
print(cor_test_result)


# Univariate Regression: StockPrice_diff ~ InterestRate_diff
univariate_model_diff <- lm(StockPrice_diff ~ InterestRate_diff, data = monthly_data_diff)
summary(univariate_model_diff)

# Load GDP data
gdp_data <- read_csv("C:/Users/Ahmed Almahari/Desktop/a/GDP.csv")

# Convert DATE column to Date type
gdp_data$DATE <- as.Date(gdp_data$DATE, format="%Y-%m-%d")

# Ensure GDP data is on the same frequency (monthly)
gdp_monthly <- gdp_data %>%
  mutate(year = year(DATE), month = month(DATE)) %>%
  group_by(year, month) %>%
  summarize(GDP = last(GDP, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DATE = make_date(year, month, 1)) %>%
  select(DATE, GDP)

# Merge GDP data with existing dataset
monthly_data <- monthly_data %>%
  inner_join(gdp_monthly, by = "DATE")

# Remove rows with missing values
monthly_data_clean <- na.omit(monthly_data)

# Conduct ADF test for undifferenced GDP data
adf_test_gdp <- adf.test(gdp_monthly$GDP, alternative = "stationary")

# Print the result
print(adf_test_gdp)

# Differencing GDP to achieve stationarity
gdp_monthly_diff <- gdp_monthly %>%
  mutate(GDP_diff = c(NA, diff(GDP))) %>%
  na.omit()

# Conduct ADF test for differenced GDP data
adf_test_gdp_diff <- adf.test(gdp_monthly_diff$GDP_diff, alternative = "stationary")

# Print the result
print(adf_test_gdp_diff)

# Second differencing GDP to achieve stationarity
gdp_monthly_diff2 <- gdp_monthly_diff %>%
  mutate(GDP_diff2 = c(NA, diff(GDP_diff))) %>%
  na.omit()

# Conduct ADF test for second differenced GDP data
adf_test_gdp_diff2 <- adf.test(gdp_monthly_diff2$GDP_diff2, alternative = "stationary")

# Print the result
print(adf_test_gdp_diff2)

# Plot original GDP data
ggplot(gdp_monthly, aes(x = DATE, y = GDP)) +
  geom_line() +
  labs(title = "Monthly GDP",
       x = "Date",
       y = "GDP")
# Plot second differenced GDP data
ggplot(gdp_monthly_diff2, aes(x = DATE, y = GDP_diff2)) +
  geom_line() +
  labs(title = "Second Differenced Monthly GDP",
       x = "Date",
       y = "Second Differenced GDP")

# Merge the second differenced GDP data with the existing differenced data
monthly_data_diff <- monthly_data_diff %>%
  inner_join(gdp_monthly_diff2 %>% select(DATE, GDP_diff2), by = "DATE")

# Multivariate Regression: StockPrice_diff ~ InterestRate_diff + GDP_diff2
multivariate_model_diff <- lm(StockPrice_diff ~ InterestRate_diff + GDP_diff2, data = monthly_data_diff)
summary(multivariate_model_diff)

