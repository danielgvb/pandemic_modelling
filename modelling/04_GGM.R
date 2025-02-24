# GGM script Covid

# required packages--------------------
library(readxl)
library(DIMORA)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging plots in a grid

# change directory
setwd('Github/pandemic_modelling/Data/silver/')

# 0. Load data-------------------------
df_m<- read.csv("covid_data_monthly.csv")
df_w <- read.csv("covid_data_weekly.csv")
df_d <- read.csv("covid_data.csv")

# 1. Example GGM-----------

# mothly
df_col <- df_m %>% filter(country == "Colombia") # filter dataframe
covid_series_col <- df_col$new_cases # select cases
ggm_toy <- GGM(covid_series_col, mt='base', display = T) # run model base
summary(ggm_toy) # summary model

# weekly
df_col_w <- df_w %>% filter(country == "Colombia")
covid_series_col_w <- df_col_w$new_cases
ggm_toy_w <- GGM(covid_series_col_w, mt='base', display = T)
summary(ggm_toy_w)

# daily
df_col_d <- df_d %>% filter(country == "Colombia")
covid_series_col_d <- df_col_d$new_cases
ggm_toy_d <- GGM(covid_series_col_d, mt='base', display = T)
summary(ggm_toy_d)

# example R2 and RMSE
pred_ggm_d <- predict(ggm_toy_d, newx = c(1:length(covid_series_col_d)))
pred_ggm_d <- ts(pred_ggm_d, start = start(covid_series_col_d), frequency = frequency(covid_series_col_d))
pred.inst_ggm_d <- make.instantaneous(pred_ggm_d)
pred.inst_ggm_d <- ts(pred.inst_ggm_d, start = start(covid_series_col_d), frequency = frequency(covid_series_col_d))



cumsum_actual <- cumsum(covid_series_col_d)
cumsum_fitted <- pred_ggm_d

# Calculate R² based on cumulative values
residuals <- cumsum_actual - cumsum_fitted
ss_res <- sum(residuals^2)
ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
# Calculate RMSE based on cumulative values
rmse <- sqrt(mean(residuals^2))
rmse


df_country <- df_d %>% filter(country == 'Panama')
covid_series <- df_country$new_cases
covid_series_cum <- df_country$cases

# Train GGM model on first n days
ggm_model <- GGM(covid_series, mt='base', display = TRUE)

# Forecast for the entire period
fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
# Check for NaN values

# Calculate cumulative sum of the actual and fitted values
cumsum_actual <- covid_series_cum
cumsum_fitted <- fitted_forecast

# Calculate R² based on cumulative values
residuals <- cumsum_actual - cumsum_fitted
ss_res <- sum(residuals^2)
ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
r2 <- 1 - (ss_res / ss_tot)

# Calculate RMSE based on cumulative values
rmse <- sqrt(mean(residuals^2))

rmse
r2

#2. functions-----------------------

# Define function to fit Bass model and generate plot for each country
plot_bm_model <- function(country_name, df, n =150) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Train BM model on first n days
  covid_series_train <- covid_series[1:n]
  bm_model <- BM(covid_series_train, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = c(1:length(covid_series)))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  
  # Create plot using ggplot2
  df_plot <- data.frame(
    Time = 1:length(covid_series),
    Actual = covid_series,
    Fitted = fitted_forecast_inst
  )
  
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Daily Cases") + xlab("Time (Days)") +
    theme_minimal()
  
  return(p)
}


plot_ggm_model <- function(country_name, df, n =150) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Train BM model on first n days
  covid_series_train <- covid_series[1:n]
  ggm_model <- GGM(covid_series_train, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = c(1:length(covid_series)))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  
  # Create plot using ggplot2
  df_plot <- data.frame(
    Time = 1:length(covid_series),
    Actual = covid_series,
    Fitted = fitted_forecast_inst
  )
  
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Daily Cases") + xlab("Time (Days)") +
    theme_minimal()
  
  return(p)
}


# Function to calculate R² and RMSE for BM
calculate_metrics_bm <- function(country_name, df) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Train BM model on first n days
  bm_model <- BM(covid_series, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = 1:length(covid_series))
  
  cumsum_actual <- covid_series_cum
  cumsum_fitted <- fitted_forecast
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}




#  Function to calculate R² and RMSE for GGM based on cumulative data
calculate_metrics_ggm <- function(country_name, df) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Train GGM model on first n days
  ggm_model <- GGM(covid_series, mt='base', display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Calculate cumulative sum of the actual and fitted values
  cumsum_actual <- covid_series_cum
  cumsum_fitted <- fitted_forecast
  
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}



unique_countries
calculate_metrics_ggm('Panama', df_d)

GGM()

# 2. BM on COVID---------------------

# countries
unique_countries <- unique(df_d$country)
unique_countries

## 2.1 Daily-------------------

length_series <- nrow(df_col_d)

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_d, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 2.2 Weekly-----------------------------------


length_series <- nrow(df_col_w)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_w, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 2.3 Monthly----------------------------------
length_series <- nrow(df_col)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_m, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)



# 3. GGM on COVID---------------------


## 2.1 Daily-------------------

length_series <- nrow(df_col_d)

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_d, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 3.2 Weekly-----------------------------------


length_series <- nrow(df_col_w)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_w, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 3.3 Monthly----------------------------------
length_series <- nrow(df_col)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_m, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


# 4. Metrics---------------

# Initialize empty data frames to store metrics
bm_metrics <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)
ggm_metrics <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)

## 4.1 Daily----------------


# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_d)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_d)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_d)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_d)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)

## 4.2 Weekly-------------------

# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_w)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_w)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_w)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_w)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)


## 4.3 Monthly-------------------

# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_m)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_m)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_m)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_m)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)
View(combined_metrics)







