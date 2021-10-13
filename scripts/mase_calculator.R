library(smooth)

# Executing the "data_loader.R" script available at: https://github.com/rakshitha123/TSForecasting/tree/master/utils
source("data_loader.R")


# Creating training and test set
# Parameters
# input_file - A .tsf file that contains the values of all 12 series covering both training and test periods
# forecast_horizon  - expected forecast horizon
create_test_sets <- function(input_file, forecast_horizon){
  loaded_data <- convert_tsf_to_tsibble(input_file, "series_value", "series_name", "start_timestamp")
  dataset <- loaded_data[[1]]
  
  all_serie_names <- unique(dataset$series_name)
  
  training_set <- list()
  test_set <- matrix(0, nrow = length(all_serie_names), ncol = forecast_horizon)
  
  for(s in seq_along(all_serie_names)){
    series_data <- dataset[dataset$series_name == as.character(all_serie_names[s]), ]
    training_set[[s]] <- series_data[1:(nrow(series_data) - forecast_horizon),][["series_value"]]
    test_set[s,] <- series_data[(nrow(series_data) - forecast_horizon + 1):nrow(series_data),][["series_value"]]
  }
  
  list(training_set, data.frame(test_set), all_serie_names)
}


# Calculate mean MASE of the submitted forecasts
# Parameters
# input_file - A .tsf file that contains the values of all 12 series covering both training and test periods
# forecasts - A dataframe which contains the data you directly read from the submission
# forecast_horizon  - expected forecast horizon
calculate_mase <- function(input_file, forecasts, forecast_horizon){
  mean_mase <- ""
  
  output <- create_test_sets(input_file, forecast_horizon)
  training_data <- output[[1]]
  actual_data <- output[[2]]
  building_names <- output[[3]]
  
  tryCatch({
    if((nrow(forecasts) == length(building_names)) & (ncol(forecasts) == forecast_horizon + 1)){
      if(all(unique(as.character(forecasts[,1])) %in% building_names)){
        forecasts <- forecasts[order(forecasts[,1]),]
        required_forecasts <- forecasts[-1]
        required_forecasts <- data.frame(sapply(required_forecasts, as.numeric))
        
        if(sum(is.na(required_forecasts)) == 0 & sum(is.null(required_forecasts)) == 0){
          mase_per_series <- NULL
          
          for(k in 1 :nrow(required_forecasts)){
            # For a given series, MASE measures the performance of a model compared with the in-sample average performance of a 28 days ahead seasonal naive benchmark
            mase_per_series[k] <- MASE(as.numeric(actual_data[k,]), as.numeric(required_forecasts[k,]), mean(abs(diff(as.numeric(training_data[[k]]), lag = 2688, differences = 1)), na.rm = TRUE))
          }
          
          mean_mase <- mean(mase_per_series)
        }
      }
    }
  }, error = function(e){  
    mean_mase <- ""
  })
  
  print(paste("Mean MASE = ", mean_mase))
}


# Example Usage
# The following example shows how to calculate the mean MASE of the forecasts considering the test month as September 2020 (forecast horizon: 96 * 30 = 2880)

# For that, we use the sample_submission.csv file which we have provided you and adjust its dimensions to fit into September month
forecasts <- read.csv("sample_submission.csv", header = F)
forecasts <- forecasts[,1:2881]

# The file, "phase_1_data.tsf" contains the values of all 12 series upto 30th September 2020 and the function will split the series for training and test sets
# Training set contains data upto 31st August 2020 and test set contains the actual data corresponding with September 2020 which we use to evaluate the forecasts
# This function will return an empty string, if the forecasts contain any formatting errors/non-numeric values/missing values/wrong dimensions etc.
calculate_mase("phase_1_data.tsf", forecasts, 2880)
