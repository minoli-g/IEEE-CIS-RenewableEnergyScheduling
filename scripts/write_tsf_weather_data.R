## Functions to download weather data files from http://www.bom.gov.au/climate/data/ and convert them into .tsf format.


# Function to stop execution and display error messages
# Parameters
# message - error message
# con - file connection (optional)
stop_execution <- function(message, con = NULL){
  stop(message)
  
  if(!is.null(con))
    close(con)
}


# Function to download data and covert them into .tsf format
# Parameters
# output_file_name - name of the .tsf file
# p_c_values - a vector of "p_c" parameters corresponding with the files that are required to be downloaded (see lines 128-134 for more details)
# relation - dataset name (provide as a string with one word)
# description - dataset description (provide as a vector containing one element for each sentence)
# frequency - dataset frequency, ex: daily, weekly, monthly, quarterly etc. (provide as a string with one word)
# missing - whether the dataset contains any missing values (TRUE or FALSE)
# attribute_names - a vector containing attribute names (optional)
# attribute_types - a vector containing attribute types (optional)
save_data_to_tsf <- function(output_file_name, p_c_values, relation, description, frequency, missing, attribute_names = NULL, attribute_types = NULL){
  # Information about the weather files that are required to be downloaded.
  download_info <- data.frame("type" = c(rep("max_temp", 2), rep("min_temp", 2), rep("rainfall", 3), rep("solar", 3)),
                              "type_code" = c(rep("IDCJAC0010", 2), rep("IDCJAC0011", 2), rep("IDCJAC0009", 3), rep("IDCJAC0016", 3)),
                              "p_nccObsCode" = c(rep(122, 2), rep(123, 2), rep(136, 3), rep(193, 3)),
                              "area" = c("olympic", "moorabbin", "olympic", "moorabbin", "olympic", "oakleigh", "moorabbin", "olympic", "oakleigh", "moorabbin"),
                              "station" = c(086338, 086077, 086338, 086077, 086338, 086088, 086077, 086338, 086088, 086077),
                              "p_c" = p_c_values
  )
  
  if(length(relation) != 1)
    stop_execution("The relation name should be a single word")
  
  if(length(frequency) != 1)
    stop_execution("The frequency should be provided as a single word")
  
  if(length(missing) != 1)
    stop_execution("missing should be either TRUE or FALSE")
  
  if(!is.null(attribute_names)){
    if(is.null(attribute_types) | (length(attribute_names) != length(attribute_types)))
      stop_execution("Please provide the type of each attribute")
    
    if(!all(attribute_names %in% colnames(download_info)))
      stop_execution("Download_info does not contain a column/s with specific attribute name/s")
  }
  
  con <- file(output_file_name)
  
  tryCatch(
    open(con, "a")  
    , error = function(e)  
      stop_execution(e)
  )
  
  write("# Dataset Information", con)
  
  for(info in description){
    write(paste0("# ", info), con)
  }
  
  write("#", con)
  
  write(paste0("@relation ", relation), con)
  
  write("@attribute series_name string", con) 
  write("@attribute start_timestamp date", con) 
  
  for(att in 1:length(attribute_names))
    write(paste0("@attribute ", attribute_names[att], " ", attribute_types[att]), con)
  
  write(paste0("@frequency ", frequency), con)
  
  if(missing)
    write(paste0("@missing ", "true"), con)
  else
    write(paste0("@missing ", "false"), con)
  
  write("@data", con)
  
  for(id in 1:nrow(download_info)){
    file_name <-  paste0(download_info[id, "type_code"], "_", download_info[id, "station"], "_1800_Data")
    csv_file_name <- paste0(file_name, ".csv")
    zip_file_name <- paste0(file_name, ".zip")
    
    if(!file.exists(csv_file_name)){ # Check whether the required .csv file is already there in the working directory
      if(!file.exists(zip_file_name)){ # Check whether the required .zip file is already there in the working directory. If not, download the .zip file from http://www.bom.gov.au/climate/data/
        tryCatch({
          download.file(paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=", download_info[id, "station"], "&p_c=", download_info[id, "p_c"], "&p_nccObsCode=", download_info[id, "p_nccObsCode"]), destfile = zip_file_name, mode = "wb")
        }, error = function(e)  
          stop_execution(e, con)
        )
      }
      
      unzip(zip_file_name)
    }
    
    data <- read.csv(csv_file_name, header = TRUE)
    
    # The following preprocessing steps are specific for the data downloaded from http://www.bom.gov.au/climate/data/.
    # If you are using this function with other data, then you may have to modify the following preprocessing steps according to your data.
    data <- data[data$Year >= 2016, ]
    series <- as.numeric(data[, 6])
    series[is.na(series)] <- '?'
    start_date <- paste0(data$Year[1], "-", sprintf("%02d", data$Month[1]), "-", sprintf("%02d", data$Day[1]), " 00-00-00")
    
    meta_data <- paste0("T",id, ":", start_date)
    
    if(!is.null(attribute_names)){
      for(att in attribute_names)
        meta_data <- paste0(meta_data, ":", download_info[id, att])
    }
    
    write(paste0(meta_data, ":", paste(series, collapse = ",")), con)
  }
  
  close(con)
}


# Usage of save_data_to_tsf function

# "p_c_values" refers to the value of "p_c" parameter in the URL that is used to download a file. An example URL is shown below. 
# http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=086338&p_c=-1490885034&p_nccObsCode=122
# A p_c value corresponding with a particular file is changning everyday and hence, we cannot hardcode them in this script.
# When you need to use this script, you have to replace "p_c_values" defined in line 135 with the valid p_c values corresponding with the files that need to be downloaded from http://www.bom.gov.au/climate/data.
# Otherwise, the script will not work.
# p_c_values vector will be directly added to the download_info dataframe defined in line 28 of save_data_to_tsf function.
# Hence, makesure that the p_c values inside the vector are in the same order as the rows of download_info.
p_c_values <- c(-1490885034, -1481884971, -1490885230, -1481885167, -1490887923, -1482266623, -1481887860, -1490902926, -1482281626, -1481902863)
description <- "This file contains information on 4 weather variables: maximum temperature, minimum temperature, rainfall and solar exposure corresponding with 3 areas: Melbourne (Olympic Park), Oakleigh (Metropolitan Golf Club) and Moorabbin Airport."
save_data_to_tsf("weather_data.tsf", p_c_values, "weather", description, "daily", TRUE, attribute_names = c("type", "area"), attribute_types = c("string", "string"))



