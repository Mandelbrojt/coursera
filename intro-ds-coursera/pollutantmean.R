pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
  ## Specify the folder name where data is stored
  mydir = "specdata"
  
  ## Create a list for all file names inside the folder
  myfiles = list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
  
  library(readr)
  ## Read all CSV files in the specified folder and store them in a list
  files_list <- sapply(myfiles[id], read_csv, simplify = FALSE)
  
  ## Merge all files into one data frame
  library(dplyr)
  raw_df <- bind_rows(files_list)
  
  ## Calculate the mean of the pollutant
  
  if (pollutant == "sulfate") {
    
    ## Create a df only for the pollutant specified
    sulfate_df <- data.frame(raw_df$Date, raw_df$sulfate, raw_df$ID)
    
    ## Delete NA values from the df
    nna_sulfate_df <- na.omit(sulfate_df)
    colnames(nna_sulfate_df) <- c("Date", "sulfate", "ID")
    
    ## Calculate the mean of pollutant across the specified monitor
    sulfate_avg <- mean(nna_sulfate_df$sulfate)
    print(sulfate_avg)
    
  } else if (pollutant == "nitrate") {
    
    ## Create a df only for the pollutant specified
    nitrate_df <- data.frame(raw_df$Date, raw_df$nitrate, raw_df$ID)
    
    ## Delete NA values from the df
    nna_nitrate_df <- na.omit(nitrate_df)
    colnames(nna_nitrate_df) <- c("Date", "nitrate", "ID")

    ## Calculate the mean of pollutant across the specified monitor
    nitrate_avg <- mean(nna_nitrate_df$nitrate)
    print(nitrate_avg)
  } else {
    print("Invalid value for pollutant")
  }
  
}
