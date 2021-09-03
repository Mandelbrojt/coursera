complete <- function(directory = "specdata", id = 1:332) {
  my_dir <- directory
  monitor_id <- id
  
  ## Create a list with the folder files names
  files <- list.files(path = my_dir, pattern = "*.csv", full.names = TRUE)
  
  ## Read every CSV file within the specified monitor id
  library(readr)
  files_tibble <- sapply(files[monitor_id], read_csv, simplify = FALSE)
  
  ## Count the number of rows on each file
  complete_matrix <- matrix(ncol = 2, nrow = length(monitor_id))
  colnames(complete_matrix) <- c("monitor_id", "num_obs")
  rownames(complete_matrix) <- 1:length(monitor_id)
  
  for (r in 1:length(monitor_id)) {
    complete_matrix[r, "monitor_id"] <- monitor_id[r]
    complete_matrix[r, "num_obs"] <- sum(complete.cases((data.frame(files_tibble[r]))))
  }
  
  complete_df <-data.frame(complete_matrix)
  print(complete_df)
}
