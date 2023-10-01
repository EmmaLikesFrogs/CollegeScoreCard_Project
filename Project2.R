library(dplyr)
library(readr)
library(data.table)

# change your working directory for your local computer (file where the CollegeSchorecard_Raw_Data is held)
setwd('/Users/yuhanburgess/Documents/GitHub/DataMungingProject2')
# https://benwhalley.github.io/just-enough-r/multiple-raw-data-files.html

# creates a table with all the file names
raw_files <- data.frame(
  filename = list.files(
  'CollegeScorecard_Raw_Data_08032021'))

# creates a table with the file names and the path associated with it 
raw_file_paths <- raw_files  %>%
  mutate(filepath = paste0(
    'CollegeScorecard_Raw_Data_08032021/', filename))


# Depending on how many files we want to look at will determine which ones we combine togeth
# Some of the files do not have the same stricture time for each column (one column could be a double but then
# a character in the next file).

# combine_csv_files rbinds the files together even with these discrepencies 
combine_csv_files <- function(raw_file_paths) {
  # Initialize an empty list to store data frames
  df_list <- list()
  
  # Loop through each CSV file and read it into a data frame
  for (file_path in raw_file_paths[,2]) {
    #stringAsFactors doesn't assign strings to a facotr/ value (setting string to factor is more efficient)
    # may run a little less efficient but because of size of data, it should be okay
    df <- read.csv(file_path, stringsAsFactors = FALSE) 
    df_list <- append(df_list, list(df))
  }
  
  # Combine the data frames using rbindlist from the data.table package
  combined_df <- data.table::rbindlist(df_list, fill = TRUE)
  return(combined_df)
}


combined_df <- combine_csv_files(raw_file_paths)


# determining completeness of each column to figure out which ones may be best to start looking at 
total_col_entry <- nrow(combined_df)
completeness <- c()
for (i in 1:ncol(combined_df)){
  freq_NULL <- table(combined_df[,..i])
  count_of_null <- (freq_NULL['NULL']/total_col_entry)*100
  completeness <- append(completeness, count_of_null)
}
total_null_feq <- data.frame(colnames(combined_df),completeness)%>%
  # renaming the columns so it makes more sense (new name, old name)
  rename('column' = 'colnames.combined_df.',
         'Null freq' = 'completeness')

# determining threshold for which columns we may want to look at based of completeness of each column
subset_null_feq <-subset(x, x$`Null freq` <50)
