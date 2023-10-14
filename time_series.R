library(dplyr)
library(readr)
library(data.table)
library (ggplot2)
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

# combine_csv_files rbinds the files together even with these discrepancies 
combine_csv_files <- function(raw_file_paths) {
  # Initialize an empty list to store data frames
  df_list <- list()
  
  # Loop through each CSV file and read it into a data frame
  for (file_path in raw_file_paths[,2]) {
    
    # finds the start year of each file
    year_match <- regexpr("MERGED(\\d{4})", file_path)
    
    if (year_match != -1) {
      year <- as.integer(substr(file_path, year_match + 6, year_match + 9))
    } else {
      year <- NA
    }
    
    df <- read.csv(file_path, stringsAsFactors = FALSE) 
    
    # creates a new column with the year of the csv file.
    df$year <- year
    
    df_list <- append(df_list, list(df))
  }
  
  # Combine the data frames using rbindlist from the data.table package
  combined_df <- data.table::rbindlist(df_list, fill = TRUE)
  return(combined_df)
}


# testing of time series graph of enrollment of different branches of University of California that is
# predomiinately undergraduate institution
combined_df<- combine_csv_files(raw_file_paths)%>%
  filter(year>2009)%>%
  filter(STABBR == 'CA')%>%
  filter(PREDDEG == 3)%>%
  filter(grepl('University of California', INSTNM))

# graph of enrollment of UC... form 2010 to present
# UGDS: Enrollment of undergraduate certificate/degree-seeking students
# INSTNM: Institution name
time_series <- ggplot(combined_df, aes(x = year, y = UGDS, group = INSTNM, color = INSTNM)) +
  geom_line()+labs(
    x = "Year",                 # X-axis label
    y = "Undergraduate Enrollment",  # Y-axis label
    title = "Enrollment Trend at University of California"  # Plot title
  )
time_series
