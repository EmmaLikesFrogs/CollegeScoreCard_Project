library(dplyr)
library(ggplot2)
library(readr)
library(ggplotify)
library(gridExtra)
library(forcats)
library(viridis)

setwd('/Users/yuhanburgess/Documents/GitHub/DataMungingProject2')
df <- read_csv("CollegeScorecard_Raw_Data_08032021/MERGED2021_22_PP.csv")

df<-df %>%
  mutate_all(~ifelse(. == "NULL", NA, .))
df2 <- df[, 1:450] 

# general filtering of data for anaylsis
df_filter <- function(df){
  #REMOVING ROWS THAT ARE 90% INCOMPLETE 
  # Calculate the percentage of NA values for each row
  row_na_percent <- rowSums(is.na(df2)) / ncol(df2) * 100
  # Find rows with 90% or more NA values
  rows_with_90_percent_or_more_na <- which(row_na_percent >= 90)
  
  # MAY REMOVE LATER
  na_df <- df2 %>%
    filter(row_number() %in% rows_with_90_percent_or_more_na)
  
  # returns df that have rows that are less than 90% incomplete
  filtered_df <- df2 %>%
    filter(!(row_number() %in% rows_with_90_percent_or_more_na))
  
  pred_deg_awarded <- c('Not classified', 'Certificate', 
                        'Associate', 'Bachelor', 
                        'Graduate')
  
  high_deg_awarded <- c('Non-degree', 'Certificate', 
                        'Associate', 'Bachelor', 
                        'Graduate')
  
  cc_basic_score <- c('Associate: High Transfer-High Traditional', 'Associate: High Transfer-Mixed Traditional/nontraditional',
                      'Associate: High Transfer-High Nontraditional', 'Associate: Mixed Transfer/Career & Technical-High Traditional',
                      'Associate: Mixed Transfer/Career & Technical-Mixed Traditional/Nontraditional','Associate: Mixed Transfer/Career & Technical-High Nontraditional',
                      'Associate: High Career and Technical- High Traditional','Associate: High Career and Technical-Mixed Traditional/Nontraditional',
                      'Associate: High Career and Technical-High Nontraditional', 'Special Focus 2-Years- Health Professions',
                      'Special Focus 2-Years: Technical Professions','Special Focus 2-Years: Arts and Design',
                      'Special Focus 2-Years: Other Fields','Baccalaureate/Associate Colleges: Associate Dominate',
                      'Doctoral Universities: Very High Research Activity','Doctoral Universities: High Research Activity',
                      'Doctoral/Professional Universities','Masters Colleges and Universities: Larger Programs',
                      'Masters Colleges and Universities: Medium Programs', 'Master Colleges and Universities: Small Programs',
                      'Baccalaureate Colleges: Arts and Science Focus','Baccalaureate Colleges: Diverse Fields','Mixed Baccalaureate/ Associates',
                      'Special Focus 4-Years: Faith-Related Institutions','Special Focus 4-Years: Medical Schools and Centers',
                      'Special Focus 4-Years: Other Health Profession Schools','Special Focus 4-Years: Engineering Schools',
                      'Special Focus 4-Years: Other Related-Technology Related Schools','Special Focus 4-Years: Business and Management Schools',
                      'Special Focus 4-Years: Arts, Music, and Design School','Special Focus 4-Years: Law Schools',
                      'Special Focus 4-Years: Other Special Focus Institutions','Tribal Colleges')
       
  filtered_df$CCBASIC[filtered_df$CCBASIC!= -2] <- cc_basic_score [as.numeric(filtered_df$CCBASIC[filtered_df$CCBASIC!= -2])+1]
  filtered_df$HIGHDEG <- high_deg_awarded[as.numeric(filtered_df$HIGHDEG)+1]
  filtered_df$PREDDEG <- pred_deg_awarded[as.numeric(filtered_df$PREDDEG)+1]
  
 return(filtered_df) 
}

# function filters out data that does not 
# contain actual states 
state_isolation <- function (df){
  states <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 
              'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 
              'IL', 'IN', 'IA', 'KS', 'KY', 'LA',
              'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 
              'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 
              'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 
              'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 
              'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 
              'WI', 'WY')
  
  filtered_df <- df%>%
    filter(STABBR %in% states)
  
  return(filtered_df)
}

# function filters data set to be used in 
# bar_plot function
instit_operations <- function(df){
  filtered_df <- state_isolation(df)
  
  state_count <- filtered_df %>%
    count(STABBR)

  open_instit <- filtered_df%>%
    filter(CURROPER == 1)%>%
    count(STABBR)

  closed_instit <- filtered_df%>%
    filter(CURROPER == 0)%>%
    count(STABBR)
  
  # if you want to return more than one thing, you have to return a list 
  return(list(state_count = state_count, open_instit = open_instit, closed_instit= closed_instit))
}

# function filters data set to be used in 
# group_bar_plot function
group_bar_filter <- function(df){
  # retrieving two columns 
  state_class <- filtered_df[c('STABBR', 'PREDDEG')]

  # count occurenaces based on the degree and which state it is from
  unique_combo <- state_class%>%
    group_by(STABBR, state_class[2])%>%
    summarise(UniqueCount = n())
  
  # check to make sure we are only looking at states
  unique_combo <- state_isolation(unique_combo)
  # create a new column of z-scores just in case
  unique_combo$zscore <- scale(unique_combo$UniqueCount)[,1] # normalizing calculations using z-score
  
  return(unique_combo)
}

filtered_df<- df_filter(df2)
institut_ops <- instit_operations(filtered_df)
group_plot_filtered_df<-group_bar_filter(filtered_df)


# creating a barplot that looks at the number of institutions in each state 
bar_plot <- function(df) {
  titles <- c('Total Institutes', 'Open Institutes', 'Closed Institutes') # main title 
  ops_plots <- list() # list of 3 different graphs 
  
  # looking thorough total, open, and closed institution's df
  for (ops in 1:length(df)) {
    data <- df[[ops]] # getting index from list
    title <- titles[ops] # retrieving associated title
    
    # order the bars in descending order
    data <- data %>%
      mutate(STABBR = fct_reorder(STABBR, n))
    
    # 
    ops_plot <- ggplot(data, aes(x = STABBR, y = n, fill = n)) +
      geom_bar(stat = 'identity', # determines how the data should be summarized (height based on value of dataset)
               alpha = 0.7, 
               width = 0.5) +
      coord_flip() +
      scale_fill_viridis(discrete = FALSE, # coloring system is continuous 
                         option = 'viridis', # color palette used
                         direction = -1)+ # direction refers to the gradient direction
      xlab('') +
      ggtitle(title) +
      theme_classic() # background color of window
    
    ops_plots[[ops]] <- ops_plot # adding plot to a list 
  }
  
  # Combine the plots into one
  combined_plots <- grid.arrange(grobs = ops_plots, ncol = length(ops_plots))
  # Return the combined plot
  return(combined_plots)
}

# Call the bar_plot function
combined_plot <- bar_plot(institut_ops)

# creating a grouped bar plot that looks at the frequency of predominate degree 
# within each state and returns a pdf file that countains a graph of each state
group_bar_plot <- function(df) {
  plots <- lapply(unique(df$STABBR), function(state) {
    
    # uses built-in state.abb to group graph based on state 
    data_subset <- subset(df, STABBR == state) 
    
    cc_dist <- ggplot(data_subset, aes(x = STABBR, y = UniqueCount, fill = PREDDEG)) +
      geom_bar(stat = 'identity', 
               alpha = 0.7, # transparency of bars
               width = 0.5, # bar width
               position = 'dodge') + # each distinct bar is positioned next to each other
      facet_wrap(~STABBR, nrow = 10) +
      scale_fill_viridis(discrete = TRUE, 
                         option = 'viridis', # color palette being used in viridis library
                         direction = 1)+ # direction of gradient color
      # setting parameters for y-axis 
      scale_y_continuous(name = 'Predominate Degree', # y title
                         limits = c(0, 300), # the min and max of y-axis
                         breaks = seq(0,300, by = 25))+ # the incrementation markers of y-axis
      xlab('') + 
      theme_classic()+
      theme(legend.text = element_text(size = 6), # font text of variables in legend
            legend.position = 'right', # 
            axis.text.x = element_blank(),
            strip.text = element_text(size = 8))# the text on top of each graph
    
    return(cc_dist)
  })
  
  pdf("preddeg_per_state_plots.pdf")  # Output to a PDF file
  for (i in 1:length(plots)) {
    print(plots[[i]])
  }
  dev.off()  # Close PDF device
  
  return(plots)
  
}

# Call the group_bar_plot function
preddeg_plots <- group_bar_plot(group_plot_filtered_df)









