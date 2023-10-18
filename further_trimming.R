library(dplyr)
library(ggplot2)


# names <-apply(dataframe, 2, function(x) sum(x !='NULL')/length(x) > 0.75)
# 
# # test1<- as.data.frame(combined_df)
# trimmed_combined_df<-dataframe[, c(names)]
# 
# # filtering based of predominately undergrad schools
# test1 <- subset(trimmed_combined_df, trimmed_combined_df$PREDDEG == 3)
# 
# test2 <- trimmed_combined_df %>%
#   filter(PREDDEG == 3)%>%
#   filter(year > 1999)

library(readr)
df <- read_csv("Documents/GitHub/DataMungingProject2/CollegeScorecard_Raw_Data_08032021/MERGED2021_22_PP.csv")

df1 <- df %>%
  mutate_all(~ifelse(. == "NULL", NA, .)) # changing all NULL into NA


# IGNORE IF YOU WANT TO LOOK AT ALL THE COLUMNS OR CHANGE TO FIT RANGE OF COLUMNS USED IN ANAYLSIS
# trimming df to look at range of variables that will most likly be used for testing
df2 <- df1[, 1:450] 

#####################################################
# testing
table(df$CCBASIC)
table(df2$CCBASIC)
hist(as.numeric(df2$CCBASIC))
total_na_values <- sum(is.na(df2$CCBASIC)) 
######################################################

# REMOVING ROWS THAT ARE 90% INCOMPLETE 
# Calculate the percentage of NA values for each row
row_na_percent <- rowSums(is.na(df2)) / ncol(df2) * 100
# Find rows with 90% or more NA values
rows_with_90_percent_or_more_na <- which(row_na_percent >= 90)
# Print the row numbers that meet the condition
print(df2[rows_with_90_percent_or_more_na, 4])

# return df with 90% incomplete data
na_df <- df2 %>%
  filter(row_number() %in% rows_with_90_percent_or_more_na)

# returns df that have rows that are less than 90% incomplete
filtered_df <- df2 %>%
  filter(!(row_number() %in% rows_with_90_percent_or_more_na))

##########################################################
# BARPLOT OF THE NUMBER OF COLLEGES IN EACH STATE
# look at frequency of institutions that are open and those that 
# are closed 

library(gridExtra)


# MAY NEED TO NORMALIZE THE DATA 
# obtains the frequency of institutions in each state 
state_count <- filtered_df %>%
  count('STABBR')
state_count$zscore <- scale(state_count$freq) # normalizing calculations using z-score

# creates a barplot of all the states and number of intuitions in each 
# in descending order
state_count_plot <- state_count %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Total Institutions')+
  theme_classic()

# filtering to only look at institutions that are operational 
open_instit <- filtered_df%>%
  filter(CURROPER == 1)%>%
  count('STABBR')
open_instit$zscore <- scale(open_instit$freq) # normalizing calculations using z-score

# creates a barplot of all the states and number of operational intuitions in each 
# in descending order
open_instit_plot <- open_instit %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Open Institutions')+
  theme_classic()

# filtering to only look at institutions that are non-operational 
closed_instit <- filtered_df%>%
  filter(CURROPER == 0)%>%
  count('STABBR')
closed_instit$zscore <- scale(closed_instit$freq)

# creates a barplot of all the states and number of non-operational intuitions in each 
# in descending order
closed_instit_plot <- closed_instit %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Closed Institutions')+
  theme_classic()

# each plot is in a list of 9. This code formats it back into a form that can be plotted
state_count_grob<- ggplotGrob(state_count_plot)
open_instit_grob<- ggplotGrob(open_instit_plot)
closed_instit_grob<- ggplotGrob(closed_instit_plot)

# Arrange the plots in a single window
combined_plots <- grid.arrange(state_count_grob, open_instit_grob, closed_instit_grob, nrow = 1)

# Display the combined plots
grid.arrange(state_count_grob, open_instit_grob, closed_instit_grob, nrow = 1)

##########################################################
# CARNEGIE CLASSIFICATION VS HIGHEST DEGREE

library(RColorBrewer) # used to select certain palette

# description of the numbers in HIGHDEG
# Highest degree awarded
# 0 Non-degree-granting
# 1 Certificate degree
# 2 Associate degree
# 3 Bachelor's degree
# 4 Graduate degree

high_deg_awarded <- c('Non-degree', 'Certificate', 
                      'Associate', 'Bachelor', 
                      'Graduate')

# description of the numbers in CCBASIC

# website contains discription of each number
#https://librarytechnology.org/libraries/carnegie/#:~:text=Level%2015%3A%20Doctoral%20Universities%3A%20Very%20High%20Research%20Activity
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
                    'Special Focus 4-Years: Other Special Focus Institutions','Tribal Colleges'
                    
)

# changing the numerical values of the columns to their categorical counterpart
# +1 is added to each since the count in the csv starts at 0 while the count in R starts at 1

# I am excluding values that contain -2 from the conversion to categorical cause there 
# is no association with -2.
filtered_df$CCBASIC[filtered_df$CCBASIC!= -2] <- cc_basic_score [as.numeric(filtered_df$CCBASIC[filtered_df$CCBASIC!= -2])+1]
filtered_df$HIGHDEG <- high_deg_awarded[as.numeric(filtered_df$HIGHDEG)+1]

# creating a table of just these two columns
heatmap_data <- table(filtered_df$CCBASIC, filtered_df$HIGHDEG)

# color palette can be found at here: 
# https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
my_color_palette <- colorRampPalette(brewer.pal(10,'RdBu'))(34) # Color palette 

# displaying heatmap between Carnegie classification and the highest degree offered
heatmap(heatmap_data, 
        Rowv = NA, # Do not cluster rows 
        Colv = NA, # Do not cluster columns 
        col = my_color_palette, 
        scale = "column", # Scale by column 
        main = "Carnegie classification vs. Highest degree offered", # title of heatmap
        cexCol = 0.8) # changing the text size of the degrees offered 
        
# ANALYSIS
# The majority of institutions with a CCBasic Score of -2 are usually non-degree-granting or certificate institutions.
