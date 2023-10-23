library(dplyr)
library(ggplot2)
library(readr)
df <- read_csv("Documents/GitHub/DataMungingProject2/CollegeScorecard_Raw_Data_08032021/MERGED2021_22_PP.csv")

##########################################################
# VISUALLY REPRESENTS MISSING VALUES
install.packages('visdat')
library(visdat)

df<-df %>%
  mutate_all(~ifelse(. == "NULL", NA, .))
vis_miss(df, warn_large_data = FALSE)
##########################################################

# IGNORE IF YOU WANT TO LOOK AT ALL THE COLUMNS OR CHANGE TO FIT RANGE OF COLUMNS USED IN ANAYLSIS
# trimming df to look at range of variables that will most likly be used for testing
df2 <- df[, 1:450] 

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


vis_miss(filtered_df, warn_large_data = FALSE)

##########################################################
# CARNEGIE CLASSIFICATION VS HIGHEST DEGREE

library(RColorBrewer) # used to select certain palette

# description of the numbers in PREDDEG
# Predominant undergraduate degree awarded
# 0 Not classified
# 1 Predominantly certificate-degree granting
# 2 Predominantly associate's-degree granting
# 3 Predominantly bachelor's-degree granting
# 4 Entirely graduate-degree granting

pred_deg_awarded <- c('Not classified', 'Certificate', 
                      'Associate', 'Bachelor', 
                      'Graduate')

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
filtered_df$PREDDEG <- pred_deg_awarded[as.numeric(filtered_df$PREDDEG)+1]

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


# creating a table of just these two columns
heatmap_data_preddeg <- table(filtered_df$CCBASIC, filtered_df$PREDDEG)

vis_miss(heatmap_data_preddeg, warn_large_data = FALSE)

# displaying heatmap between Carnegie classification and the highest degree offered
heatmap(heatmap_data_preddeg, 
        Rowv = NA, # Do not cluster rows 
        Colv = NA, # Do not cluster columns 
        col = my_color_palette, 
        scale = "column", # Scale by column 
        main = "Carnegie classification vs. Predominate degree offered", # title of heatmap
        cexCol = 0.8) # changing the text size of the degrees offered 

# ANALYSIS
# The majority of institutions with a CCBasic Score of -2 are usually non-degree-granting or certificate institutions.

##########################################################
# # PERCENTAGE OF DEGREES AWARDED BASED 
# instit_info <- filtered_df[, 4:24]
# pcip_values <- filtered_df[, 62:99] 
# 
# degrees_awarded_df <- cbind(instit_info,pcip_values)
# 
# special_focus_instit <- grep('Special Focus 4', degrees_awarded_df$CCBASIC, value = FALSE)
# degrees_awarded_specialfocus_df<- degrees_awarded_df[special_focus_instit,]
# 
# row_na <- rowSums(is.na(degrees_awarded_specialfocus_df[21:59])) / ncol(degrees_awarded_specialfocus_df[21:59]) * 100
# # Find rows with 90% or more NA values
# rows_with_90_or_more_na <- which(row_na >= 90)
# 
# degrees_awarded_specialfocus_df <- degrees_awarded_specialfocus_df %>%
#   filter(!(row_number() %in% rows_with_90_or_more_na))
# 
# special_focus_engin <- grep('Engineering', degrees_awarded_specialfocus_df$CCBASIC, value = FALSE)
# enginner_group<- degrees_awarded_specialfocus_df[special_focus_engin,]
# vis_miss(enginner_group, warn_large_data = FALSE)
# hist(as.numeric(enginner_group$PCIP01))

##########################################################
# DENSITY MAP, LOOKING AT DISTRIBUTION OF STUDENTS OF DIFFERENT RACES 
# AT VARIOUS 4 YEAR AND BACCALAURATE INSTIUTIONS

install.packages('ggridges')
library(ggridges)
library(GGally)
library(gridExtra)
library(stringr)
# retrieving info of total share of enrollment of undergraduate 
# degree-seeking student of a certain ethnicity/race

# white: White
# black: Black
# hisp: Hispanic
# asian: Asian 
# aian: American Indian/ Alaska Native
# NHPI: Native Hawaiian/Pacific Islander
# 2More: two or more races
# nra: non-resident aliens
# unkn: race unkown

demograph <- filtered_df[,291:301]  # retrieving information about student demographic
pred_undergrad <- filtered_df[,4:24] # retrieving principal information about institution

# this anaylsis looks specifically at special focus 4-year institutions and those that are considered baccalaureate
bach_stud <- cbind(pred_undergrad,demograph)%>%
  filter(PREDDEG == 'Bachelor')%>%
  filter(str_detect(CCBASIC, 'Special Focus 4-Years') | str_detect(CCBASIC, 'Baccalaureate'))

vis_miss(demograph, warn_large_data = FALSE) # visual representation of missing and non-missing data


dem_group <-colnames(bach_stud[,24:32]) # getting the column names of the demographics that are listed 

# creates a density plot for each column of the demographic background
for(dem in dem_group){
  print(ggplot(bach_stud, aes(x = as.numeric(.data[[dem]]), y = CCBASIC, fill = CCBASIC))+
          geom_density_ridges()+ # name of the density plot in r
          scale_fill_brewer(palette = 'RdYlBu')+ # the color system that is being used for each grouping
          theme_ridges()+ # sets various theme elements 
          scale_x_continuous(limits = c(0,1)) + # setting range between 0 and 1 since that is the range of these
          theme(legend.position = 'bottom', text = element_text(size = 8))+
          theme(axis.text.y = element_blank(), axis.title.y = element_blank())) # removes the y-axis label since we have a legend

}

# ANALYSIS
# 1. There is a greater distribution of those who identify as white throughout all categories which could be associated with
# majority of those who have access to higher education are those are white 
##########################################################

# BARPLOT OF THE NUMBER OF COLLEGES IN EACH STATE
# look at frequency of institutions that are open and those that 
# are closed 

library(gridExtra)
library(forcats)

# MAY NEED TO NORMALIZE THE DATA 
# obtains the frequency of institutions in each state 

# Assuming you have a data frame named filtered_df with a 'STABBR' column

state_count <- filtered_df %>%
  count(STABBR)
state_count$zscore <- scale(state_count$n)[,1] # normalizing calculations using z-score

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
  count(STABBR)
open_instit$zscore <- scale(open_instit$n)[,1]# normalizing calculations using z-score

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
  count(STABBR)
closed_instit$zscore <- scale(closed_instit$n)[,1]

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
# TESTING
table(df$CCBASIC)
table(df2$CCBASIC)
hist(as.numeric(df2$CCBASIC))
total_na_values <- sum(is.na(df2$CCBASIC)) 

##########################################################