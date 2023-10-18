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

df2 <- df1[, 1:450] # trimming df to look at range of variables that will mostlikly be used for testing

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
# look at requency of institutions that are open and those that 
# are closed 

# MAY NEED TO NORMALIZE THE DATA 
state_count <- filtered_df %>%
  count('STABBR')
state_count$zscore <- scale(state_count$freq)

state_count_plot <- state_count %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Total Institutions')+
  theme_classic()


open_instit <- filtered_df%>%
  filter(CURROPER == 1)%>%
  count('STABBR')
open_instit$zscore <- scale(open_instit$freq)

open_instit_plot <- open_instit %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Open Institutions')+
  theme_classic()

closed_instit <- filtered_df%>%
  filter(CURROPER == 0)%>%
  count('STABBR')
closed_instit$zscore <- scale(closed_instit$freq)

closed_instit_plot <- closed_instit %>%
  mutate(STABBR = fct_reorder(STABBR, zscore))%>%
  ggplot(aes(x=STABBR, y = zscore, fill= zscore)) + 
  geom_bar(stat = 'identity', alpha = 0.7, width = 0.5) + 
  coord_flip()+
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  xlab('')+
  ggtitle('Closed Institutions')+
  theme_classic()

library(gridExtra)

state_count_grob<- ggplotGrob(state_count_plot)
open_instit_grob<- ggplotGrob(open_instit_plot)
closed_instit_grob<- ggplotGrob(closed_instit_plot)
# Arrange the plots in a single window
combined_plots <- grid.arrange(state_count_grob, open_instit_grob, closed_instit_grob, nrow = 1)

# Display the combined plots
print(combined_plots)

# PCIP11: Percentage of degrees awarded in Computer & Information Sciences and Support Services
# PCIP14: Percentage of degrees awarded in Engineering.
# PCIP15: Percentage of degrees awarded in Engineering Technologies & Engineering-Related Fields.
# PCIP27: Percentage of degrees awarded in Mathematics & Statistics.
# PCIP41: Percentage of degrees awarded in Science Technologies/Technicians.

# CIP11CERT1  CIP11CERT2  CIP11CERT4  CIP11BACHL
# CIP11CERT1: Cert. <1 academic yr in Computer & Information Sciences & Support Services.
# CIP11CERT2: Cert. >1<2 academicyrs in Computer & Information Sciences & Support Services.
# CIP11ASSOC: Associate degree in Computer And Information Sciences And Support Services.
# CIP11CERT4: Award >2<4 academic yrs in Computer & Information Sciences & Support Services.
# CIP11BACHL: Bchlr's deg. in Computer & Information Sciences & Support Services.


# CIP14CERT1  CIP14CERT2  CIP14ASSOC  CIP14CERT4  CIP14BACHL
# same pattern as lines 16-20 but for engineering

# CIP15CERT1  CIP15CERT2  CIP15ASSOC  CIP15CERT4  CIP15BACHL
# same pattern as lines 16-20 but for Engineering Technologies & Engineering-Related Fields.

# CIP27CERT1  CIP27CERT2  CIP27ASSOC  CIP27CERT4  CIP27BACHL
# same pattern as lines 16-20 but for Mathematics & Statistics

# CIP41CERT1  CIP41CERT2  CIP41ASSOC  CIP41CERT4  CIP41BACHL
# same pattern as lines 16-20 but for Computer & Information Sciences & Support Services.