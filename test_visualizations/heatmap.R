library(readr)
library(dplyr)
library(ggplot2)

setwd('/Users/yuhanburgess/Documents/GitHub/DataMungingProject2')
score_df <- read_csv("csv_files/MERGED2021_22_PP.csv")

score_df <-score_df%>%
  group_by(INSTNM, PREDDEG, HIGHDEG)%>%
  summarize(Count = n())
score_df$Count <- as.factor(score_df$Count)

# PELL_COMP_ORIG_YR4_RT: Percent of students who received a Pell Grant at the institution and who completed in 4 years at original institution
# COMP_ORIG_YR4_RT: Percent completed within 4 years at original institution

heatplot <- ggplot(score_df, aes(x= PREDDEG, y=HIGHDEG)) + 
  geom_tile(aes(fill = Count), color = 'blue')+ 
  scale_fill_viridis_d() +  # Adjust the color scale as needed
  labs(
    x = "Predominant Degree",
    y = "Highest Degree",
    fill = "Count",
    title = "Heatmap of Institutions by Degree Count"
  ) +
  theme_minimal()

heatplot

