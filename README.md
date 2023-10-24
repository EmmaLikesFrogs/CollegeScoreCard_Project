# U.S. Education System Survey (2021-2022)

## About

Our project aims to analyse the data of the U.S. education system for the academic year 2021-2022. The goal is to provide transparency and empower students and families to make informed decisions about college choices. This project focuses on analyzing educational institution data, including factors like the number of institutions, predominant degree, demographic variables, Carnegie classification, and financial aspects.

## Data Source

The data is sourced from the [College Scorecard](https://collegescorecard.ed.gov) initiated by the U.S. Department of Education.
Download [CollegeScorecard.zip](https://ncf.instructure.com/courses/7450/files/712104/download?download_frd=1) and navigate inside the folder. In this analysis we will be working with 'MERGED2021_22_PP.csv' file which contains data for the academic year 2021-2022.

## Content

The project includes the following components:

1. **Data Preprocessing:**

   - Data preprocessing involves wrangling the data to filter and clean it for more precise analysis.

2. **Data Visualization:**
   - We will use bar plots, grouped bar plots, ridge plots, and heatmaps to visualize the data for better understanding.

3. **Analysis and Interpretation:**

   - We will provide analysis and interpretations for each visualization, discussing patterns, trends, and potential factors influencing the data.

## Libraries

To run this project, you'll need the following R libraries:

```R
library(dplyr)
library(ggplot2)
library(readr)
library(ggridges)
library(ggplotify)
library(gridExtra)
library(forcats)
library(viridis)
library(visdat)
```

## Data Filtering and Operations

We perform several data filtering and operations to prepare the dataset for analysis. These include handling NULL values, removing bottom rows, filtering columns by completeness, and converting columns to categorical data.

1. **Handling NULL Values:**

   - Replaces all occurrences of "NULL" values with a standardized "NA" representation. This conversion ensures uniformity in missing data representation.

2. **Removing Bottom Rows:**

   - The function identifies and removes the bottom rows of the dataset. These rows are typically considered to contain less relevant or critical information for analysis. Eliminating them can improve computational efficiency and focus the analysis on more informative data points.

3. **Filtering Columns by Completeness:**

   - Filters out columns that do not meet a specified completeness threshold, which is set at 70% in our implementation. This function helps eliminate variables with significant missing data that could potentially skew analysis results.

4. **Converting Columns to Categorical Data:**
 
   - Finally, the function incorporates the conversion of specific columns into categorical data. This conversion aids in ensuring that the dataset is appropriately formatted making it more suitable for further analysis.

5. **Removing Highly Incomplete Rows:**

   - Identifies and removes rows that are considered highly incomplete. By eliminating these rows, the dataset becomes more concise and focused on relevant data points.

6. **Limiting the Number of Columns:**

   - Restricts the dataset to include only the first 450 columns. This is implemented to reduce computational complexity, speed up the analysis, and narrow the scope of investigation to the most relevant attributes.
   

## Visualization
In this project we used bar plots, grouped bar plots, ridge plots, and heatmaps to visualize the data for better understanding. Every plot provides description and analysis of patternt observed.


## Conclusion:

In conclusion, our project's visualizations are not just aesthetically pleasing but serve as a means to extract valuable insights from the data. Understanding these patterns is invaluable for parents and students when choosing educational institutions. It empowers them to make informed decisions based on factors like the concentration of institutions, degree offerings, demographics, finansial aspects and more. Ultimately, our data analysis aims to enhance transparency and facilitate data-driven decision-making in the realm of U.S. education.
