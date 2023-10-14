library(dplyr)
library(ggplot2)
names <-apply(dataframe, 2, function(x) sum(x !='NULL')/length(x) > 0.75)

# test1<- as.data.frame(combined_df)
trimmed_combined_df<-dataframe[, c(names)]

# filtering based of predominately undergrad schools
test1 <- subset(trimmed_combined_df, trimmed_combined_df$PREDDEG == 3)

test2 <- trimmed_combined_df %>%
  filter(PREDDEG == 3)%>%
  filter(year > 1999)

graphTest<- ggplot(test2, aes(x = ))
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