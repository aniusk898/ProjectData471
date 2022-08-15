library(tidyr)
library(plotly)
library(dplyr)
library(psych)
library(purrr)
library(MASS)
library(car)
library(mctest)


data = read.csv(
  "C:/Users/Ana/Desktop/Data471/ProjectData471/proceedings.csv",
  head = T,
  fileEncoding = "UTF-8",
  sep = ";"
)
data = data %>% filter(Age.Group.5Yr.Band == "15 - 19 years inclusive")
data = data %>% dplyr::select(-c(Table.1, Age.Group, X..Variance, Months.Ago, Number.of.Records,Age.Group.5Yr.Band))
data$ANZSOC.Division = as.factor(data$ANZSOC.Division)
data$Method.of.Proceeding = as.factor(data$Method.of.Proceeding)
data$Mop.Subdivision = as.factor(data$Mop.Subdivision)
data$ANZSOC.Group = as.factor(data$ANZSOC.Group)
data$ANZSOC.Subdivision = as.factor(data$ANZSOC.Subdivision)
data$Previous.Period = as.factor(data$Previous.Period)
data$Ethnic.Group = as.factor(data$Ethnic.Group)
data$Ethnicity = as.factor(data$Ethnicity)
data$Mop.Division = as.factor(data$Mop.Division)
data$Mop.Group = as.factor(data$Mop.Group)
data$Person.Organisation = as.factor(data$Person.Organisation)
data$Police.Area = as.factor(data$Police.Area)
data$Police.District = as.factor(data$Police.District)
data$Police.District = as.factor(data$Police.District)
summary(data)
plot_ly(data, labels = ~Police.District, type = 'pie')

table(data$Ethnicity, data$ANZSOC.Group)
hist(data$Police.District)
