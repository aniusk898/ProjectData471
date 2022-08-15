library(tidyr)
library(plotly)
library(dplyr)
library(psych)
library(MASS)
library(car)
library(mctest)
library(lubridate)
library(moonBook)
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

# Load de directory and data
setwd("C:/Users/Ana/Desktop/Data471/ProjectData471/")
getwd()
dataAES = read.csv(
  "AES_Data.csv",
  head = T,
  fileEncoding = "UTF-8",
  sep = ";"
)
# Filter data by age
dataAES = dataAES %>% filter(Age.Group.5Yr.Band == "15 - 19 years inclusive")

#Set categorical variables as factors
dataAES$ANZSOC.Division = as.factor(dataAES$ANZSOC.Division)
dataAES$ANZSOC.Group = as.factor(dataAES$ANZSOC.Group)
dataAES$ANZSOC.Subdivision = as.factor(dataAES$ANZSOC.Subdivision)
data$Mop.Subdivision = as.factor(data$Mop.Subdivision)
dataAES$Ethnic.Group = as.factor(dataAES$Ethnic.Group)
dataAES$Ethnicity = as.factor(dataAES$Ethnicity)
dataAES$Mop.Division = as.factor(dataAES$Mop.Division)
dataAES$Person.Organisation = as.factor(dataAES$Person.Organisation)
dataAES$PROCEEDINGS_FREQUENCY = as.factor(dataAES$PROCEEDINGS_FREQUENCY)
dataAES$SEX = as.factor(dataAES$SEX)
dataAES$Year.Month = as.Date(dataAES$Year.Month, format = "%d/%m/%Y")

# Data summary
summary(dataAES)

# Number of crimes per year
crimesYear = as.data.frame(table(dataAES$Year.Month))

# Draw a time series, number of crimes per date
ggplot(crimesYear, aes(x=as.Date(Var1), y=Freq)) +
  geom_line(color = "#00AFBB", size = 1) + 
  xlab("Date")+ylab("Number of crimes")+scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+theme(axis.text.x=element_text(angle=90, hjust=1)) 

#Pie Chart Crimes rate per Gender
dataAES %>% group_by(SEX,ANZSOC.Division) %>% summarise(n=n()) %>% mutate(freq =round(n/sum(n),3) ) %>% ggplot(aes(x = "", y = freq, fill = ANZSOC.Division)) +
  geom_col(color = "black") +
  geom_text(aes(x = 1.6,label = scales::percent(freq, accuracy = .1)), position = position_stack(vjust = .5),size = 2) +
  coord_polar(theta = "y") +
  theme_void()+facet_wrap(~SEX)+theme(legend.text=element_text(size=5),legend.key.size = unit(0.3, 'cm'),legend.title = element_text(size=7)) 

#Contingency Table of ethnicity and gender
cont = table(dataAES$Ethnicity,dataAES$SEX)




