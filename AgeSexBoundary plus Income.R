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


# Load directory and data
getwd()
setwd("./R/Data471/EDA")

# Age, Sex, Region
dataASG_full <- read.csv(
  file="ASG_data.csv",
  head = T,
  fileEncoding = "utf-16le", #or "UTF-8",
  sep = "\t")  #or ";"

householdIncome <- read.csv(
  file="HouseholdIncome.csv",
  head = T,
  fileEncoding = "us-ascii", # "utf-16le", #or "UTF-8",
  sep = "\t") 

head(householdIncome)

# Filter data by age
dataASG = dataASG_full %>% filter(Age.Group.5Yr.Band %in% c("15 - 19 years inclusive", "5 - 9 years inclusive", "10 - 14 years inclusive"))
nrow(dataASG)

# Add new "Region" column to Household Income and ASG dataset
# Household Income
householdIncome$Region2 <- NA
householdIncome$Region2 <- ifelse(householdIncome$Region == "Northland Region",
                                  "Northland", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Auckland Region",
                                  "Auckland", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Waikato Region",
                                  "Waikato", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Gisborne/Hawkes Bay Regions",
                                  "Gisborne/Hawkes Bay", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Bay of Plenty Region",
                                  "Bay of Plenty", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Canterbury Region",
                                  "Canterbury", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Tasman/Nelson/Marlborough/West Coast Regions",
                                  "Tasman/Nelson/Marlborough/West Coast", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Wellington Region",
                                  "Wellington", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Manawatu-Wanganui Region",
                                  "Taranaki/Manawatu/Wanganui", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Taranaki Region",
                                  "Taranaki/Manawatu/Wanganui", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Southland Region",
                                  "Southland/Otago", householdIncome$Region2)
householdIncome$Region2 <- ifelse(householdIncome$Region == "Otago Region",
                                  "Southland/Otago", householdIncome$Region2)

distinct(householdIncome, householdIncome$Region2)

#ASG data
dataASG$Region2 <- NA
dataASG$Region2 <- ifelse(dataASG$Police.District == "Northland",
                          "Northland", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Auckland City",
                          "Auckland", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Counties/Manukau",
                          "Auckland", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Waitemata",
                          "Auckland", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Waikato",
                          "Waikato", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Eastern",
                          "Gisborne/Hawkes Bay", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Bay Of Plenty",
                          "Bay of Plenty", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Canterbury",
                          "Canterbury", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Tasman",
                          "Tasman/Nelson/Marlborough/West Coast", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Wellington",
                          "Wellington", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Central",
                          "Taranaki/Manawatu/Wanganui", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Southern",
                          "Southland/Otago", dataASG$Region2)
dataASG$Region2 <- ifelse(dataASG$Police.District == "Not Specified (District)",
                          "Not Specified", dataASG$Region2)

distinct(dataASG, dataASG$Region2)


#Set categorical variables as factors
dataASG$ANZSOC.Division = as.factor(dataASG$ANZSOC.Division)
dataASG$ANZSOC.Group = as.factor(dataASG$ANZSOC.Group)
dataASG$ANZSOC.Subdivision = as.factor(dataASG$ANZSOC.Subdivision)
dataASG$Mop.Division = as.factor(dataASG$Mop.Division)
dataASG$Police.Area = as.factor(dataASG$Police.Area)
dataASG$Police.District = as.factor(dataASG$Police.District)
dataASG$SEX = as.factor(dataASG$SEX)
dataASG$Year.Month = as.Date(dataASG$Year.Month, format = "%d/%m/%Y")

# Data summary
summary(dataASG)

# Number of crimes per year by Age Group (Youth Only)
crimesYearByAge = as.data.frame(table(dataASG$Year.Month, dataASG$Age.Group.5Yr.Band))
head(crimesYearByAge)


# Draw a time series, number of crimes per date by Age Group
ggplot(crimesYearByAge, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  xlab("Date")+ylab("Number of crimes")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_colour_manual("Age Group", 
                      breaks = c("5 - 9 years inclusive", "10 - 14 years inclusive","15 - 19 years inclusive"),
                      values = c("red", "green", "blue")) 

# Number of crimes per year (Total)
dataASG_full$Year.Month = as.Date(dataASG_full$Year.Month, format = "%d/%m/%Y")
crimesYearTotal = as.data.frame(table(dataASG_full$Year.Month))
head(crimesYearTotal)

# Draw a time series, number of crimes per date by Age Group
ggplot(crimesYearByAge, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  geom_line(data=crimesYearTotal, aes(color="All Ages")) +
  xlab("Date")+ylab("Number of crimes")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_colour_manual("Age Group", 
                      breaks = c("5 - 9 years inclusive", "10 - 14 years inclusive","15 - 19 years inclusive", "All Ages"),
                      values = c("red", "green", "blue","purple")) 




# Number of crimes per year by Type of Crime
crimesYearByType = as.data.frame(table(dataASG$Year.Month, dataASG$ANZSOC.Division))
head(crimesYearByType)


# Draw a time series, number of crimes per date by Type of Crime
ggplot(crimesYearByType, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  xlab("Date")+ylab("Number of crimes")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(color="Region")


# Number of crimes per year by Region
crimesYearByRegion = as.data.frame(table(dataASG$Year.Month, dataASG$Region2))
head(crimesYearByRegion)


# Draw a time series, number of crimes per date by Region
ggplot(crimesYearByRegion, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  xlab("Date")+ylab("Number of crimes")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(color="Region")




# Average Household Income per Year by Region (no monthly breakdown)
aveHouseholdIncome <- householdIncome %>%
dplyr::select(Year, Region2, Value..Flags) %>%
  filter(householdIncome$Measure == "Average Income from All Sources collected")

# Draw a time series, average household income per Year by Region
ggplot(aveHouseholdIncome, aes(x=Year, y=Value..Flags, color = Region2)) +
  geom_line()+ 
  xlab("Year")+
  ylab("Average Weekly Household Income ($NZD)")+ 
  labs(color="Region")
  


####### Notes/other things to look into 
# Change in crime relative to changes in population - Stats NZ Census/Population estimates?
#  % of crime committed by youth as a percentage of overall crime
# Increase in Household Income vs Cost of Living/Inflation
#######