library(tidyr)
library(dplyr)
library(psych)
library(MASS)
library(car)
library(mctest)
library(lubridate)
library(anytime)
library(moonBook)
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")


# Load directory and data
getwd()
setwd("./R/Data471/EDA")

# Age, Sex, Region
dataARG_full <- read.csv(
  file="ARG_Data.csv",
  head = T,
  fileEncoding = "UTF-8", #or "utf-16le",
  sep = ";" )  #or  "\t"

householdIncome <- read.csv(
  file="HouseholdIncome.csv",
  head = T,
  fileEncoding = "us-ascii", # "utf-16le", #or "UTF-8",
  sep = "\t") 


head(householdIncome)

#Set date column values to the same date format
d = parse_date_time(dataARG_full$Year.Month, c('%m%y', '%b%y', '%m%Y', '%b%Y'))
d[is.na(d)] = as.POSIXct(as.Date(as.numeric(dataARG_full$Year.Month[is.na(d)])), 
                         origin = "30-12-1899")
dataARG_full$Year.Month =as.Date(d)


# Filter data by age
dataARG = dataARG_full %>% filter(Age.Group.5Yr.Band %in% c("0 - 4 years inclusive","5 - 9 years inclusive","10 - 14 years inclusive","15 - 19 years inclusive"))
nrow(dataARG)



# Add new "Region" column to Household Income and ARG dataset
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

#ARG data
dataARG$Region2 <- NA
dataARG$Region2 <- ifelse(dataARG$Police.District == "Northland",
                          "Northland", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Auckland City",
                          "Auckland", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Counties/Manukau",
                          "Auckland", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Waitemata",
                          "Auckland", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Waikato",
                          "Waikato", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Eastern",
                          "Gisborne/Hawkes Bay", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Bay Of Plenty",
                          "Bay of Plenty", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Canterbury",
                          "Canterbury", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Tasman",
                          "Tasman/Nelson/Marlborough/West Coast", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Wellington",
                          "Wellington", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Central",
                          "Taranaki/Manawatu/Wanganui", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Southern",
                          "Southland/Otago", dataARG$Region2)
dataARG$Region2 <- ifelse(dataARG$Police.District == "Not Specified (District)",
                          "Not Specified", dataARG$Region2)

distinct(dataARG, dataARG$Region2)


#Set categorical variables as factors
dataARG$ANZSOC.Division = as.factor(dataARG$ANZSOC.Division)
dataARG$ANZSOC.Group = as.factor(dataARG$ANZSOC.Group)
dataARG$ANZSOC.Subdivision = as.factor(dataARG$ANZSOC.Subdivision)
dataARG$Method.of.Proceeding = as.factor(dataARG$Method.of.Proceeding)
dataARG$Mop.Division = as.factor(dataARG$Mop.Division)
dataARG$Mop.Group = as.factor(dataARG$Mop.Group)
dataARG$Mop.Subdivision = as.factor(dataARG$Mop.Subdivision)
dataARG$Person = as.factor(dataARG$Person)
dataARG$Police.Area = as.factor(dataARG$Police.Area)
dataARG$Police.District = as.factor(dataARG$Police.District)
dataARG$ROV.Division = as.factor(dataARG$ROV.Division)
dataARG$ROV.Group = as.factor(dataARG$ROV.Group)
dataARG$ROV.Subdivision = as.factor(dataARG$ROV.Subdivision)
dataARG$Region2 = as.factor(dataARG$Region2)
dataARG$Age.Group.5Yr.Band = as.factor(dataARG$Age.Group.5Yr.Band)
dataARG$Age.Group.5Yr.Band = factor(dataARG$Age.Group.5Yr.Band, levels = c("0 - 4 years inclusive","5 - 9 years inclusive","10 - 14 years inclusive","15 - 19 years inclusive"))


# Data summary
summary(dataARG)

# Number of offenses per year by Age Group (Youth Only)
offensesYearByAge = as.data.frame(table(dataARG$Year.Month, dataARG$Age.Group.5Yr.Band))
head(offensesYearByAge)


# Draw a time series, number of offenses per date by Age Group
ggplot(offensesYearByAge, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  xlab("Date")+ylab("Number of offenses")+
  scale_x_date(date_labels = "%b/%Y",date_breaks = "5 months")+ggtitle("Number of offenses by Age Group")+
  theme(axis.text.x=element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5, size = 10),text = element_text(size=10),legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm')) + 
  scale_colour_manual("Age Group", 
                      breaks = c("0 - 4 years inclusive","5 - 9 years inclusive", "10 - 14 years inclusive","15 - 19 years inclusive"),
                      values = c("blue", "red", "green", "purple")) 

# Number of offenses per year (Total All ages)
dataARG_full$Year.Month = as.Date(dataARG_full$Year.Month, format = "%b/%Y")
offensesYearTotal = as.data.frame(table(dataARG_full$Year.Month))
head(offensesYearTotal)

# Number of offenses per year (Total youth)
offensesYearYouth = as.data.frame(table(dataARG$Year.Month))

# Draw a time series, number of offenses per date bu all ages and youth
ggplot(offensesYearYouth, aes(x=as.Date(Var1), y=Freq, color = "Youth")) +
  geom_line()+ 
  geom_line(data=offensesYearTotal, aes(color="All Ages")) +
  xlab("Date")+ylab("Number of offenses")+ggtitle("Number of Offenses by Youth and All ages")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1),plot.title = element_text(hjust = 0.5, size = 10),text = element_text(size=10),legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm')) +
  scale_colour_manual("Age Group", 
                      breaks = c("Youth", "All Ages"),
                      values = c("green","purple")) 




# Number of offenses per year by Type of offense
offensesYearByType = as.data.frame(table(dataARG$Year.Month, dataARG$ANZSOC.Division))
head(offensesYearByType)

###Change to a bar graph
# Draw a time series, number of offenses per date by Type of offense
ggplot(offensesYearByType, aes(x=as.Date(Var1), y=Freq, color = Var2)) +
  geom_line()+ 
  xlab("Date")+ylab("Number of offenses")+
  scale_x_date(date_labels = "%d%b/%Y",date_breaks = "5 months")+
  theme(axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, 'cm'))+
  labs(color="Region")


# Number of offenses per year by Region
offensesYearByRegion = as.data.frame(table(dataARG$Year.Month, dataARG$Region2))
head(offensesYearByRegion)


# Draw a time series, number of offenses per date by Region
ggplot(offensesYearByRegion, aes(x = as.Date(Var1), y = Freq, color = Var2)) +
  geom_line() +
  xlab("Date") + ylab("Number of offenses") +
  scale_x_date(date_labels = "%d%b/%Y", date_breaks = "5 months") +theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) +
  labs(color = "Region")

#Pie Chart Method of proceeding by age group
dataARG %>% 
  group_by(Age.Group.5Yr.Band, Mop.Division)  %>% 
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), 3)) %>%
  ggplot(aes(x = "", y = freq, fill = Mop.Division)) +
  geom_col(color = "black") +
  geom_text(
    aes(x = 1.6, label = scales::percent(freq, accuracy = .1)),
    position = position_stack(vjust = .5),
    size = 2
  ) +
  coord_polar(theta = "y") +
  theme_void() + facet_wrap(~ Age.Group.5Yr.Band) +ggtitle("Mop Division by Age Group")+ theme(
    legend.text = element_text(size = 5),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

#Pie Chart ROV by Age group
dataARG %>%
  group_by(Age.Group.5Yr.Band, ROV.Subdivision) %>%
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), 3)) %>%
  ggplot(aes(x = "", y = freq, fill = ROV.Subdivision)) +
  geom_col(color = "black") +ggtitle("ROV by Age group")+
  geom_text(
    aes(x = 1.6, label = scales::percent(freq, accuracy = .1)),
    position = position_stack(vjust = .5),
    size = 2
  ) +
  coord_polar(theta = "y") +
  theme_void() + facet_wrap(~ Age.Group.5Yr.Band) + theme(
    legend.text = element_text(size = 5),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

#bar Chart ANZOC by Age group
dataARG %>%
  group_by(Age.Group.5Yr.Band, ANZSOC.Division) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x =ANZSOC.Division, y = n, fill = ANZSOC.Division),stat = "identity") +
  coord_flip()+facet_wrap(~ Age.Group.5Yr.Band,scales = 'free')+theme_bw()+theme(axis.text.y =element_blank(), panel.grid.major = element_blank(),text = element_text(size=7),legend.key.size = unit(0.3, 'cm')) 




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
# Change in offense relative to changes in population - Stats NZ Census/Population estimates?
#  % of offense committed by youth as a percentage of overall offense
# Increase in Household Income vs Cost of Living/Inflation
#######