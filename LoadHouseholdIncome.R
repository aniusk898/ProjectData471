library(tidyr)
library(dplyr)
library(psych)
library(MASS)
library(car)
library(mctest)
library(lubridate)
library(anytime)
library(moonBook)
library(ggplot2)
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

# Load directory and data
getwd()
setwd("./R/Data471/EDA")



householdIncome <- read.csv(
  file="HouseholdIncome.csv",
  head = T,
  fileEncoding = "us-ascii", # "utf-16le", #or "UTF-8",
  sep = "\t") 

head(householdIncome)


################

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


# Average Household Income per Year by Region (no monthly breakdown)
# First we need to recalculate the averages for combined regions "Southland/Otago" and "Taranaki/Manawatu/Wanganui"

householdIncomeNew = householdIncome  %>%
  dplyr::select(Year, Region2, Region, Measure, Value..Flags) %>%
  filter(Measure %in% c("Number of households (000)", "Average Income from All Sources collected"))

householdIncomeNew[householdIncomeNew$Region2 =="Southland/Otago" & householdIncomeNew$Year==2021,]

householdIncomeNew = householdIncomeNew %>%
  pivot_wider(names_from=Measure, values_from=Value..Flags)

householdIncomeNew = householdIncomeNew %>%
  mutate(TotalIncome=`Average Income from All Sources collected` * `Number of households (000)`)


aveHouseholdIncome = householdIncomeNew %>% dplyr::select(Year, Region2,`Number of households (000)`, TotalIncome ) %>%
  group_by(Year, Region2) %>%
  summarise_at(c("Number of households (000)", "TotalIncome"), sum) %>%
  mutate(AveWeeklyIncome=TotalIncome/`Number of households (000)`)

# Test
aveHouseholdIncome[aveHouseholdIncome$Region2 =="Southland/Otago" & aveHouseholdIncome$Year==2021,]


# Draw a time series, average household income per Year by Region
ggplot(aveHouseholdIncome, aes(x=Year, y=AveWeeklyIncome, color = Region2)) +
  geom_line()+ 
  xlab("Year")+
  ylab("Average Weekly Household Income ($NZD)")+ 
  labs(color="Region")




#########################
