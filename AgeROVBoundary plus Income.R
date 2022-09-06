options(warn=-1)
library(tidyr)
library(dplyr)
library(psych)
library(MASS)
library(car)
library(ggplot2)
library(mctest)
library(lubridate)
library(anytime)
library(tseries)
library(xts)
library(lmtest)
library(moonBook)
library(xtable)
library(smooth)
library(forecast)
library(seastests)
library(Kendall)




Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")


# Load directory and data
getwd()
#setwd("./R/Data471/EDA")
#setwd("C:/Users/Ana/Desktop/Data471/ProjectData471/")

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

#The age between 0-4 is so unrepresentative that is not necessary to be included
# Filter data by age
dataARG = dataARG_full %>% filter(Age.Group.5Yr.Band %in% c("5 - 9 years inclusive","10 - 14 years inclusive","15 - 19 years inclusive"))
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
dataARG$Age.Group.5Yr.Band = factor(
  dataARG$Age.Group.5Yr.Band,
  levels = c(
    "5 - 9 years inclusive",
    "10 - 14 years inclusive",
    "15 - 19 years inclusive"
  )
)
# Data dimensions after removing columns
dim(dataARG)

# Data summary in latex format
xtable(summary(dataARG), type = latex)

#Offences division
dataARG %>% group_by(ANZSOC.Division) %>% summarise(n = n()) %>%
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = ANZSOC.Division, y = pct, label = scales::percent(pct)))+ 
  geom_col(position = 'dodge', show.legend = FALSE)+ 
  geom_text(position = position_dodge(width = .3),    # move to center of bars
            vjust = 0.5,# nudge above top of bar
            hjust = 0,
            size = 1.5) + ggtitle("Total offences commited by youth")+
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0, size = 10),
    text = element_text(size = 7)
  )+ coord_flip()

#Contingency table ANZSOC Division vs ROV Group
xtable(table( dataARG$ANZSOC.Division,dataARG$ROV.Group), type = latex)

# Number of Offences per year by Age Group (Youth Only)
OffencesYearByAge = as.data.frame(table(dataARG$Year.Month, dataARG$Age.Group.5Yr.Band))
head(OffencesYearByAge)

#Offences by age 
ggplot(OffencesYearByAge, aes(x = as.Date(Var1), y = Freq)) +
  geom_line() +
  xlab("Date") + ylab("Number of Offences") +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "6 months") +
  ggtitle("Number of Offences per date by Age Group") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) + facet_wrap( ~ Var2, scales = "free")
  
#Proceeding according to offence
dataARG %>% group_by(Method.of.Proceeding, ANZSOC.Division) %>% summarise(n = n()) %>%
  ggplot(aes(fill = Method.of.Proceeding, y = n, x = ANZSOC.Division)) +
  geom_bar(position = "fill", stat = "identity")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm')
  )+coord_flip()

#ANZSOC by Region stacked chart
dataARG %>% 
  count(Region2, ANZSOC.Division) %>%
  group_by(Region2) %>%
  mutate(percentage = n/sum(n) * 100) %>%
  ggplot() + aes(Region2, percentage, fill =ANZSOC.Division, label = paste0(round(percentage, 2), "%")) + 
  geom_col() + ggtitle("ANZSOC DIvision by Region")+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 8),
    text = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm')
  ) +coord_flip()


#ANZSOC by Mop stacked chart
dataARG %>% 
  count(ANZSOC.Division, Method.of.Proceeding) %>%
  group_by(ANZSOC.Division) %>%
  mutate(percentage = n/sum(n) * 100) %>%
  ggplot() + aes(ANZSOC.Division, percentage, fill = Method.of.Proceeding, label = paste0(round(percentage, 2), "%")) + 
  geom_col() +ggtitle("Method of Proceeding by ANZSOC Division")+
  geom_text(position=position_stack(0.5), size=2)+ theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 8),
    text = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm')
  )+coord_flip()
  
#stacked chart ROV by Mop
dataARG %>% 
  count(ROV.Subdivision, Method.of.Proceeding) %>%
  group_by(ROV.Subdivision) %>%
  mutate(percentage = n/sum(n) * 100) %>%
  ggplot() + aes(ROV.Subdivision, percentage, fill = Method.of.Proceeding, label = paste0(round(percentage, 2), "%")) + 
  geom_col() +ggtitle("Method of Proceeding by ROV Subdivision")+
  geom_text(position=position_stack(0.5), size=2)+ theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.3, 'cm')
  ) +coord_flip()+scale_fill_grey(start = 0.2, end = 0.95)

# Top 5 more commited offenses by age
xtable(dataARG %>% group_by(Age.Group.5Yr.Band, ANZSOC.Division) %>% summarise(n =
                                                                          n()) %>% arrange(desc(n)) %>% slice(1:5), type = latex)

# Offences According to the number of proceedings

topProceed = dataARG %>% arrange(desc(Proceedings)) %>%slice(1:10) %>% group_by(ANZSOC.Division) 
ggplot( aes(x=ANZSOC.Division, y=n)) +
  geom_bar(stat="identity")+ ggtitle("ANZSOC Division offences proportions") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = -1, size = 10),
    text = element_text(size = 7)
  )+facet_wrap(~Proceedings, scales = "free")+coord_flip()

# Number of Offences per year (Total All ages)
dataARG_full$Year.Month = as.Date(dataARG_full$Year.Month, format = "%b/%Y")
OffencesYearTotal = as.data.frame(table(dataARG_full$Year.Month))
OffencesYearTotal$Var1 = ymd(OffencesYearTotal$Var1)
OffencesYearTotal
#offences variation per age group
five_Nine = dataARG %>% group_by(Age.Group.5Yr.Band,Year.Month) %>% summarise(n=n()) %>% filter(Age.Group.5Yr.Band == "5 - 9 years inclusive")
variation59 =(five_Nine$n[length(five_Nine$n)] - five_Nine$n[1]) /
  five_Nine$n[1]
#Offences year youth (all ages)
OffencesYearYouth = as.data.frame(table(dataARG$Year.Month))

# Draw a time series, number of Offences per date all ages and youth
ggplot(OffencesYearYouth, aes(x = as.Date(Var1), y = Freq, color = "Youth")) +
  geom_line() +
  geom_line(data = OffencesYearTotal, aes(color = "All Ages")) +
  xlab("Date") + ylab("Number of Offences") + ggtitle("Number of Offences by Youth and All ages") +
  scale_x_date(date_labels = "%d%b/%Y", date_breaks = "5 months") + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) +
  scale_colour_manual(
    "Age Group",
    breaks = c("Youth", "All Ages"),
    values = c("green", "purple")
  )

# Number of Offences per year by Type of offense
OffencesYearByType = as.data.frame(table(dataARG$Year.Month, dataARG$ANZSOC.Division))
head(OffencesYearByType)

# Draw a time series, number of Offences per date by Type of offense
ggplot(OffencesYearByType, aes(x = as.Date(Var1), y = Freq, color = Var2)) +
  geom_line() +
  xlab("Date") + ylab("Number of Offences") +
  scale_x_date(date_labels = "%d%b/%Y", date_breaks = "5 months") + theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) +
  labs(color = "ANZSOC Division")


# Number of Offences per year by Region
OffencesYearByRegion = as.data.frame(table(dataARG$Year.Month, dataARG$Region2))
head(OffencesYearByRegion)


# Draw a time series, number of Offences per date by Region
ggplot(OffencesYearByRegion, aes(x = as.Date(Var1), y = Freq, color = Var2)) +
  geom_line() +
  xlab("Date") + ylab("Number of Offences") +
  scale_x_date(date_labels = "%d%b/%Y", date_breaks = "5 months") + theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.text = element_text(size = 5),
    legend.key.size = unit(0.3, 'cm')
  ) +
  labs(color = "Region")

#Pie Chart Method of proceeding Subdivision by age group
dataARG %>%
  group_by(Age.Group.5Yr.Band, Mop.Subdivision)  %>%
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), 3)) %>%
  ggplot(aes(x = "", y = freq, fill = Mop.Subdivision)) +
  geom_col(color = "black") +
  geom_text(
    aes(x = 1.7, label = scales::percent(freq, accuracy = .1), angle = 45),
    position = position_stack(vjust = .5),
    size = 2.5,
    
  ) +
  coord_polar(theta = "y") +
  theme_void() + facet_wrap( ~ Age.Group.5Yr.Band) + ggtitle("Mop Subdivision by Age Group") + theme(,
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 10)
  )+scale_fill_grey(start = 0, end = 0.95)

#Pie Chart ROV by Age group
dataARG %>%
  group_by(Age.Group.5Yr.Band, ROV.Subdivision) %>%
  summarise(n = n()) %>% mutate(freq = round(n / sum(n), 3)) %>%
  ggplot(aes(x = "", y = freq, fill = ROV.Subdivision)) +
  geom_col(color = "black") + ggtitle("ROV by Age group") +
  geom_text(
    aes(x = 1.6, label = scales::percent(freq, accuracy = .1)),
    position = position_stack(vjust = .5),
    size = 2
  ) +
  coord_polar(theta = "y") +
  theme_void() + facet_wrap( ~ Age.Group.5Yr.Band) + theme(
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
  geom_bar(aes(x = ANZSOC.Division, y = n, fill = ANZSOC.Division), stat = "identity") +
  ylab("Number of Offences") +
  coord_flip() + facet_wrap( ~ Age.Group.5Yr.Band, scales = 'free') + theme_bw() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  )

# Average Household Income per Year by Region (no monthly breakdown)
aveHouseholdIncome <- householdIncome %>%
  dplyr::select(Year, Region2, Value..Flags) %>%
  filter(householdIncome$Measure == "Average Income from All Sources collected")

# Draw a time series, average household income per Year by Region
ggplot(aveHouseholdIncome,
       aes(x = Year, y = Value..Flags, color = Region2)) +
  geom_line() +
  xlab("Year") +
  ylab("Average Weekly Household Income ($NZD)") +
  labs(color = "Region")

###############################################################################################
# TIME SERIES ANALYSIS
###########################################################################
#Create time series object
totalOffencesTS = ts(
  OffencesYearTotal$Freq,
  frequency = 12,
  start = c(2014, 7),
  end = c(2022, 6)
)

# Plot time series
A = autoplot(totalOffencesTS, ts.scale = TRUE) + xlab("Date") +
  ylab("Total Offences") + ggtitle("Total Offences Time series") + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  )
A$scales$scales[[1]]$breaks <- scales::extended_breaks(8)
A



# Decompose time series
decomp = decompose(totalOffencesTS)

D = autoplot(decompose(totalOffencesTS)) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  )
D$scales$scales[[1]]$breaks <- scales::extended_breaks(8)
D

#Augmented dicker fuller test to test stationarity
adf.test(totalOffencesTS)

#Mackendall to evaluate monotony
MannKendall(totalOffencesTS)

# forecasting model using arima model
arimaModel <- auto.arima(totalOffencesTS)
arPlot = autoplot(arimaModel$fitted, color = "red") + xlab("Date") +
  ylab("Total Offences") + ggtitle("Total Offences Time series") + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) +
  geom_line(data = totalOffencesTS, color = "black")
arPlot$scales$scales[[1]]$breaks <- scales::extended_breaks(8)
arPlot

#Residuals Autocorrelation 
arima_Residuals = arimaModel$residuals
acfSerie = stats::acf(arima_Residuals, lag.max = 32,plot = FALSE)
pacfSerie = stats::pacf(arima_Residuals, lag.max = 32,plot = FALSE)

acf = autoplot(acfSerie) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) + ggtitle("Arima residuals ACF")
acf$scales$scales[[1]]$breaks <- scales::extended_breaks(8)
acf 
pac = autoplot(pacfSerie) + theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10),
    text = element_text(size = 10),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm')
  ) + ggtitle("Arima residuals PACF")
pac$scales$scales[[1]]$breaks <- scales::extended_breaks(24)
pac


# Next 5 forecasted values
forecast(arimaModel, 5)

# plotting the graph with next
# 5 weekly forecasted values
plot(forecast(arimaModel, 10), xlab ="monthly data",
     ylab ="Total Offences",
     main ="Total Offences per month")

#Offences variation crimes in jul 2014 compared with jun2022
variation = (totalOffencesTS[length(totalOffencesTS)] - totalOffencesTS[1]) /
  totalOffencesTS[1]


