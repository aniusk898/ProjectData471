#####################
# Child Poverty Dataset: 2019-2021
# Data/Details from: 
# https://www.stats.govt.nz/information-releases/household-income-and-housing-cost-statistics-year-ended-june-2021/
# https://www.stats.govt.nz/methods/child-poverty-statistics-year-ended-june-2020-technical-appendix
#
#
#MEASA	Low income: less than 50% of median equivalised disposable household income before deducting housing costs (BHC) for the financial year
#MEASB	Low income: less than 50% of median equivalised disposable household income after deducting housing costs (AHC) for the base financial year
#MEASC	Material hardship
#MEASE	Low income: less than 60% of median equivalised disposable household income before deducting housing costs (BHC) for the financial year
#MEASF	Low income: less than 60% of median equivalised disposable household income after deducting housing costs (AHC) for the financial year
#MEASG	Low income: less than 50% of median equivalised disposable household income after deducting housing costs (AHC) for the financial year
#MEASH	Low income: less than 40% of median equivalised disposable household income after deducting housing costs (AHC) for the financial year
#MEASI	Severe material hardship
#MEASJ	Low income and hardship: less than 60% of median equivalised disposable household income after deducting housing costs (AHC) for the financial year and material hardship
####################


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


# Load child poverty stats
child_pov_full <- read.csv(
  file="child_poverty.csv",
  head = T,
  fileEncoding = "us-ascii", #"UTF-8", #or "utf-16le",
  sep = ",")

head(child_pov_full)


# Filter Regional Data, and proportions and numbers(in 000s) per region only
child_pov <- child_pov_full %>% filter(grepl("REGC", DemCode)) %>% 
  filter(grepl(c("B_Number|C_Population"), EstCode))%>% 
  dplyr::select(MsCode, DemCode, EstCode, Year, Estimate)


head(child_pov )


# Add new Region column
child_pov$Region2 <- NA
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC01",
                                  "Northland", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC02",
                                  "Auckland", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC03",
                                  "Waikato", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC05",
                                  "Gisborne/Hawkes Bay", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC04",
                                  "Bay of Plenty", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC10",
                                  "Canterbury", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC09",
                                  "Tasman/Nelson/Marlborough/West Coast", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC08",
                                  "Wellington", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC07",
                                  "Taranaki/Manawatu/Wanganui", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC06",
                                  "Taranaki/Manawatu/Wanganui", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC11",
                                  "Southland/Otago", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC12",
                                  "Southland/Otago", child_pov$Region2)
child_pov$Region2 <- ifelse(child_pov$DemCode == "REGC99",
                            "All Regions", child_pov$Region2)

distinct(child_pov, child_pov$Region2)



# Regroup and sum values for the same region value
child_pov <- child_pov %>% dplyr::select(MsCode, Region2, EstCode, Year, Estimate) %>%
  group_by(Region2, MsCode, EstCode, Year) %>%
  summarise(Estimate = sum(Estimate)) 


# Calculate new percentage per region
child_pov <- child_pov %>% pivot_wider(names_from=EstCode, values_from=Estimate) %>%
  mutate(Percentage = B_Number / C_Population * 100)
  
head(child_pov)


# Visualise
child_pov$Year = as.factor(child_pov$Year)

# MEASC = Material Hardship
#Percentage of children facing Material Hardship between 2019-2021, per Region
child_pov %>% filter(MsCode=="MEASC") %>%
  ggplot(aes(x=Region2, y=Percentage, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge() )+ 
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+
  labs(title="Percentage of Children per Region facing Material Hardship*",
       subtitle="*Deprivation Index Dep-17 score of 6+")+
  scale_fill_brewer(palette = "Paired")



# MEASB = Low income: less than 50% of median equivalised disposable household 
# income after deducting housing costs (AHC) for the base financial year (2017/2018)
child_pov %>% filter(MsCode=="MEASB") %>%
  ggplot(aes(x=Region2, y=Percentage, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge() )+ 
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+ 
  labs(title="Percentage of Children per Region in Low Income Households*", 
       subtitle="*Less than 50% of median equivalised disposable household income \nafter deducting housing costs")+
  scale_fill_brewer(palette = "Paired")







