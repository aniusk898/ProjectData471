########################

# Note: Ensure Police Data, HouseholdIncome, Population Estimates and Child Poverty datasets are loaded first

########################

##### 2019 - 2021 #####

# Average Household income per region from 2019-2021
aveHouseholdIncome19_21 = aveHouseholdIncome %>% filter(Year %in% c(2019, 2020,2021))

aveHouseholdIncome19_21 %>% 
  ggplot(aes(x=reorder(Region2, desc(Region2)), y=AveWeeklyIncome))+
  geom_bar(stat="identity") + 
  xlab("Region") + ylab("Income ($)") +
  coord_flip()+
  theme_bw() +
  labs(title = "Average Weekly Household per Region") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) + facet_wrap(~Year)

# Child Poverty Proportion per Region 2019 - 2021
# MEASC = Material Hardship
#Percentage of children facing Material Hardship between 2019-2021, per Region
child_pov %>% filter(MsCode=="MEASC" & !Region2=="All Regions") %>%
  ggplot(aes(x=reorder(Region2, desc(Region2)), y=Percentage)) +
  geom_bar(stat="identity")+ 
  coord_flip()+
  xlab("Region")+
  ylab("Percentage")+
 theme_bw() +
  labs(title="Percentage of Children facing Material Hardship* per Region",
       subtitle="*Deprivation Index Dep-17 score of 6+")+
   facet_wrap(~Year)

# MEASB = Low income: less than 50% of median equivalised disposable household 
# income after deducting housing costs (AHC) for the base financial year (2017/2018)
child_pov %>% filter(MsCode=="MEASB" & !Region2=="All Regions") %>%
  ggplot(aes(x=reorder(Region2, desc(Region2)), y=Percentage)) +
  geom_bar(stat="identity" )+ 
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+ 
  theme_bw() +
  labs(title="Percentage of Children per Region in Low Income Households*", 
       subtitle="*Less than 50% of median equivalised disposable household income \nafter deducting housing costs")+
  facet_wrap(~Year)


## Offences 2021 ###
# Calculate offences per 100,000 inhabitants
pop_est2021 = pop_est %>% filter(year==2021) %>% dplyr::select(Region2, population)
dataARG_2021 = dataARG %>% filter(Year.Month>"2021-01-01" & Year.Month<"2022-01-01") 
offensesByRegionAndType = as.data.frame(table(dataARG_2021$Region2, dataARG_2021$ANZSOC.Division))
head(offensesByRegionAndType)

offencesPer100k_2021 = merge(pop_est2021,offensesByRegionAndType,by.x="Region2", by.y="Var1")
offencesPer100k_2021  = offencesPer100k_2021  %>% mutate(per100k = Freq/population*100000)
head(offencesPer100k_2021 )


# Bar chart of total offences by 100,000 inhabitants per region for 2021
offencesPer100k_2021 %>% 
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("Region") + ylab("Number of offences per 100,000") + 
  labs(title = "2021: Total Offences per Region") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) 


# Theft Only per Region, for the year 2021
offencesPer100k_2021 %>% filter(Var2=="Theft and Related Offences") %>%
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k)) +
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Region") + ylab("Number of offences per 100,000") + theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) 

# Unlawful Entry per Region, for the year 2021
offencesPer100k_2021 %>% filter(Var2=="Unlawful Entry With Intent/Burglary, Break and Enter") %>%
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k)) +
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Region") + ylab("Number of offences per 100,000") + theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) 

# Top Offences Nationwide
topOffences = dataARG %>% filter(Year.Month>"2021-01-01" & Year.Month<"2022-01-01") %>%
  dplyr::select(ANZSOC.Division)
topOffences = as.data.frame(table(topOffences))
arrange(topOffences,Freq)
# Theft and Related Offences, Acts Intended to Cause Injury, Dangerous or Negligent Acts Endangering Persons
# Traffic and Vehicle Regulatory Offences, Unlawful Entry With Intent/Burglary, Break and Enter, Property Damage and Environmental Pollution

# Top 8 Offences Nationwide, split per Region, for the year 2021

new_labels <- c("Theft and Related Offences"="Theft/Related Offences",
                "Acts Intended to Cause Injury" = "Acts to Cause Injury",
                "Dangerous or Negligent Acts Endangering Persons"="Dangerous/Negligent Acts",
                "Traffic and Vehicle Regulatory Offences"="Traffic/Vehicle Offences.",
                "Unlawful Entry With Intent/Burglary, Break and Enter"="Unlawful Entry/Burglary",
                "Property Damage and Environmental Pollution"="Prop. Damage/Pollution",
                "Illicit Drug Offences"="Illicit Drug Offences", 
                "Public Order Offences"="Public Order Offences")


offencesPer100k_2021 %>% 
  filter(Var2 %in% c("Theft and Related Offences", "Acts Intended to Cause Injury", 
  "Dangerous or Negligent Acts Endangering Persons", "Traffic and Vehicle Regulatory Offences",
  "Unlawful Entry With Intent/Burglary, Break and Enter", "Property Damage and Environmental Pollution",
  "Illicit Drug Offences", "Public Order Offences")) %>%
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k, fill=Var2)) +
  geom_bar(stat="identity",show.legend = FALSE) + 
  coord_flip()+
  scale_fill_grey(start = 0, end = 0.9)+
  xlab("Region") + ylab("Number of offences per 100,000") + 
  theme_bw() +
  labs(title = "Top 8 Nationwide Offences per Region in 2021") +
  facet_wrap(~Var2, ncol=4, labeller = labeller(Var2 = new_labels))



## Offences 2019 ###
# Calculate offences per 100,000 inhabitants
pop_est2019 = pop_est %>% filter(year==2019) %>% dplyr::select(Region2, population)
dataARG_2019 = dataARG %>% filter(Year.Month>"2019-01-01" & Year.Month<"2020-01-01") 
offensesByRegionAndType = as.data.frame(table(dataARG_2019$Region2, dataARG_2019$ANZSOC.Division))
head(offensesByRegionAndType)

offencesPer100k_2019 = merge(pop_est2019,offensesByRegionAndType,by.x="Region2", by.y="Var1")
offencesPer100k_2019  = offencesPer100k_2019  %>% mutate(per100k = Freq/population*100000)
head(offencesPer100k_2019 )


# Bar chart of total offences by 100,000 inhabitants per region for 2021
offencesPer100k_2019 %>% 
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("Region") + ylab("Number of offences per 100,000") + 
  labs(title = "2019: Total Offences per Region") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, 'cm')) 


# Top Offences Nationwide
topOffences19 = dataARG %>% filter(Year.Month>"2019-01-01" & Year.Month<"2020-01-01") %>%
  dplyr::select(ANZSOC.Division)
topOffences19 = as.data.frame(table(topOffences19))
arrange(topOffences19,Freq)
# Theft and Related Offences, Acts Intended to Cause Injury, Property Damage and Environmental Pollution 
# Public Order Offences, Traffic and Vehicle Regulatory Offences, Unlawful Entry With Intent/Burglary, Break and Enter, Property Damage and Environmental Pollution

# Top 8 Offences Nationwide, split per Region, for the year 2021
offencesPer100k_2019 %>% 
  filter(Var2 %in% c("Theft and Related Offences", "Acts Intended to Cause Injury", 
                     "Dangerous or Negligent Acts Endangering Persons", "Traffic and Vehicle Regulatory Offences",
                     "Unlawful Entry With Intent/Burglary, Break and Enter", "Property Damage and Environmental Pollution",
                     "Illicit Drug Offences", "Public Order Offences"))  %>%
  ggplot(aes(x = reorder(Region2, desc(Region2)), y = per100k, fill=Var2)) +
  geom_bar(stat="identity",show.legend = FALSE) + 
  coord_flip()+
  scale_fill_grey(start = 0, end = 0.9)+
  xlab("Region") + ylab("Number of offences per 100,000") + 
  theme_bw() +
  labs(title = "Top 8 Nationwide Offences per Region in 2019") +
  facet_wrap(~Var2, ncol=4, labeller = labeller(Var2 = new_labels))


