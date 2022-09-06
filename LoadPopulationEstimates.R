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

# Load Population Estimates
pop_est_full <- read.csv(
  file="pop_estimates.csv",
  head = T,
  fileEncoding = "UTF-8", #"UTF-8", #or "utf-16le",
  sep = ",")

head(pop_est_full)


# Filter Population Estimates by Regions2
pop_est = pop_est_full %>% filter(name %in% c("Northland region",
                                              "Auckland region",
                                              "Waikato region",
                                              "Bay of Plenty region",
                                              "Gisborne region",
                                              "Hawke's Bay region",
                                              "Taranaki region",
                                              "Manawatū-Whanganui region",
                                              "Wellington region",
                                              "Tasman region",
                                              "Nelson region",
                                              "Marlborough region",
                                              "West Coast region",
                                              "Canterbury region",
                                              "Otago region",
                                              "Southland region"))
pop_est = pop_est %>% dplyr::select(name:erp21)

# Pivot df to long format
pop_est = pop_est %>% pivot_longer(cols=c('erp18', 'erp19', 'erp20', 'erp21'), names_to='year', values_to='population')
pop_est
# Convert year labels to years
pop_est$year<- ifelse(pop_est$year== "erp18",
                      2018, pop_est$year)
pop_est$year<- ifelse(pop_est$year== "erp19",
                      2019, pop_est$year)
pop_est$year<- ifelse(pop_est$year== "erp20",
                      2020, pop_est$year)
pop_est$year<- ifelse(pop_est$year== "erp21",
                      2021, pop_est$year)

head(pop_est)


# Convert Regions to be in line with other data sets
pop_est$Region2 <- NA
pop_est$Region2 <- ifelse(pop_est$name == "Northland region",
                          "Northland", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Auckland region",
                          "Auckland", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Waikato region",
                          "Waikato", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Gisborne region",
                          "Gisborne/Hawkes Bay", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Hawke's Bay region",
                          "Gisborne/Hawkes Bay", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Bay of Plenty region",
                          "Bay of Plenty", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Wellington region",
                          "Wellington", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Taranaki region",
                          "Taranaki/Manawatu/Wanganui", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Manawatū-Whanganui region",
                          "Taranaki/Manawatu/Wanganui", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Tasman region",
                          "Tasman/Nelson/Marlborough/West Coast", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Nelson region",
                          "Tasman/Nelson/Marlborough/West Coast", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Marlborough region",
                          "Tasman/Nelson/Marlborough/West Coast", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "West Coast region",
                          "Tasman/Nelson/Marlborough/West Coast", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Canterbury region",
                          "Canterbury", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Southland region",
                          "Southland/Otago", pop_est$Region2)
pop_est$Region2 <- ifelse(pop_est$name == "Otago region",
                          "Southland/Otago", pop_est$Region2)

distinct(pop_est, pop_est$Region2)

# Regroup and sum values for the same region value
pop_est <- pop_est %>% dplyr::select(Region2, year, population) %>%
  group_by(Region2, year) %>%
  summarise(population = sum(population))

pop_est$year <- as.integer(pop_est$year)
ggplot(pop_est, aes(x=year, y=population, color = Region2)) +
  geom_line()+ 
  xlab("Year")+
  ylab("Population Estimate")+ 
  labs(color="Region")

