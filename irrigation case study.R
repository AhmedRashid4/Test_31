# Irrigation analysis
# Ahmed AlRashid
# 01.10.2020
# A small case study
library(tidyverse)
#begin with wide "messy" format:
irrigation <- read_csv("data/irrigation_wide.csv")


  
  # Examine the data
glimpse(irrigation)

#or
summary(irrigation)

# in 2007, what is the total area under irrigation only
#for the Americas

irrigation %>% 
  filter(year == 2007) %>% 
  select(`N. America`,`S. America`)
  
# to answer the following questions we should use tidy data

irrigation_t <- irrigation %>% 
  pivot_longer(-year, names_to = "region")
# What is the total area under irrigation in each year?

irrigation_t %>% 
  group_by(year) %>% 
summarise(total = sum(value))

#which 2 regions increased the most from 1980 to 2007?
irrigation_t %>% 
  group_by(region) %>% 
  summarise(diff = value[year == 2007] - value[year == 1980]) %>% 
arrange(-diff) %>% 
  slice(1:2)

#our use top_n()

irrigation_t %>% 
  group_by(region) %>% 
  summarise(diff = value[year == 2007] - value[year == 1980]) %>%
  slice_max(diff, n = 2)
# What is the rate of change in each regain?

xx <- c(1, 1.2, 1.6, 1.1)
xx
diff(xx)
# There are the absolute differences:

irrigation_t %>% 
  group_by(region) %>% 
  mutate(rate = c(0, diff(value)/value[-length(value)]))

# Where is it the lowest and highest?

irrigation_t[which.max(irrigation_t$rate),]
irrigation_t[which.min(irrigation_t$rate),]

# the tidyverse way

irrigation_t %>%
  ungroup() %>% 
  slice_max(rate, n =1)

irrigation_t %>%
  ungroup() %>% 
  slice_min(rate, n =1)
  



# Standardize against 1980 (relative change over 1980) (easier)
# Plot area over time for each region?