#Diamond Analysis
#Ahmed AlRAshid
#29.09.2020
# A small case study for EDA and stats


# Load packages
library(tidyverse)

# Read in the data (csv format):
#newer methods from tidyr package
jems <- read_csv("data/diamonds.csv")

#super convenient way
library(rio)
#jems2 <- import("data/diamonds.csv")

# Get familiar with our data

summary(jems)
names(jems)
str(jems)
glimpse(jems)


# more detail:

attributes(jems)
typeof(jems)

#basic filtering:
#are there any diamonds withVVS2 (clarity) & good (cut)
jems %>% 
  filter(clarity == "VVS2" & cut == "Good")



#Exercise 8.3 (Counting individual groups)-
#How many diamonds with a clarity of category “IF” are present in the data-set?
fil <- jems %>% filter(clarity == "IF") %>%  count()
fil

#or

clarity <- jems %>% 
  filter(clarity == "IF") %>% 
  nrow(clarity)
#or

sum(jems$clarity == "IF")

# What fraction of the total do they represent?

fraction0fif <- fil$n/nrow(jems)
fraction0fif

#or

nrow(clarity)/nrow(jems)

#What proportion of the whole is made up of each category of clarity?

jems %>% 
  group_by(jems$clarity) %>% 
  mean()

#or

jems %>% 
  group_by(clarity) %>% 
  count() %>% 
  mutate(prop = n/nrow(jems))
# What is the cheapest diamond price overall? 


cheapest <- jems %>% 
  filter(price == min(price))
cheapest

#or
min(jems$price)

# What is the range of diamond prices?

pricerange <- jems$price %>% 
  range()
pricerange

#or 
range(jems$price)

# What is the average diamond price in each category of cut and color?

jems %>% 
  group_by(cut, color) %>% 
  summarise(avg = mean(price))
#Make a scatter plot that shows 
#the price of a diamond as described by another continous variable, like the carat.


#what group_by does
jems %>% 
  group_by(cut, color) %>% 
  group_split()


#Using the functions we discuss earlier, and in class, apply
#a log10 transformation to both the price and carat. 
#You can save these as new columns in the data set called 
#price_log10 and carat_log10.

price_log10 <- log10(jems$price)
carat_log10 <- log10(jems$carat)
 
  jems$price_log10 <- price_log10
  jems$carat_log10 <- carat_log10
  jems
  
  #transformations
  #or 
  #recall the function for applying 
  #transformations from the tidyverse
  
  jems <- jems %>% 
    mutate(carat_log10 = log10(carat),
           price_log10 = log10(price))
  


#Make a scatter plot that shows the price of a diamond
#as described by another continous variable,
#like the carat
  
ggplot(jems, aes(x = carat_log10, y = price_log10)) +
  geom_point()







# Exercise 8.6 (Basic plotting)
ggplot(jems, aes(x = carat, y = price)) +
  geom_point()


# Produce our model
jems_lm <- lm(price_log10 ~ carat_log10, data = jems)
jems_lm

# Plot :

ggplot(jems, aes(carat_log10, price_log10)) +
       geom_point() +
  geom_smooth(method = 'lm' ,
        se = FALSE ,
        colour = "red")



