library(ipumsr)
library(tidyverse)
library(ggthemes)
library(gt)
library(janitor)
library(rstanarm)

# read in the data 

ddi <- read_ipums_ddi("raw_data/nhis_00001.xml")
sleep_data <- read_ipums_micro(ddi)

ddi <- read_ipums_ddi("raw_data/meps_00002.xml")
cost_data <- read_ipums_micro(ddi)

# rename the columns 

costs <- cost_data %>%
 select(YEAR, EXPTOT, EXPSELFPAY, CHGTOT) %>%
  rename(direct_pay = EXPTOT,
         self_pay = EXPSELFPAY,
         total = CHGTOT) 

# find the median for each year 

median_costs <- costs %>%
 group_by(YEAR) %>%
  summarize(median(total),
            median(direct_pay),
            median(self_pay)) %>%
  rename(cost = `median(total)`,
         self = `median(self_pay)`,
         direct = `median(direct_pay)`)
  
# plot median costs over time 

median_costs %>%
  ggplot(aes(x = YEAR,
             y = direct)) +
  geom_line()
  

  
  
  
  ggplot(aes(x = YEAR, ))







# to start, let's explore sleep and exercise. 
# Does sleep improve with more exercise? 

# data %>%
#   select(YEAR, MOD10FWK, VIG10FWK, STRONGFWK, HRSLEEP, SLEEPFALL, SLEEPSTAY) %>%
#   slice_sample(n = 10)
