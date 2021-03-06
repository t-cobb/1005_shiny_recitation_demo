library(ipumsr)
library(tidyverse)
library(ggthemes)
library(gt)
library(janitor)
library(rstanarm)

# read in the ipums data 

ddi <- read_ipums_ddi("raw_data/nhis_00001.xml")
sleep_data <- read_ipums_micro(ddi)

ddi <- read_ipums_ddi("raw_data/meps_00002.xml")
costaccess_data <- read_ipums_micro(ddi)

# rename the cost variables

costs <- costaccess_data %>%
  rename(direct_pay = EXPTOT,
         self_pay = EXPSELFPAY,
         total = CHGTOT) 

# find the median for each year
# Choosing median over mean because I'm wary of outliers throwing off our range

median_costs <- costs %>%
  group_by(YEAR) %>%
  summarize(median(total),
            median(direct_pay),
            median(self_pay),
            .groups = "drop") %>%
  rename(total = `median(total)`,
         self_pay = `median(self_pay)`,
         direct_pay = `median(direct_pay)`) %>%
  select(YEAR,
         direct_pay,
         self_pay,
         total) 

# Plot the medians. We need to stack a few to get them to appear on one plot.
# clarify definitions about each so I know with certainty what's in each bucket 

median_plot <- median_costs %>%
  ggplot(aes(x = YEAR,
             y = total)) +
  geom_point() +
  geom_line() +
  geom_point(aes(x = YEAR,
                 y = direct_pay)) +
  geom_line(aes(x = YEAR,
                y = direct_pay)) +
  geom_point(aes(x = YEAR,
                 y = self_pay)) +
  geom_line(aes(x = YEAR,
                y = self_pay)) + 
 annotate("text", x = 2016, y = 1200, label = "Total Cost") +
 annotate("text", x = 2014, y = 450, label = "Direct Payments") +
 annotate("text", x = 2011, y = 150, label = "Out of Pocket") +
  labs(title = "Annual Cost of Medical Care in the US (median)",
       subtitle = "2016 saw an uptick in all categories",
       x = "",
       y = "Amount Paid in USD",
       caption = "Source: IPUMS") +
  theme_classic()
  
  
# this treatment just stacks them one on another, all on the top line 
  # geom_label(label = "Total Payment", color = "black", size = 2) 
  # geom_label(label = "Direct Payment", color = "blue", size = 2) +
  # geom_label(label = "Self Payment", color = "red", size = 2) 

# question: 
# do people who cause more medical expenditures pay more or less out of pocket year over year?

# if I log my full data I'll need to remove the zeros first, like we did here: 

costsmall <- costs %>% 
  filter(total > 0) %>%
  slice_sample(n = 10000)

fit_1 <- stan_glm(costsmall,
         formula = log(total) ~ self_pay + YEAR + self_pay*YEAR,
         family = gaussian,
         refresh = 0,
         seed = 288)

# ~~~~~~~~~~~~~~~

# isolate the access tibble, and rename the variables
# all of these are reasons for "no usual source of care"
# codes: 0 = n/a, 1 = no, 2 = yes
# we need to re-code to standard binary so our models are more interpretable 

costs_clean <- costs %>%
  select(YEAR, 
         NOUSLYDKWHER, 
         NOUSLYDRMOV, 
         NOUSLYFAR, 
         NOUSLYLANG, 
         NOUSLYNOLIKE,
         NOUSLYNONEED, 
         NOUSLYOTH,
         NOUSLYJOB, 
         NOUSLYNOINS,
         total,
         self_pay,
         direct_pay) %>% 
  rename(where = NOUSLYDKWHER,
         doc_moved = NOUSLYDRMOV,
         far = NOUSLYFAR, 
         language = NOUSLYLANG, 
         dislike_doc = NOUSLYNOLIKE,
         noneed_doc = NOUSLYNONEED, 
         other = NOUSLYOTH,
         jobrelated = NOUSLYJOB, 
         noinsurance = NOUSLYNOINS)

# recode all of the binary variables to NA, 0, 1
# Because there are some coded as "unknown" I've decided to recode these as NA 

access_clean <- costs_clean %>%
    mutate(where = case_when(where == 1 ~ "No",
                           where == 2 ~ "Yes",
                           TRUE ~ NA_character_),
           doc_moved = case_when(doc_moved == 1 ~ "No",
                               doc_moved == 2 ~ "Yes",
                               TRUE ~ NA_character_),
         far = case_when(far == 1 ~ "No",
                          far == 2 ~ "Yes",
                         TRUE ~ NA_character_),
         language = case_when(language == 1 ~ "No",
                              language == 2 ~ "Yes",
                              TRUE ~ NA_character_),
         dislike_doc = case_when(dislike_doc == 1 ~ "No",
                                 dislike_doc == 2 ~ "Yes",
                                 TRUE ~ NA_character_),
         noneed_doc = case_when(noneed_doc == 1 ~ "No",
                                noneed_doc == 2 ~ "Yes",
                                TRUE ~ NA_character_),
         other = case_when(other == 1 ~ "No",
                           other == 2 ~ "Yes",
                           TRUE ~ NA_character_),
         jobrelated = case_when(jobrelated == 1 ~ "No",
                                jobrelated == 2 ~ "Yes",
                                TRUE ~ NA_character_),
         noinsurance = case_when(noinsurance == 1 ~ "No",
                                 noinsurance == 2 ~ "Yes",
                                 TRUE ~ NA_character_))

# experiment with some models to explore the interaction between access and cost

# combining self_pay and access, what variables would we expect to have an 
# interesting relationship?

# roughly if 2x the SD is = Median the model is significant

# run with larger sets of data
# run loocompare and select the models that most significantly predict total cost and self pay
# run these models on the full data 
# based on the access variables, what can we learn about potential health care costs and expenditures? 

# select 2 fits, save the output as RDS

x <- access_clean %>%
  filter(total > 0) %>%
  slice_sample(n = 50000)

fit_2 <- stan_glm(x,
                  formula = log(total) ~ language + self_pay,
                   refresh = 0,
                   family = gaussian,
                   seed = 254)

# saveRDS(fit_2, file = "fit_2.rds")
# this is the line to save the output of my fit. Then I move it to clean_data folder 
# within my app, I call readRDS("(foldername)/fit_2.RDS") assign to an object
# don't put rstanarm in the app or the app won't publish 

fit_3 <- stan_glm(x,
                 formula = log(total) ~ language + noinsurance + self_pay + self_pay*jobrelated,
                 refresh = 0,
                 family = gaussian,
                 seed = 254)

fit_4 <- stan_glm(x,
                  formula = log(total) ~ self_pay + where + doc_moved + far + direct_pay*doc_moved,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

# could be interesting result in noinsurance coefficient? 
# in comparison with fit_2: how would all of these access variables impact total costs and out of pocket costs? 

fit_5 <- stan_glm(x,
                  formula = self_pay ~ total + noinsurance + jobrelated + noneed_doc + other +
                            total*jobrelated,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

# this spit out some crazy errors and seems unreliable 

fit_6 <- stan_glm(x,
                  formula = self_pay ~ total + direct_pay + doc_moved +
                  noinsurance*jobrelated,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

fit_7 <- stan_glm(x,
                  formula = log(total) ~ self_pay + YEAR + self_pay*noinsurance,
                  family = gaussian,
                  refresh = 0,
                  seed = 288)


# Access posteriors: what is the probability of being impacted by various barriers to care?


