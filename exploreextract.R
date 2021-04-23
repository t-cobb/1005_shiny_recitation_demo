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

# isolate the cost tibble and rename the variables

costs <- costaccess_data %>%
 select(YEAR, 
        EXPTOT, 
        EXPSELFPAY, 
        CHGTOT) %>%
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
         direct_pay = `median(direct_pay)`) 

# plot the medians. We need to stack a few to get them to appear on one plot

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
  labs(title = "Annual Cost of Medical Care in the US (median)",
       subtitle = "Total cost, direct payments, and out-of-pocket all on the rise ",
       x = "",
       y = "Amount Paid in USD",
       caption = "Source: IPUMS") +
  theme_classic()
  
  
  # geom_label(label = "Total Payment", color = "black", size = 2) 
  
  
# This treatment yields the following error 
  #Error: geom_text requires the following missing aesthetics: label
  # geom_text() +
  # annotate("text", 
  #          label = "Total Payment", 
  #          x = 2012, 
  #          y = 7500, 
  #          size = 8, 
  #          colour = "black")
  # 
# this treatment just stacks them one on another, all on the top line 
  # geom_label(label = "Total Payment", color = "black", size = 2) 
  # geom_label(label = "Direct Payment", color = "blue", size = 2) +
  # geom_label(label = "Self Payment", color = "red", size = 2) 

  
# Another example 
  # geom_text() +
  #   annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")

# geom_label(label = "Direct Payment", color = "blue", size = 2) 
# geom_label(label = "Self Payment", color = "red", size = 2)

# this is the reference from exam 2. as.date appears to specify the location. 
# How do I do this if I don't have date on the x? 
  
# geom_label(x=as.Date("2020-05-18"), y = 0.05, 
#            label = "Postcards Sent", color = "black", size = 2)

# also can add colors, labs, and style this plot 
  
# find definitions about each so I know with certainty what's in each bucket 

# question: 
# do people who cause more medical expenditures pay more or less out of pocket year over year?

# if I log my full data I'll need to remove the zeros first, like we did here: 

costsmall <- costs %>% 
  filter(total > 0) %>%
  slice_sample(n = 10000)

costmodel <- stan_glm(costsmall,
         formula = log(total) ~ self_pay + YEAR + self_pay*YEAR,
         family = gaussian,
         refresh = 0,
         seed = 288)


# combining self_pay and access, what variables would we expect to have an interesting relationship.

# ~~~~~~~~~~~~~~~

# isolate the access tibble, and rename the variables
# all of these are reasons for "no usual source of care"
# codes: 0 = n/a, 1 = no, 2 = yes
# we need to re-code to standard binary so our models are more interpretable 

access <- costaccess_data %>%
  select(YEAR, 
         NOUSLYDKWHER, 
         NOUSLYDRMOV, 
         NOUSLYFAR, 
         NOUSLYLANG, 
         NOUSLYNOLIKE,
         NOUSLYNONEED, 
         NOUSLYOTH,
         NOUSLYJOB, 
         NOUSLYNOINS) %>% 
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

access_binary <- access %>%
  mutate(where = case_when(where == 0 ~ NA_character_,
                           where == 1 ~ "No",
                           where == 2 ~ "Yes"),
         doc_moved = case_when(doc_moved == 0 ~ NA_character_,
                               doc_moved == 1 ~ "No",
                               doc_moved == 2 ~ "Yes"),
         far = case_when(far == 0 ~ NA_character_,
                          far == 1 ~ "No",
                          far == 2 ~ "Yes"),
         language = case_when(language == 0 ~ NA_character_,
                              language == 1 ~ "No",
                              language == 2 ~ "Yes"),
         dislike_doc = case_when(dislike_doc == 0 ~ NA_character_,
                                 dislike_doc == 1 ~ "No",
                                 dislike_doc == 2 ~ "Yes"),
         doc_moved = case_when(doc_moved == 0 ~ NA_character_,
                               doc_moved == 1 ~ "No",
                               doc_moved == 2 ~ "Yes"),
         doc_moved = case_when(doc_moved == 0 ~ NA_character_,
                               where == 1 ~ "No",
                               where == 2 ~ "Yes"),
         noneed_doc = case_when(noneed_doc == 0 ~ NA_character_,
                                noneed_doc == 1 ~ "No",
                                noneed_doc == 2 ~ "Yes"),
         other = case_when(other == 0 ~ NA_character_,
                           other == 1 ~ "No",
                           other == 2 ~ "Yes"),
         jobrelated = case_when(jobrelated == 0 ~ NA_character_,
                                jobrelated == 1 ~ "No",
                                jobrelated == 2 ~ "Yes"),
         noinsurance = case_when(noinsurance == 0 ~ NA_character_,
                                 noinsurance == 1 ~ "No",
                                 noinsurance == 2 ~ "Yes"))
