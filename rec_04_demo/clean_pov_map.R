

library(tidycensus)
library(gt)
library(janitor)
library(tidyverse)
library(ggthemes)

# from load_variables() call and lots of scrolling - use same specs
# as you do in get_*()

# B17010B_002 == Estimate!!Total!!Income in the past 12 months below poverty level
# 
# B01001_001 == Estimate!!Total

# x <- get_acs(geography = "state",
#              survey = "acs5",
#              year = 2015,
#              variables = c("B17010B_002", "B01001_001"),
# 
#              # pivot wider hack
# 
#              output = "wide",
#              geometry = TRUE,
# 
#              # move AK and HI down (scaled)
# 
#              shift_geo = TRUE)

# x1 <- x %>%
# 
#   # renaming of vars was misbehaving - forced tibble format seemed to work
#   # but the map failed to compile - upon research, forcing as_tibble() somehow
#   # corrupts the listed geometry object. see link for details from github:
#   # https://github.com/tidyverse/ggplot2/issues/3391
# 
#   # as_tibble() %>%
#   select(-ends_with("M")) %>%
# 
#   # don't care about MOE columns at the moment
# 
#   rename(poverty_pop = B17010B_002E,
#          total_pop = B01001_001E) %>%
#   clean_names() %>%
#   mutate(pov_ratio = (poverty_pop/total_pop)*100)



# saveRDS(x1, file = 'poverty_map.Rds')


# pov_plot <-  x1 %>%
#   ggplot(aes(fill = pov_ratio)) +
#       geom_sf() +
#       scale_fill_viridis_c(option = "viridis") +
#       labs(title = "Impoverished Households - 2015",
#            subtitle = "Poverty Line in 2015 was $24,250 for a family of 4",
#            caption = "Sources: ACS 2015, ASPE",
#            fill = "% HH") +
#       theme_few()

library(primer.data)

# primer.data::qscores

table <- nhanes %>% 
  select(gender, education, bmi) %>% 
  drop_na(education) %>% 
  group_by(education) %>% 
  summarise(avg_bmi = mean(bmi, na.rm = T),
            .groups = "drop") %>% 
  gt() %>%  
  cols_label(education = "Education",
             avg_bmi = "Avg. BMI") %>% 
  cols_align(align = "left")

# qscores %>% 
#   ggplot(aes(x = hours,
#              y = enrollment,
#              color = term)) +
#   geom_point() +
#   labs(
#     title = "Workload at Harvard",
#     y = "# Enrolled",
#     x = "Expected Workload / Week",
#     color = "Term")
  

plot_enroll <- qscores %>% 
  ggplot(aes(x = hours,
             y = enrollment,
             color = term)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Workload at Harvard",
    y = "# Enrolled",
    x = "Expected Workload / Week",
    color = "Term")

# If input$plot_type is "b"

plot_rating <- qscores %>% 
  ggplot(aes(x = hours,
             y = rating,
             color = term)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Teaching Staff at Harvard",
    y = "Instructor Rating",
    x = "Expected Workload / Week",
    color = "Term")


# ____________

data1 <- read_rds("poverty_map.Rds")

map2 <- data1 %>%
  ggplot(aes(fill = pov_ratio)) +
  geom_sf() +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "Impoverished Households - 2015",
       subtitle = "Poverty Line in 2015 was $24,250 for a family of 4",
       caption = "Sources: ACS 2015, ASPE",
       fill = "% HH") +
  theme_few()





