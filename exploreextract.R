library(ipumsr)
library(tidyverse)
library(ggthemes)
library(gt)
library(janitor)

ddi <- read_ipums_ddi("raw_data/nhis_00001.xml")
data <- read_ipums_micro(ddi)

