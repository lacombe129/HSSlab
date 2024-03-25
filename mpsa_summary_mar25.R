## MPSA data, summary statistics

library(rio)
library(tidyverse)
library(janitor)

## import the MPSA data (mpsa tab in Bill Collection google sheet)
mpsa_data <- import("data/mpsa_march25.csv")

## check what policy tags we have lots of data for

tags <- mpsa_data %>% 
  group_by(policy) %>% 
  summarize(adoptions = sum(adoption)) %>% 
  filter(adoptions > 0 | is.na(adoptions)) ## get rid of the extra rows for the duplicate policies

## this is how many policies we have for each tag

total_nums <- mpsa_data %>% 
  summarise(total = sum(adoption, na.rm = T)) ## 392 adoptions

## adoption years

years <- mpsa_data %>% 
  group_by(adoption_year) %>% 
  summarise(adoptions_per_year = sum(adoption)) ## how many policies were adopted per year

years2 <- mpsa_data %>% 
  group_by(policy, adoption_year) %>% 
  summarise(policy_adoptions_year = sum(adoption)) ## number of adoptions per year split by policy tag

years3 <- years2 %>% 
  group_by(policy) %>% 
  mutate(oldest_year = min(adoption_year, na.rm = T), recent_year = max(adoption_year, na.rm = T), 
         highest_num = max(policy_adoptions_year), 
         highest_num_year = ifelse(policy_adoptions_year == highest_num, adoption_year, NA)) %>% 
  distinct(policy, .keep_all = T) %>% 
  filter(policy_adoptions_year != 0) %>% 
  select(-c(policy_adoptions_year, adoption_year)) ## get the earliest, latest adoptions, most policies per year

## state

state <- mpsa_data %>% 
  group_by(state) %>% 
  summarise(total_state = sum(adoption)) ## total number of policies adopted per state

state2 <- mpsa_data %>% 
  group_by(state, adoption_year) %>% 
  summarise(total_state_year = sum(adoption)) %>% 
  filter(!is.na(adoption_year)) ## number of policies per state per year


