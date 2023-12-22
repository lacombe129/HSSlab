library(rio)
library(tidyverse)
library(janitor)
library(snakecase)

policy_data <- import("data/cleaned_policies.csv") ## import the data
policy_data <- policy_data %>% 
  mutate(row = row_number())

## i have discovered that the data are not cleaned that well for observations within variables

## make the topic area snake case
policy_data$topic_area <- to_snake_case(policy_data$topic_area, sep_out = "_")
policy_data$topic_area[policy_data$topic_area == "" | policy_data$topic_area == " "] <- NA

## fix some state names
policy_data %>% 
  distinct(state) ## get a list of all the states

policy_data$state[policy_data$state == "Illinois"] <- "IL" ## fix illinois
policy_data$state[policy_data$row == 702] <- "NY" ## fix the one 
policy_data$state[policy_data$state == "" | policy_data$state == " "] <- NA

## now the policy names 
policy_data %>% 
  distinct(policy)

## replace all the non-policy-tag observations w/ NA
policy_data$policy[policy_data$policy == "make sure to brainstorm a replacement search for parental rights"] <- NA
policy_data$policy[policy_data$policy == ""] <- NA
policy_data$policy[policy_data$policy == "99.0"] <- NA
policy_data$policy[policy_data$policy == "??"] <- NA
policy_data$policy[policy_data$policy == "-double check what our standard for anti-esg laws is"] <- NA
policy_data$policy[policy_data$policy == "what is the threshold for similar to and same as"] <- NA
policy_data$policy[policy_data$policy == "mention identical marker can be wrong"] <- NA
policy_data$policy[policy_data$policy == "are we looking at the whole bill or only the part that is relevant to us"] <- NA
policy_data$policy[policy_data$policy == "has to be identical in effect, mark if it is complex."] <- NA
policy_data$policy[policy_data$policy == "specifically mentions lgbt"] <- NA
policy_data$policy[policy_data$policy == "access medical records, look at cirriculum - parental rights general tag neutral"] <- NA
policy_data$policy[policy_data$policy == "dont include state money for religous schools unless lgbt mentioned"] <- NA
policy_data$policy[policy_data$policy == " what did we decide on trans inclusive abortion bans?? - HB2690"] <- NA
policy_data$policy[policy_data$policy == "double check how we tag defining gac as child abuse, i argue for normal gac ban tag"] <- NA
policy_data$policy[policy_data$policy == "Don not leave the lab meeting today without:"] <- NA
policy_data$policy[policy_data$policy == "- finding a criteria for parental rights and determining what i still have to search"] <- NA
policy_data$policy[policy_data$policy == "- figuring out generally how specific we want to be with duplicates, what to do with them"] <- NA
policy_data$policy[policy_data$policy == "- how to deal with super specific discrimination laws"] <- NA
policy_data$policy <- to_snake_case(policy_data$policy, sep_out = "_") ## make snake case
policy_data$policy[policy_data$policy == "99_0"] <- NA

## filter the empty observations
policy_data <- policy_data %>% 
  filter(!is.na(state))

## replace empty pass dates w/ -99

policy_data$date_passed[policy_data$date_passed == "" | policy_data$date_passed == " "] <- "-99"
policy_data$date_passed[policy_data$date_passed == "99.0"] <- "-99"
policy_data$date_passed[policy_data$date_passed == "-99.0"] <- "-99"

## fix bill numbers
policy_data$bill_no[policy_data$bill_no == "" | policy_data$bill_no == " "] <- NA

## now onto the summary statistics
policy_data2 <- policy_data %>% 
  distinct(bill_no, .keep_all = T) ## get each distinct policy, no repeats

## number passed by legislature, and by legislature and state, and by legislature and topic

num_passed <- policy_data2 %>% 
  group_by(status) %>% 
  summarize(num_passed = n()) ## what does each of the status numbers mean

num_adopted <- policy_data2 %>% 
  group_by(adopted) %>% 
  summarize(num_adopted = n()) ## what does each of the status numbers mean

adopted_states <-policy_data2 %>% 
  group_by(state) %>% 
  filter(adopted == 1) %>% 
  summarize(num_adopted = n())

## make a map of 2023 adoptions
library(usmap)


plot_usmap(data = adopted_states, values = "num_adopted") + 
  scale_fill_continuous(name = "Number of Adoptions", label = scales::comma) + 
  theme(legend.position = "right") ## map of the number of adoptions per states

  