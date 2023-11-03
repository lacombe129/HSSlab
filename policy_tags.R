## unique policy tags represented in the data

library(rio)
library(tidyverse)
library(snakecase)

## import the data

policies <- import("data/cleaned_policies.csv")

policy_tags <- policies %>% 
  group_by(policy) %>% 
  summarize(appears = n()) %>% 
  filter(!policy %in% c("", "-99.0", 
                      "- figuring out generally how specific we want to be with duplicates, what to do with them", 
                      "- finding a criteria for parental rights and determining what i still have to search", 
                      "- how to deal with super specific discrimination laws", 
                      "??", 
                      "Don not leave the lab meeting today without:", 
                      "access medical records, look at cirriculum - parental rights general tag neutral", 
                      "are we looking at the whole bill or only the part that is relevant to us", 
                      "dont include state money for religous schools unless lgbt mentioned", 
                      "double check how we tag defining gac as child abuse, i argue for normal gac ban tag", 
                      "double check what our standard for anti-esg laws is", 
                      "has to be identical in effect, mark if it is complex.", 
                      "make sure to brainstorm a replacement search for parental rights", 
                      "mention identical marker can be wrong", 
                      "what did we decide on trans inclusive abortion bans?? - HB2690", 
                      "what is the threshold for similar to and same as"))

## summarizing the policy tags to see how many each of them appear in the data
## getting rid of the random message tags that people have put in

policy_tags <- policy_tags %>% 
  mutate(policy = to_snake_case(policy)) ## making all of the policy tags into snake case

## base R: gsub(" ", "_", tolower(policy)


## exporting
##export(policy_tags, "policy_tags.csv")
