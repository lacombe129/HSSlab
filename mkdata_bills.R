### Reading google sheet into R

library(rio)
library(tidyverse)
library(googlesheets4)



sheet_url <- "https://docs.google.com/spreadsheets/d/1sKr_IVYbZ_Rvjwb-J1ioXKiYw3lKBfTP6qJ-7Z9GErM/edit#gid=608748933"




#extracting sheets
scott <- read_sheet(sheet_url, sheet="Scott LaCombe")
## need to tell R to use my credentials to pull from the sheets
2
margot <- read_sheet(sheet_url, sheet="Margot Audero")
angelica <- read_sheet(sheet_url, sheet="Angelica Brito")
perla <- read_sheet(sheet_url, sheet="Perla Ingabire")
olivia <- read_sheet(sheet_url, sheet="Olivia Smith")
bridget <- read_sheet(sheet_url, sheet="Bridget Provost")
avery <- read_sheet(sheet_url, sheet="Avery Spicka")
molly <- read_sheet(sheet_url, sheet="Molly Zelloe")
yujie <- read_sheet(sheet_url, sheet="Yujie Gong")
jaden <- read_sheet(sheet_url, sheet="Jaden Gerard")
josey <- read_sheet(sheet_url, sheet="Josey Gerrard")






scott$coder <- "Scott"
margot$coder <- "Margot"
margot$...16 <- NULL
margot$...17 <- NULL
margot$...18 <- NULL
margot$...19 <- NULL
margot$...20 <- NULL
margot$...21 <- NULL
margot$...22 <- NULL


angelica$coder <- "Angelica"
angelica$notes <- ""
perla$coder <- "Perla"
perla$...16 <- NULL
perla$...17 <- NULL
perla$notes <- perla$...15
perla$...15 <- NULL

olivia$coder <- "Olivia"
olivia$...16 <- NULL
olivia$...17 <- NULL

bridget$coder <- "Bridget"
avery$coder <- "Avery"
avery$...16 <- NULL

molly$coder <- "Molly"


yujie$coder <- "Yujie"
jaden$coder <- "Jaden"
josey$coder <- "Josey"
josey$...16 <- NULL




policies <- rbind(scott, margot, molly, bridget, olivia, avery, perla, angelica, jaden, josey, yujie)




table(policies$adopted)

policies <- policies |>
  mutate(adoption_bin=if_else(status==3, 1, 0)) |>
  drop_na(state, bill_no, direction, description, status) |>
  filter(policy!="NANA") |>
  filter(branch!="NANA") |>
  filter(author!="NULL") |>
  filter(party!="NULL")


policies[is.null(policies)] <- NA
policies$year <- as.numeric(policies$year)
policies$policy <- as.character(policies$policy)
policies$date_passed <- as.character(policies$date_passed)
policies$branch <- as.character(policies$branch)
policies$author <- as.character(policies$author)
policies$party <- as.character(policies$party)
policies$direction <- as.factor(as.character(policies$direction))
policies$topic_area <- as.factor(policies$topic_area)

## checking bills
### helpful for cleaning data- seems to override tidyverse though
library(forcats)


policies$direction1 <- fct_collapse(policies$direction, expansive = c("expanding", "Expanding", "expansive", "expansve"),
                                    restrictive = c("restricting", "Restricting"), neutral=c("Neutral", "Neutral/restricting", "neutral/restrictive", "neutral/restricting", "-99"))

table(policies$direction1)

policies$topic1 <- fct_collapse(policies$topic_area, Civil_Rights=c("Cilvil Rights", "Other, civil_rights", "civil_rights", "Civil_rights", "civil_rights, Health", "Civil_rights/....."),
                                Discrimination=c("discrimination", "Discrimination", "descrimination", "discimination", "Discrinimation", "Accomodations"),
                                Economics=c("Economic", "Economics", "economics"),
                                Education=c("education", "Education, Health", "Education,accommodations", "Education/Sports"),
                                Family=c("families", "Gay Families"),
                                Health=c("Health", "health care", "health", "Health, civil_rights", "Health, Other", "Health; Education",
                                         "healthcare", "Healthcare", "Healthcare/Medical", "Medial", "medial", "medical", "Medical", "medical/economical"),
                                Legal=c("legal", "legal recognition", "legal_recognition", "legal recognition", "Legal Recognition", "Legal_recognition"),
                                Other=c("other", "other"),
                                Public_Precence=c("protection", "public presence", "Public Presence", "Public Presense", "public_presence", "public_presense"),
                                Safety=c("safety", "Safety"),
                                Sports=c("Sports", "sports ban", "sports"))
                                
policies$topic1[is.na(policies$topic1)] <- "Other"


table(policies$topic1)

export(policies, "cleaned_policies.csv")

library(tidyverse)


library(usmap)
library(ggplot2)
## filter out policies we aren't sure about yet
## for now, only keeping 2023
policies <- policies |>
  filter(year==2023)

adoptions <- policies |>
  filter(adoption_bin==1)

summary_direction <- policies |>
  group_by(state, direction1) |>
summarise(count_direction=n())

expansive <- summary_direction |>
  filter(direction1=="expansive")


restrictive <- summary_direction |>
  filter(direction1=="restrictive")

### need to replace missing with 0 for states with 0 expansive
## VT and NJ still have missing data

states <- policies |>
  group_by(state) |>
  summarise(count=n())

restrictive <- merge(states, restrictive, by="state", all=T)

restrictive$count_direction[is.na(restrictive$count_direction)] <- 0

expansive <- merge(states, expansive, by="state", all=T)

expansive$count_direction[is.na(expansive$count_direction)] <- 0

plot_usmap(data = expansive, values = "count_direction") + 
  scale_fill_continuous(name = "Count of Expansive Policies", label = scales::comma,
                        low="lightblue", high="blue") + 
  theme(legend.position = "right")

plot_usmap(data = restrictive, values = "count_direction") + 
  scale_fill_continuous(name = "Count of Restrictive Policies", label = scales::comma,
                        low="lightblue", high="blue") 
  theme(legend.position = "right")

### count of adopted policies

summary_adopt <- policies |>
  filter(adoption_bin==1) |>
  group_by(state) |>
  summarise(count_adopt=n())

summary_adopt <- merge(states, summary_adopt, by="state", all=T)

summary_adopt$count_adopt[is.na(summary_adopt$count_adopt)] <- 0


plot_usmap(data = summary_adopt, values = "count_adopt") + 
  scale_fill_continuous(name = "Count of Policy Adoptions", label = scales::comma,
                        low="lightblue", high="blue")
  theme(legend.position = "right")

  
  
  summary_adopt <- policies |>
    filter(adoption_bin==1)
  
  
  
  ggplot(policies, aes(x=topic1))+ geom_bar() + labs(x="Topic Area", y="Count") +
    theme(axis.text.x = element_text(angle = 90, hjust=1, size=12))
  