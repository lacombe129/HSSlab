# Preparing IRT data

library(rio)
library(tidyverse)

library(googlesheets4)

# Authenticate with your Google account

# Replace 'your_sheet_url' with the actual URL of your Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1sKr_IVYbZ_Rvjwb-J1ioXKiYw3lKBfTP6qJ-7Z9GErM/edit?usp=sharing"

# Read the Google Sheet into an R data frame
df <- read_sheet(sheet_url, sheet="mpsa")

## you will need to specify how you want to read in the sheet data 
2
# Print the data
print(df)




df <- df |>
  mutate(adoption=(ifelse(adoption==1, 1, 0))) |>
  filter(!(policy == "dont_say_gay" & adoption_year == 2023 & state == "Florida")) |>
  filter(adoption==1) |>
  select(state, policy, adoption_year) 

df <- na.omit(df)

df$year <- df$adoption_year

policy_wide <- df |>
  group_by(state) |>
  pivot_wider(names_from = policy, values_from = adoption_year)





states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
            "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# Create a vector of years from 1973 to 2023
years <- 1962:2023

# Create an empty dataframe
state_data <- data.frame()


for (state in states) {
  for (year in years) {
    data_point <- data.frame(
      state = state,
      year = year
    )
    state_data <- rbind(state_data, data_point)
  }
}


merged_data <- merge(df, state_data, by=c("state", "year"), all=T)


policy_wide <- merged_data |>
  group_by(state, year) |>
  pivot_wider(names_from = policy, values_from = adoption_year) 

policy_wide$`NA` <- NULL

policy_wide[is.na(policy_wide)] <- 0

policy_wide$same_sex_marriage_legal <- policy_wide$same_sex_mariage_legal
##
policy_wide$same_sex_mariage_legal <- NULL

policy_wide <- policy_wide |>
  group_by(state) |>
  mutate(dont_say_gay=max(dont_say_gay)) |>
  mutate(gac_ban=max(gac_ban)) |>
  mutate(same_sex_marriage_ban=max(same_sex_marriage_ban)) |>
  mutate(relig_lib_protection_discrim=max(relig_lib_protection_discrim)) |>
  mutate(same_sex_marriage_legal=max(same_sex_marriage_legal)) |>
  mutate(gac_ban=max(gac_ban)) |>
  mutate(bathroom_ban=max(bathroom_ban)) |>
  mutate(surgery_require_genderchange=max(surgery_require_genderchange)) |>
  mutate(lgbt_discrim_protect=max(lgbt_discrim_protect)) |>
  mutate(preemption_discrim =max(preemption_discrim )) |>
  mutate(Genderchange_x=max(Genderchange_x)) |>
  mutate(No_surgery_genderchange=max(No_surgery_genderchange)) |>
  mutate(decrim_same_sex=max(decrim_same_sex)) |>
  mutate(No_genderchange=max(No_genderchange)) 
 



policy_wide <- policy_wide |>
  group_by(state) |>
  mutate(dont_say_gay=ifelse(dont_say_gay<=year & dont_say_gay!=0,1,0)) |>
  mutate(gac_ban1=ifelse(gac_ban<=year & gac_ban!=0,1,0))|>
  mutate(same_sex_marriage_ban=ifelse(same_sex_marriage_ban<=year & same_sex_marriage_ban!=0,1,0)) |>
  mutate(relig_lib_protection_discrim=ifelse(relig_lib_protection_discrim<=year & relig_lib_protection_discrim!=0,1,0)) |>
  mutate(same_sex_marriage_legal=ifelse(same_sex_marriage_legal<=year & same_sex_marriage_legal!=0,1,0)) |>
  mutate(gac_ban=ifelse(gac_ban<=year & gac_ban!=0,0,0)) |>
  mutate(bathroom_ban=ifelse(bathroom_ban<=year & bathroom_ban!=0,-1,0)) |>
  mutate(surgery_require_genderchange=ifelse(surgery_require_genderchange<=year & surgery_require_genderchange!=0,1,0)) |>
  mutate(lgbt_discrim_protect=ifelse(lgbt_discrim_protect<=year & lgbt_discrim_protect!=0,1,0)) |>
  mutate(preemption_discrim =ifelse(preemption_discrim <=year & preemption_discrim!=0,1,0)) |>
  mutate(Genderchange_x=ifelse(Genderchange_x<=year & Genderchange_x!=0,1,0)) |>
  mutate(decrim_same_sex=ifelse(decrim_same_sex<=year & decrim_same_sex!=0,1,0)) |>
  mutate(No_surgery_genderchange=ifelse(No_surgery_genderchange<=year & No_surgery_genderchange!=0,1,0)) |>
  mutate(No_genderchange=ifelse(No_genderchange<=year & No_genderchange!=0,1,0)) 



## for same sex marriage ban- needs to be removed if marriage legalized
policy_wide$same_sex_marriage_legal[policy_wide$year >= 2015] <- 1
policy_wide$same_sex_marriage_ban[policy_wide$same_sex_marriage_legal == 1] <- 0

policy_wide$gac_ban[policy_wide$gac_ban1 == 1] <- 1
policy_wide$gac_ban1 <- NULL

policy_wide <- policy_wide |>
  filter(year>=2000) 


export(policy_wide, "MPSA paper/policy_data_wide.csv")

