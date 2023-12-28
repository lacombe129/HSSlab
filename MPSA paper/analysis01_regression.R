## Analysis for regression models
library(rio)
library(tidyverse)


## Load lgbt scale- make sure state names are same for merging
df <- import("MPSA paper/scale.csv")


df$state_name <-  toupper(df$statenam)
## covariate data
complete_covars <- import("data/complete_covars.csv")

# extra variables
extra_covar <- import("MPSA paper/extra_covar.csv")

extra_covar$state_name <-  toupper(extra_covar$state_name)

## merge together
df1 <- merge(df, complete_covars, by=c("state_name", "year"))
df1 <- merge(df1, extra_covar, by=c("state_name", "year"), all.x=T)


df1 <- df1 |>
  mutate(gop_control=ifelse(party_control=="REP" | rep_unified==1, 1, 0)) |>
  mutate(dem_control=ifelse(party_control=="DEM" | dem_unified==1, 1, 0)) |>
  mutate(std_pop=scale(population_total)) |>
  mutate(std_inc=scale(incomepcap)) |>
  mutate(initiative=ifelse(init>=1, 1, 0))
  
  

## Models
## tasks
## year fixed effects?
## professionalism
## ballot initiative

lm1 <- lm(F1~pollib_median+rep_unified+dem_unified+ std_pop+evangelical_pop+std_inc+initiative+as.factor(year), data=df1)

summary(lm1)