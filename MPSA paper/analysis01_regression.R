## Analysis for regression models
library(rio)
library(tidyverse)
library(usmap)


## Load lgbt scale- make sure state names are same for merging
df <- import("MPSA paper/scale.csv")


df$state_name <-  toupper(df$statenam)
## covariate data
complete_covars <- import("data/complete_covars.csv")

# extra variables
extra_covar <- import("MPSA paper/extra_covar.csv")
map <- na.omit(import("MPSA paper/MAP data.csv"))
ideology <- import("MPSA paper/ideology.dta")
ideology$state_name <- toupper(ideology$state_name)


extra_covar$state_name <-  toupper(extra_covar$state_name)
map$state_name <- toupper(map$State)
map$state <- map$State
map$year <- 2023
## Comparison to MAP

map1 <- merge(map, df, by=c("state_name", "year"))



## converting to quartiles first to make more direct comparison
map1$f1_4 <- ntile(map1$F1, 4)  
map1$MAP_4 <- ntile(map1$MAP_score, 4)  

cor(map1$F1, map1$MAP_score)



plot_usmap(data = map1, values = "f1_4") + 
  scale_fill_continuous(name = "2023 Openness Score", label = scales::comma,
                        low="lightblue", high="blue")+ 
  theme(legend.position = "right")


plot_usmap(data = map1, values = "MAP_4") + 
  scale_fill_continuous(name = "2023 MAP Score", label = scales::comma,
                        low="lightblue", high="blue")+
  theme(legend.position = "right")

## merge together
df1 <- merge(df, complete_covars, by=c("state_name", "year"))
df1 <- merge(df1, extra_covar, by=c("state_name", "year"), all.x=T)

ideology <- ideology |>
  select(year, state_name, LJKE_StatePolicyMood, masssociallib_est, masseconlib_est, lag_social)
ideology$year <- as.numeric(ideology$year)
ideology$masseconlib_est <- as.numeric(ideology$masseconlib_est)
ideology$masssociallib_est <- as.numeric(ideology$masssociallib_est)

df1 <- merge(df1, ideology, by=c("state_name", "year"), all.x=T)


df1 <- df1 |>
  mutate(gop_control=ifelse(party_control=="REP" | rep_unified==1, 1, 0)) |>
  mutate(dem_control=ifelse(party_control=="DEM" | dem_unified==1, 1, 0)) |>
  mutate(std_pop=scale(population_total)) |>
  mutate(std_inc=scale(incomepcap)) |>
  mutate(initiative=ifelse(init>=1, 1, 0))

df1$gop_control[is.na(df1$gop_control)] <- 0
df1$dem_control[is.na(df1$dem_control)] <- 0  
  

## Models


lm0 <- lm(F1~lag_social+gop_control+dem_control+ as.factor(year)+ as.factor(state_name), data=df1)


lm1 <- lm(F1~lag_social+rep_unified+dem_unified+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

lm2 <- lm(F1~lag_social*rep_unified+lag_social*dem_unified+ std_pop+evangelical_pop+std_inc+as.factor(year), data=df1)


summary(lm0)
summary(lm1)
summary(lm2)

library(texreg)

screenreg(list(lm1, lm2), custom.coef.map = list("lag_social" = "Mass Social Liberalism",
                                               "rep_unified" = "Unified GOP", 
                                               "lag_social:rep_unified"="Liberalism*GOP",
                                               "dem_unified" = "Unified Dem",
                                               "lag_social:dem_unified"="Liberalism*Dem",
                                               "std_pop"="Population",
                                               "std_inc"="Income Per Cap",
                                               "evangelical_pop"="Evangelical %",
                                               "(Intercept)"="Intercept"))


htmlreg(list(lm1, lm2), custom.coef.map = list("lag_social" = "Mass Social Liberalism",
                                               "rep_unified" = "Unified GOP", 
                                               "lag_social:rep_unified"="Liberalism*GOP",
                                               "dem_unified" = "Unified Dem",
                                               "lag_social:dem_unified"="Liberalism*Dem",
                                               "std_pop"="Population",
                                               "std_inc"="Income Per Cap",
                                               "(Intercept)"="Intercept"), file="MPSA paper/opennessmodels.doc")
