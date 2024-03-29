## Analysis for regression models
library(rio)
library(tidyverse)
library(usmap)


## Load lgbt scale- make sure state names are same for merging
df <- import("MPSA paper/scale.csv")


df$state_name <-  toupper(df$state)
## covariate data
complete_covars <- import("data/complete_covars.csv")

# extra variables
extra_covar <- import("MPSA paper/extra_covar.csv")

### ideology also estimated for 2021-2023 using predicted results from regression
### regression generated predictions from model using interaction of year and state to generate linear predictions for each state
## basically linear interpolation
ideology <- import("MPSA paper/ideology.dta")
ideology$state_name <- toupper(ideology$state_name)


extra_covar$state_name <-  toupper(extra_covar$state_name)


## merge together
df1 <- merge(df, complete_covars, by=c("state_name", "year"))
df1 <- merge(df1, extra_covar, by=c("state_name", "year"), all.x=T)


df1 <- merge(df1, ideology, by=c("state_name", "year"), all.x=T)


df1 <- df1 |>
  mutate(gop_control=ifelse(party_control=="REP" | rep_unified==1, 1, 0)) |>
  mutate(dem_control=ifelse(party_control=="DEM" | dem_unified==1, 1, 0)) |>
  mutate(std_pop=scale(population_total)) |>
  mutate(std_inc=scale(incomepcap)) |>
  mutate(initiative=ifelse(init>=1, 1, 0))

df1$gop_control[is.na(df1$gop_control)] <- 0
df1$dem_control[is.na(df1$dem_control)] <- 0  
  

## Models- original data


lm1 <- lm(std_1~lag_social+gop_control+dem_control+ as.factor(year)+ as.factor(state_name), data=df1)


lm2 <- lm(std_1~lag_social+gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

lm3 <- lm(std_1~lag_social*gop_control+lag_social*dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)


lm4 <- lm(std_2~lag_social+gop_control+dem_control+as.factor(year)+ as.factor(state_name), data=df1)

lm5 <- lm(std_2~lag_social+gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

lm6 <- lm(std_2~lag_social*gop_control+lag_social*dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

### ommiting ideology from std2 so estimating parallel models

lm5 <- lm(std_1~gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)
lm6 <- lm(std_2~gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)


summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)


library(texreg)

screenreg(list(lm2, lm3, lm5, lm6), custom.coef.map = list("lag_social" = "Mass Social Liberalism",
                                               "gop_control" = "Unified GOP", 
                                               "lag_social:gop_control"="Liberalism*GOP",
                                               "dem_control" = "Unified Dem",
                                               "lag_social:dem_control"="Liberalism*Dem",
                                               "std_pop"="Population",
                                               "std_inc"="Income Per Cap",
                                               "evangelical_pop"="Evangelical %",
                                               "(Intercept)"="Intercept"))


htmlreg(list(lm2, lm3, lm5, lm6), custom.coef.map = list("lag_social" = "Mass Social Liberalism",
                                               "gop_control" = "Unified GOP", 
                                               "lag_social:gop_control"="Liberalism*GOP",
                                               "dem_control" = "Unified Dem",
                                               "lag_social:dem_control"="Liberalism*Dem",
                                               "std_pop"="Population",
                                               "std_inc"="Income Per Cap",
                                               "(Intercept)"="Intercept"), file="MPSA paper/opennessmodels.doc")


### models- interpolated data
## note that if you restrict analysis to just more recent years, the negative correlation wh public opinion disappears, eventually becomes positive

lm1 <- lm(std_1~est_social+gop_control+dem_control+ as.factor(year)+ as.factor(state_name), data=df1)


lm2 <- lm(std_1~est_social+gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

lm3 <- lm(std_1~est_social*gop_control+est_social*dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)


lm4 <- lm(std_2~est_social+gop_control+dem_control+as.factor(year)+ as.factor(state_name), data=df1)

lm5 <- lm(std_2~est_social+gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

lm6 <- lm(std_2~est_social*gop_control+est_social*dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)

screenreg(list(lm2, lm3, lm5, lm6), custom.coef.map = list("est_social" = "Mass Social Liberalism",
                                                           "gop_control" = "Unified GOP", 
                                                           "est_social:gop_control"="Liberalism*GOP",
                                                           "dem_control" = "Unified Dem",
                                                           "est_social:dem_control"="Liberalism*Dem",
                                                           "std_pop"="Population",
                                                           "std_inc"="Income Per Cap",
                                                           "evangelical_pop"="Evangelical %",
                                                           "(Intercept)"="Intercept"))



### what if we remove liberalism?

### time
filtered <- df1 |>
  filter(year==2015)

lm1 <- lm(std_1~gop_control+dem_control+ as.factor(year)+ as.factor(state_name), data=df1)


lm2 <- lm(std_1~gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=df1)



lm4 <- lm(std_2~gop_control+dem_control+as.factor(year)+ as.factor(state_name), data=df1)

lm5 <- lm(std_2~gop_control+dem_control+ std_pop+evangelical_pop+std_inc+as.factor(year)+ as.factor(state_name), data=filtered)



screenreg(list(lm1, lm2, lm4, lm5), custom.coef.map = list("gop_control" = "Unified GOP", 
                                                         "dem_control" = "Unified Dem",
                                                         "std_pop"="Population",
                                                         "std_inc"="Income Per Cap",
                                                         "evangelical_pop"="Evangelical %",
                                                         "(Intercept)"="Intercept"))


htmlreg(list(lm1, lm2, lm4, lm5), custom.coef.map = list("gop_control" = "Unified GOP", 
                                                           "dem_control" = "Unified Dem",
                                                           "std_pop"="Population",
                                                           "std_inc"="Income Per Cap",
                                                           "evangelical_pop"="Evangelical %",
                                                           "(Intercept)"="Intercept"), file="MPSA paper/opennessmodels_nolib.doc")


## for interpreting models

subset<- df1 |>
  filter(year==2023) |>
  select(state, std_1, std_2)
