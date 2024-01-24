# IRT Analysis


### irt model
library(rio)
library(tidyverse)
library(mirt)
library(psych)
## Probably going to need to install from gihub (masurp/ggmirt)
library(ggmirt)

df <- import("MPSA paper/policy_data_wide.csv")

# First need to isolate out only values we will include in the IRT model
# I combined same sex marriage into a single variable- recoded to -1, 1, 0 if ban, legal, no law
df1 <- df |>
  select(-state,-year, -bathroom_ban)


## Post conference
# mess around with number of items
# mess around with method



## Sample code
## https://philippmasur.de/2022/05/13/how-to-run-irt-analyses-in-r/

fit3PL <- mirt(data = df1, 
               model = 1,  
               itemtype = "3PL", 
               verbose = FALSE, se=T, draws = 5000000, 
               TOL = .0001, optimizer = 'BFGS')

itemfitPlot(fit3PL)

M2(fit3PL)

summary(fit3PL)


est.theta <- as.data.frame(fscores(fit3PL))


merged <- cbind(est.theta, df)

merged$pm_index <- merged$F1

merged$statenam <- merged$state
merged$state <- NULL

export(merged, "MPSA paper/scale.csv")

library(usmap)

#us map requires variable to be called state
merged$state <- merged$state

 summary_score <- merged |>
     group_by(state) |>
    filter(year==2023)
 
 plot_usmap(data = summary_score, values = "pm_index") + 
    scale_fill_continuous(name = "Average Score", label = scales::comma) + 
     theme(legend.position = "right")
 
 
 ggplot(merged, aes(x=year, y=F1)) + 
   geom_smooth()+
   labs(x="Year", y="LGBTQ Openness")
 
 
 avg <- merged |>
   group_by(year) |>
   summarise(mean=mean(F1))
 