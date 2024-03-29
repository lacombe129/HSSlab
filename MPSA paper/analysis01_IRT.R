# IRT Analysis


### irt model
library(rio)
library(tidyverse)
library(mirt)
library(psych)
## Probably going to need to install from gihub (masurp/ggmirt)
library(ggmirt)


library(rio)
library(tidyverse)
## Load in the data

df <- import("MPSA paper/policy_data_wide.csv")

# First need to isolate out only values we will include in the IRT model
efa <- df |>
  select(-state,-year, -lgbt_discrim_expire_rescind)



# for when I want to label



## drop variables as needed below
df1 <- efa |>
  select(-surgery_require_genderchange, -No_surgery_genderchange, -preemption_discrim, -No_genderchange, -decrim_same_sex,-conversion_ban)


## Sample code
## https://philippmasur.de/2022/05/13/how-to-run-irt-analyses-in-r/

fit3PL <- mirt(data = df1, 
               model = 3,  
               itemtype = "3PL", 
               verbose = FALSE, se=T, optimizer = 'BFGS',  technical = list(NCYCLES = 3000))

itemfitPlot(fit3PL)

M2(fit3PL)

summary(fit3PL)


est.theta <- as.data.frame(fscores(fit3PL))


merged <- cbind(est.theta, df)

merged$pm_index1 <- merged$F1
merged$pm_index2 <- merged$F2

merged$statenam <- merged$state
merged$state <- NULL

export(merged, "MPSA paper/scale.csv")

library(usmap)

#us map requires variable to be called state
merged$state <- merged$state

 summary_score <- merged |>
     group_by(state) |>
    filter(year==2023)
 
 plot_usmap(data = summary_score, values = "pm_index1") + 
    scale_fill_continuous(name = "Average Score", label = scales::comma) + 
     theme(legend.position = "right")
 
 plot_usmap(data = summary_score, values = "pm_index2") + 
   scale_fill_continuous(name = "Average Score", label = scales::comma) + 
   theme(legend.position = "right")
 
 
 ggplot(merged, aes(x=year, y=F1)) + 
   geom_point()+
   labs(x="Year", y="LGBTQ Openness")
 
 
 avg <- merged |>
   group_by(year) |>
   summarise(mean1=mean(F1))
 
ggplot(avg, aes(x=year, y=mean1))+ geom_point()


ggplot(avg, aes(x=year, y=mean2))+ geom_point()

 
 #if (!requireNamespace("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("paul-buerkner/brms")
 
 