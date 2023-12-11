# IRT Analysis


### irt model
library(rio)
library(tidyverse)
library(mirt)
library(psych)

df <- import("policy_data_wide.csv")

# First need to isolate out only values we will include in the IRT model
df1 <- df |>
  select(-state,-year)



## Sample code
## https://philippmasur.de/2022/05/13/how-to-run-irt-analyses-in-r/

fit3PL <- mirt(data = df1, 
               model = 1,  # alternatively, we could also just specify model = 1 in this case
               itemtype = "3PL", 
               verbose = FALSE, se=T, draws = 5000000, TOL = .01, optimizer = 'BFGS', se=T)


summary(fit3PL)


est.theta <- as.data.frame(fscores(fit3PL))


merged <- cbind(est.theta, df)

merged$pm_index <- merged$F1

library(usmap)


 summary_score <- merged |>
     group_by(state) |>
    summarise(mean_score=mean(F1))
 
 plot_usmap(data = summary_score, values = "mean_score") + 
    scale_fill_continuous(name = "Average Score", label = scales::comma) + 
     theme(legend.position = "right")
 