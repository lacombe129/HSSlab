## FAMD analysis
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
  

#if (!require("rio"))install.packages("rio")
#if (!require("MCMCpack"))install.packages("MCMCpack")
#if (!require("FactoMineR"))install.packages("FactoMineR")
#if (!require("missMDA"))install.packages("missMDA")
#if (!require("factoextra"))install.packages("factoextra")
#if (!require("matrixStats"))install.packages("matrixStats")

library(rio)
library(matrixStats)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(tidyverse)
## Load in the data

df <- import("MPSA paper/policy_data_wide.csv")

# First need to isolate out only values we will include in the IRT model
# I combined same sex marriage into a single variable- recoded to -1, 1, 0 if ban, legal, no law
efa <- df |>
  select(-state,-year, -lgbt_discrim_expire_rescind)


efa$dont_say_gay <- as.ordered(efa$dont_say_gay)
efa$same_sex_marriage_ban <- as.ordered(efa$same_sex_marriage_ban)
efa$relig_lib_protection_discrim <- as.ordered(efa$relig_lib_protection_discrim)
efa$decrim_same_sex <- as.ordered(efa$decrim_same_sex)
efa$sports_ban <- as.ordered(efa$sports_ban)
efa$bathroom_ban <- as.ordered(efa$bathroom_ban)
efa$gac_ban <- as.ordered(efa$gac_ban)
efa$surgery_require_genderchange <- as.ordered(efa$surgery_require_genderchange)
efa$lgbt_discrim_protect <- as.ordered(efa$lgbt_discrim_protect)
efa$preemption_discrim <- as.ordered(efa$preemption_discrim)
efa$Genderchange_x <- as.ordered(efa$Genderchange_x)
efa$No_genderchange <- as.ordered(efa$No_genderchange)
efa$No_surgery_genderchange <- as.ordered(efa$No_surgery_genderchange)
efa$hate_crimes_nolgbt <- as.ordered(efa$hate_crimes_nolgbt)
efa$hate_crimes_gayonly <- as.ordered(efa$hate_crimes_gayonly)
efa$hate_crimeslgbt <- as.ordered(efa$hate_crimeslgbt)
efa$conversion_ban <- as.ordered(efa$conversion_ban)




# for when I want to label



## drop variables as needed below
efa <- efa |>
  select(-surgery_require_genderchange, -No_surgery_genderchange, -preemption_discrim, -No_genderchange,-decrim_same_sex)


names(efa)[1] <- "Dont_Say_Gay"
names(efa)[2] <- "Hate_Crimes_NoLGBT"
names(efa)[3] <- "Marriage_Ban"
names(efa)[4] <- "Religious_Liberty_Law"
names(efa)[5] <- "Sports_Ban"
names(efa)[6] <- "Bathroom_Ban"
names(efa)[7] <- "GAC_Ban"
names(efa)[8] <- "LGBT_Discrim_Protect"
names(efa)[9] <- "Conversion Therapy Ban"
names(efa)[10] <- "Hate_Crimes_GayOnly"
names(efa)[11] <- "Hate_Crimes_LGBT"
names(efa)[12] <- "Gender_Change_X"
names(efa)[13] <- "Marriage_Legal"

## Estimate factor analysis, set dimensions to 8 as a starting point

fit <- FAMD(efa, ncp=2, graph=F)


## Eigen Values (proportion of variances retained by different dimensions)
eig.val <- get_eigenvalue(fit)
head(eig.val)

## Scree Plot
fviz_screeplot(fit)

## Variable info

var <- get_famd_var(fit)
var

head(var$coord)


head(var$contrib)
## plotting contribution of each variable

## Dimension 1- Culture war, focused primarily on transgender rights
## Dimension 2- Focuses on  legal status (marriage, anti-discrimination ordinances, etc)
## Distinguishing culture war from legal battles
fviz_famd_var(fit, repel = TRUE, title="")

fviz_contrib(fit, "var", axes = 1)
fviz_contrib(fit, "var", axes = 2)



ind <- get_famd_ind(fit)
loadings <- get_famd_var(fit)


coord <- as.data.frame(ind$coord)



merged_scores <- cbind(df, coord)

merged_scores$std_1 <- scale(merged_scores$Dim.1)
merged_scores$std_2 <- scale(merged_scores$Dim.2)*-1


export(merged_scores, "MPSA paper/scale.csv")
library(usmap)


summary_score <- merged_scores |>
  group_by(state) |>
  filter(year==2023)

plot_usmap(data = summary_score, values = "std_1") + 
  scale_fill_continuous(name = "Dimension 1 Openness", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = summary_score, values = "std_2") + 
  scale_fill_continuous(name = "Average Score", label = scales::comma) + 
  theme(legend.position = "right")




ggplot(merged_scores, aes(x=year, y=std_1)) + 
  geom_smooth()+
  labs(x="Year", y="Dimension 1 Openness")

ggplot(merged_scores, aes(x=year, y=std_2)) + 
  geom_smooth()+
  labs(x="Year", y="Dimension 2 Openness")


avg <- merged_scores |>
  group_by(year) |>
  summarise(mean1=mean(std_1),
            mean2=mean(std_2))


### map map

map <- na.omit(import("MPSA paper/MAP data.csv"))
map$state <- map$State

plot_usmap(data = map, values = "MAP_score") + 
  scale_fill_continuous(name = "2023 MAP Score", label = scales::comma,
                        low="blue", high="lightblue")+
  theme(legend.position = "right")

### corelation with MAP

merged_map <- merge(merged_scores, map, by="state")

cor.test(merged_map$std_1, merged_map$MAP_score)
cor.test(merged_map$std_2, merged_map$MAP_score)






