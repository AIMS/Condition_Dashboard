### Aggregate invididual indicator scores into a dataframe

library(tidyverse)
library(purrr)

##Indicator Scores

load("data/2015_baseline/CC__scores_reef_year.RData")
cc<-mods |> select(-c(Scores, REEF.d, Below)) |> unnest(Summary) |> 
  mutate(Reference=case_when(
    Metric=="pcb.rescale.dist.metric" ~ "Critical",
    Metric=="rescale.dist.metric" ~ "Baseline",
    .default=NA
  ),
  Year=as.numeric(as.character(fYEAR)),
  Indicator="Coral.cover")

load("data/2015_baseline/MA__scores_reef_year.RData")
ma<-mods |> select(-c(Scores, REEF.d, Below)) |> unnest(Summary) |> 
  mutate(Reference=case_when(
    Metric=="consequence.metric" ~ "Critical",
    Metric=="distance.metric" ~ "Baseline",
    .default=NA
  ),
  Year=as.numeric(as.character(fYEAR)),
  Indicator="Macroalgae")

load("data/2015_baseline/CO__scores_reef_year.RData")
co<-mods |> select(-c(Scores, REEF.d, Below)) |> unnest(Summary) |> 
  mutate(Reference=case_when(
    Metric=="Critical" ~ "Critical",
    Metric=="Reference" ~ "Baseline",
    .default=NA
  ),
  Year=as.numeric(as.character(fYEAR)),
  Indicator="Community.composition")

load("data/2015_baseline/JU__scores_reef_year.RData")
ju<-mods |> select(-c(Scores, REEF.d, Below)) |> unnest(Summary) |> 
  mutate(Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    Metric=="Total" ~ "Baseline",
    .default=NA
  ),
  Year=as.numeric(as.character(fYEAR)),
  Indicator="Juvenile.density" )


load("data/2015_baseline/RPI__scores_reef_year.RData")
rp<-mods |> select(-c(Scores, REEF.d, Below)) |> unnest(Summary) |> 
  mutate(Reference=case_when(
    Metric=="critical" ~ "Critical",
    Metric=="reference" ~ "Baseline",
    .default=NA
  ),
  Year=as.numeric(as.character(fYEAR)),
  Indicator="Recovery.performance" )
rm(mods)
gc()

# rp<-read.csv("Indices.csv") |> filter(Level=="reef", Indicator=="Recovery.performance") |> as_tibble() |> 
#   select(-c(Level, Depth,Shelf,tn.reefs, n.below, Latitude, Longitude)) |> mutate(fYEAR=as.factor(Year)) |> 
#   group_by(Name, Indicator, Metric, Reference) |> 
#   tidyr::fill(Median, Upper, Lower) |> 
#   mutate(Name = str_remove_all(Name, "\\(|\\)"))
# 

scores<-cc |> 
  bind_rows(ma, ju, co, rp) |> 
  select(-c(n, BIOREGION.agg, `p<0.5`, DEPTH.f, data, REEF, variable, sd, mean)) |>
  rename(Name=REEF.d, Median=median, Upper=upper, Lower=lower) |>
  mutate(across(c(Median, Upper, Lower), ~as.numeric(.x)))

scores
write.csv(scores, "indicator_scores_revised_28092025.csv")
