### explore Recovery indicator


library(sf)

library(tidyverse)

source("scripts/Misc/HighLevel_Classification.R")
source("scripts/plotting_functions/radial.plot.summary.R")


## Load Data ####
load("indices 2.RData")
indices<-indices |> 
  mutate(fYEAR=Year, Year=as.numeric(as.character(Year)),
         Reference=case_when(
           (Reference=="Combined") & 
             (Indicator %in% c("Community.composition", "Recovery.performance")) ~ "old_combined",
           (Reference=="Baseline") &
             (Indicator %in% c("Community.composition", "Recovery.performance")) ~ "Combined",
           .default=Reference),
         Median=ifelse(is.na(Median), 0.5, Median),
         Upper=ifelse(is.na(Upper), 0.5, Upper),
         Lower=ifelse(is.na(Lower), 0.5, Lower))


reg.p<-indices |> filter(!(Level %in% c("reef", "BIOREGION.agg","TUMRA")), Indicator=="Recovery.performance", Reference=="Combined", Shelf=="All") |> 
  ggplot(aes(x=Year, y=Median))+
  geom_pointrange(aes(ymax=Upper, ymin=Lower, color=Name))+
  geom_line(aes(color=Name))+
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  facet_wrap("Level")+
  theme_classic()


indices |> filter(!(Level %in% c("reef", "BIOREGION.agg","TUMRA")), Indicator=="Recovery.performance", Reference=="Combined") |> 
  ggplot(aes(x=Year))+
  geom_bar(aes(y=n.below*100/tn.reefs, fill=Name), stat="identity", position = "dodge")+
  facet_wrap("Level")+
  theme_classic()
