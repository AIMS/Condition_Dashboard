rm(list=ls())
library(tidyverse)


# Load original Indicators dataframe 
load("indicators_dataframe.Rdata")
s.df |> filter(Indicator=="Juvenile.density", Level=="reef")
scores<-s.df

# %>%
#   mutate(Name=case_when(Level=="reef" ~ paste0(Name," (",Depth,")"),
#                         .default=Name))

## LOAD NEW DATA FOR JUVENILES

juv.files=list.files("old_data_files/juveniles/", full.names = T)

# BIOREGIONS
load(juv.files[[1]])
juv.bio<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="BIOREGION.agg",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
                                     Indicator="Juvenile.density",
                                     Reference=case_when(
                                       Metric=="Acropora" ~ "Critical",
                                       .default="Baseline"
                                     )) |> 
    rename(Name=BIOREGION.agg, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))
  
# GBRMP
load(juv.files[[2]])
juv.gbrmp<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="GBRMP",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  )) |> 
  rename(Name=GBRMP, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))


# GBRMP Management Areas
load(juv.files[[3]])
juv.gbrMA<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="GBRMPA.MA",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  )) |> 
  rename(Name=GBRMPA.MA, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))

# NRM
load(juv.files[[4]])
juv.NRM<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="NRM",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  )) |> 
  rename(Name=NRM, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))


# Reef
load(juv.files[[5]])
juv.reef<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="reef",Year=as.numeric(as.character(fYEAR)),
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  ),
  Depth=case_when(str_detect(REEF.d, "deep") ~ "deep slope",
                  .default="shallow slope"),
  tn.reefs=NA, n.below=NA,
  Name=sprintf("%s (%s)",REEF, Depth))|> 
  rename( Median=median,Lower=lower, Upper=upper) |> 
  left_join(scores |> filter(Indicator=="Juvenile.density", Level=="reef") |> 
              select(Name, Shelf, Latitude, Longitude) |> unique()) |> 
  select(names(scores))



# TUMRA
load(juv.files[[6]])
juv.TUMRA<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="TUMRA",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  )) |> 
  rename(Name=TUMRA, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))

# ZONE
load(juv.files[[7]])
juv.ZONE<-bind_rows(mods$Summary) |> ungroup() |> mutate(
  Level="ZONE",Year=as.numeric(as.character(fYEAR)), Depth=NA, Latitude=NA, Longitude=NA,
  Indicator="Juvenile.density",
  Reference=case_when(
    Metric=="Acropora" ~ "Critical",
    .default="Baseline"
  )) |> 
  rename(Name=ZONE, Median=median,Lower=lower, Upper=upper) |> 
  select(names(s.df))

juv.new<-bind_rows(juv.reef,juv.bio, juv.gbrMA, juv.gbrmp, juv.NRM, juv.TUMRA, juv.ZONE)


scores<-scores |> 
  filter(Indicator != "Juvenile.density") |> 
  bind_rows(juv.new)

write_csv(scores, "Indices.csv")


