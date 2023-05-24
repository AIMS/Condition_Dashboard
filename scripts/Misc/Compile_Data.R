# Compile Data
library(tidyverse)
library(sf)
library(gisaimsr)

library(posterior)

library(tidybayes)
rm(list=ls())

setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")


##Load compiled data by Murray

scores<-read_csv("outputs/Indices.csv")%>%
  group_by(Level,Year,Name,Latitude,Longitude,Depth,Indicator,Metric,Reference)%>%
  summarise(Median=mean(Median), Lower=mean(Lower),Upper=mean(Upper),tn.reefs=mean(tn.reefs), n.below=mean(n.below))

reefs<-read_csv("outputs/Indices.csv")%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Latitude,Longitude))%>%
  unique()%>%
  group_by(Name)%>%
  summarise(Latitude=mean(Latitude), Longitude= mean(Longitude))%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)


#Load Regions


gbrmpa<-st_read("GIS/Great_Barrier_Reef_Marine_Park_Boundary.geojson",quiet = T)%>%
  mutate(Region="GBRMP", Name="GBRMP")%>%
  select(Name,Region, geometry)%>%
  st_buffer(dist = 0.1)
tumra<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Traditional_Use_of_Marine_Resources_Agreement_areas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  group_by(Name)%>%
  # summarise(geometry=st_combine(.))%>%
  mutate(Region="TUMRA")

# ma<-st_read("GIS/Management_Areas_of_the_Great_Barrier_Reef_Marine_Park.geojson",quiet = T)%>%
#   select(AREA_DESCR, geometry)%>%rename(Name=AREA_DESCR)%>%
#   mutate(Region="GBRMPA_Management")
ma<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Management_Areas_of_the_Great_Barrier_Reef_Marine_Park/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(AREA_DESCR, geometry)%>%rename(Name=AREA_DESCR)%>%
  mutate(Region="GBRMPA.MA")
nrm<-nrm_regions%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  mutate(Region= "NRM")
# reefs2<-reef.LatLongs%>%
#   rename(Name=REEF)%>%
#   group_by(Name)%>%
#   summarise(LATITUDE=mean(LATITUDE), LONGITUDE= mean(LONGITUDE))%>%
#   st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

regions=ma%>%bind_rows(nrm,tumra, gbrmpa)%>%
  st_make_valid()%>%
  st_transform(4326)

##Ancilliary functions

##Get Reef
get_reef<-function(reefName){
  reef<-reefs%>%
    filter(Name==reefName)
  
  return(reef)
}

get_reefs<-function(selVal){
  
  this.region<-regions%>%
    filter(Name==selVal)%>%
    st_make_valid()%>%
    st_transform(4326)
  this.reefs<-reefs%>%
    st_filter(x = ., y=this.region)
  return(this.reefs)
}





###Composition ####
cc<-read.csv("outputs/GBR_LOF.csv")%>%
  mutate(Metric=paste0("k",k),
         Reef=str_replace(string=Reef,pattern = "Farquarson Reef", replacement ="Farquharson Reef"),
         Reference=case_when(
           Metric=="k6" ~ "Baseline",
           Metric=="k3" ~"Critical",
           .default=NA
         ),
         Level="reef",
         Lower=LOF,
         Upper=LOF,
         Indicator="Composition")%>%
  rename(Name=Reef, Median=LOF)%>%
  separate_wider_delim(Name, "_",names =  c("Name","Depth"), too_few = "align_start")%>%
  mutate(Name=as.factor(Name),
         Depth=str_remove(Depth, "m"),
         Depth=case_when(
           is.na(Depth) ~"deep slope",
           Depth=="5" ~"deep slope",
           Depth=="2" ~"shallow slope",
           .default = NA
         ),
         tn.reefs=NA,
         n.below=NA)%>%
  select(c(Level,Year,Name,Depth,Indicator,Metric,Reference,Median,Lower,Upper))%>%
  # group_by(Name,Year)%>%
  # fill(Median:Upper, .direction="down")%>% #Fill K6 with k3 values
  ungroup%>%
  group_by(Level,Year,Name,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

reef.df<-reefs%>%
  st_intersection(regions)%>%
  st_drop_geometry()%>%
  rename(Level=Region,Level.Name=Name.1)

idf.rg<-cc%>%
  ungroup%>%select(-c(Level, Lower,Upper))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  select(-c(Name, Depth))%>%
  filter(!is.na(Median))%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,Year,Indicator, Reference, Metric)%>%
  # posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  posterior::summarise_draws(Hmisc::smean.cl.boot )%>%
  rename(Name=Level.Name)%>%
  mutate(Level=factor(Level, 
                      levels=c('Reef','NRM','TUMRA','GBRMPA.MA','GBRMP')),
         Name=as.factor(Name),
         Reference=as.factor(Reference),
         Depth=NA,
         Latitude=NA,
         Longitude=NA
  )%>%
  ungroup%>%
  # rename(Median=median,Lower=lower,Upper=upper)%>%
  rename(Median=Mean)%>%
  select(-c(variable))
  # select(-c(mean, variable,sd))

cc.c<-cc%>%
  ungroup%>%
  select(-c(Level))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  group_by(Level,Level.Name,Metric, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5, na.rm = T))%>%
  rename(Name=Level.Name)

idf.rg<-idf.rg%>%
  left_join(cc.c, by=c("Level","Name","Metric","Year"))


cc<-cc%>%bind_rows(idf.rg)
rm(idf.rg)
#### Juveniles ####
##Baseline
load("outputs/reef.scores.distribution.juv.RData")
juv.b<-reef.scores.distribution.juv%>%
  mutate(Year=as.numeric(as.character(fYEAR)),
         Name=str_remove(string=REEF.d,pattern=fixed("shallow slope")),
         Name=str_remove(string=Name,pattern=fixed("deep slope")),
         Depth=DEPTH.f,
         Name=str_trim(Name),
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"))%>%
  select(Year,Name,Depth,rescale.dist.met)%>%
  posterior::as_draws() %>%
  group_by(Name, Year, Depth)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Indicator="Juvenile", Reference="Baseline", Level="reef", tn.reefs=NA,n.below=NA)%>%
  rename(Metric=variable, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Depth,Indicator,Metric,Reference,Median,Lower,Upper,tn.reefs,n.below)

juv.brg<-reef.scores.distribution.juv%>%
  mutate(Year=as.numeric(as.character(fYEAR)),
         Name=str_remove(string=REEF.d,pattern=fixed("shallow slope")),
         Name=str_remove(string=Name,pattern=fixed("deep slope")),
         Depth=DEPTH.f,
         Name=str_trim(Name),
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  select(Level,Level.Name, Year,rescale.dist.met)%>%
  filter(!is.na(rescale.dist.met))%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,  Year)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Indicator="Juvenile", Reference="Baseline", Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name,Metric=variable, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Latitude, Longitude, Depth,Indicator,Metric,Reference,Median,Lower,Upper)

juv.c<-juv.b%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  group_by(Level,Level.Name, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

juv.brg<-juv.brg%>%
  left_join(juv.c, by=c("Level","Name","Year"))

juv.b<-juv.b%>%bind_rows(juv.brg)
rm(reef.scores.distribution.juv,juv.brg, juv.c, to_complete)

##Critical
load("outputs/reef.scores.distribution.juv.Acrop.RData")
juv.cr<-reef.scores.distribution.juv.Acrop%>%
  mutate(Year=as.numeric(as.character(fYEAR)),
         Name=str_remove(string=REEF.d,pattern=fixed("shallow slope")),
         Name=str_remove(string=Name,pattern=fixed("deep slope")),
         Depth=DEPTH.f,
         Name=str_trim(Name),
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"),
         rescale.dist.met=crit.metric)%>% 
  select(Year,Name,Depth,rescale.dist.met)%>%
  posterior::as_draws() %>%
  group_by(Name, Year, Depth)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Indicator="Juvenile", Reference="Critical", Level="reef", tn.reefs=NA,n.below=NA)%>%
  rename(Metric=variable, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Depth,Indicator,Metric,Reference,Median,Lower,Upper,tn.reefs,n.below)

juv.crg<-reef.scores.distribution.juv.Acrop%>%
  mutate(Year=as.numeric(as.character(fYEAR)),
         Name=str_remove(string=REEF.d,pattern=fixed("shallow slope")),
         Name=str_remove(string=Name,pattern=fixed("deep slope")),
         Depth=DEPTH.f,
         Name=str_trim(Name),
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"),
         rescale.dist.met=(crit.metric*-1)+1)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  select(Level,Level.Name, Year,rescale.dist.met)%>%
  filter(!is.na(rescale.dist.met))%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,  Year)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Indicator="Juvenile", Reference="Critical", Depth=NA, 
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name,Metric=variable, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Latitude, Longitude, Depth,Indicator,Metric,Reference,Median,Lower,Upper)

juv.c<-juv.cr%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  group_by(Level,Level.Name, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

juv.crg<-juv.crg%>%
  left_join(juv.c, by=c("Level","Name","Year"))

juv.cr<-juv.cr%>%bind_rows(juv.crg)
juv<-juv.b%>%bind_rows(juv.cr)
rm(reef.scores.distribution.juv,juv.brg, juv.c, to_complete, juv.b, juv.cr, juv.crg)

#### Recovery ####
##Baseline
rp<-read_csv("outputs/RPI.baseline.index.temporal.csv")%>%select(-"...1")%>%
  mutate(Indicator="Performance", Reference="Baseline",
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"))

rp.brg<-rp%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  filter(!is.na(Median))%>%
  select(Level,Level.Name, Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,  Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Indicator="Performance", Reference="Baseline", Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c<-rp%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  group_by(Level,Level.Name, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.brg<-rp.brg%>%left_join(rp.c, by=c("Level","Name","Year"))

rp.b<-rp%>%bind_rows(rp.brg)

##Critical
rp<-read_csv("outputs/RPI.critical.index.temporal.csv")%>%select(-"...1")%>%
  mutate(Indicator="Performance", Reference="Critical",
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"))

rp.crg<-rp%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  filter(!is.na(Median))%>%
  select(Level,Level.Name, Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,  Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(mean, sd, median, HDInterval::hdi)%>%
  mutate(Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c<-rp%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  group_by(Level,Level.Name, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.crg<-rp.crg%>%left_join(rp.c, by=c("Level","Name","Year"))

rp.c<-rp%>%bind_rows(rp.crg)

rp<-rp.b%>%bind_rows(rp.c)


###COMPILE ALL INDICATORS
scores<-read_csv("outputs/Indices.csv")%>%
  group_by(Level,Year,Name,Latitude,Longitude,Depth,Indicator,Metric,Reference)%>%
  summarise(Median=mean(Median), Lower=mean(Lower),Upper=mean(Upper),tn.reefs=mean(tn.reefs), n.below=mean(n.below))

scores<-scores%>%
  filter(Level!="BIOREGION.agg")%>%bind_rows(cc, juv, rp)%>%
  filter(Year>=2007)%>%
  # group_by(Level,Year,Depth)%>%
  ungroup()
rm(list=setdiff(ls(), "scores"))
# %>%
#   complete(nesting(Indicator,Metric, Reference),fill =list(Median=NA,
#                       Upper=NA,
#                       Lower=NA,
#                       tn.reefs=NA,
#                       n.below=NA)
#   )
# 
# to_complete<-expand.grid(Indicator=c("Performance","Composition","Juvenile"),
#                          Metric=NA,
#                          Reference=c("Critical","Baseline"),
#                          Median=0.5,
#                          Lower=0.5,
#                          Upper=0.5,
#                          tn.reefs=NA,
#                          n.below=NA)

to_complete<-scores%>%ungroup%>%select(Indicator, Metric,Reference)%>%unique()

# 
to_complete<-scores%>%
  ungroup%>%
  dplyr::select(c(Level,Year,Name, Depth))%>%
  unique()%>%
  cross_join(to_complete)%>%
  anti_join(y=scores, x=., na_matches="na")%>%
  mutate(Median=NA,
                               Upper=NA,
                               Lower=NA,
                               tn.reefs=NA,
                               n.below=NA,
         Latitude=NA,Longitude=NA)
# 
scores<-scores%>%
  bind_rows(to_complete)

test=scores%>%group_by(Name,Year, Depth)%>%tally()%>%filter(n!=10)





reefs<-read_csv("outputs/Indices.csv")%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Depth,Latitude,Longitude))%>%
  unique()%>%
  group_by(Name)%>%
  mutate(Latitude=case_when(
    Depth=="shallow slope" ~ Latitude+0.0001, .default = Latitude),
    Longitude=case_when(
      Depth=="shallow slope" ~ Longitude+0.0001, .default = Longitude),
    p.name=sprintf("%s (%s)", Name, str_trim(str_remove(Depth,"slope"))))

test1<-scores%>%filter(Level=="reef")%>%select(Name, Depth)%>%unique()%>%
  anti_join(y=reefs)


scores<-scores%>%anti_join(x=., y=test1)%>%
  select(-c(Latitude,Longitude))%>%
  left_join(reefs)%>%
  mutate(Name=case_when(
    Level=="reef" ~p.name,
    .default=Name
  ))%>%
  select(-p.name)


save(scores, file="outputs/scores.Rdata")

