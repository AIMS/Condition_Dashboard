###
library(tidyverse)
library(readr)
library(sf)
library(gisaimsr)

library(posterior)

library(tidybayes)
rm(list = ls())

##data compiled by Murray
scores<-read_csv("C:\\Users\\mgonzale\\OneDrive - Australian Institute of Marine Science\\projects\\RIMReP\\Habitat_Indicators\\data\\processed\\Indices.csv")

##Recovery performance from Kerryn
load("c:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/data/recovery/RPI.reference_2022.RData")
load("c:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/data/recovery/RPI.reference.distribution_2022.RData")



##Load regions
gbrmpa<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Great_Barrier_Reef_Marine_Park_Boundary/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  mutate(Region="GBRMP", Name="GBRMP")%>%
  select(Name,Region, geometry)
tumra<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Traditional_Use_of_Marine_Resources_Agreement_areas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  group_by(Name)%>%
  mutate(Region="TUMRA")
ma<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Management_Areas_of_the_Great_Barrier_Reef_Marine_Park/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(AREA_DESCR, geometry)%>%rename(Name=AREA_DESCR)%>%
  mutate(Region="GBRMPA.MA")
nrm<-nrm_regions%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  mutate(Region= "NRM")
## Zones
northern.bbox <- st_bbox(c(xmin = 142, xmax = 155, ymin = -15.4, ymax = 0)) %>%
  st_as_sfc() %>%
  st_sf(crs = st_crs(gbrmpa)) %>%
  mutate(Zone = 'Northern')
central.bbox <- rbind(c(142,-20.7),
                      c(148.7,-20.7),
                      c(152,-19.6),
                      c(152,-15.4),
                      c(142,-15.4)) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(gbrmpa)) %>%
  st_sf() %>%
  mutate(Zone = 'Central')



southern.bbox <- rbind(c(142,-20.7),
                       c(148.7,-20.7),
                       c(152,-19.6),
                       c(155,-19.6),
                       c(155,-25),
                       c(142,-25)) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(gbrmpa)) %>%
  st_sf() %>%
  mutate(Zone = 'Southern')
zones.bbox <- rbind(northern.bbox, central.bbox, southern.bbox)

zones <- gbrmpa %>%
  st_intersection(zones.bbox) %>%
  mutate(Region = 'ZONE',
         Name = Zone) %>%
  dplyr::select(-Zone) %>%
  suppressMessages() %>%
  suppressWarnings()
D<-rbind(c(149.673,-22.5),
         c(149.79,-22.39),
         c(149.87,-22.03),
         c(150.24,-21.6),
         c(152.8,-21.6),
         c(152.8,-22.35),
         c(151.27,-23.085),
         c(150.87,-23.58))%>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(gbrmpa)) %>%
  st_sf() %>%
  mutate(Region="TUMRA",Name = 'Darumbal')%>%
  st_make_valid()

tumra<-tumra%>%bind_rows(D)


regions=ma%>%bind_rows(nrm,tumra, zones,gbrmpa)%>%st_make_valid()

reefs<-scores%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Latitude,Longitude))%>%
  unique()%>%
  group_by(Name)%>%
  summarise(Latitude=mean(Latitude), Longitude= mean(Longitude))%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)

reef.df<-reefs%>%
  st_intersection(regions)%>%
  st_drop_geometry()%>%
  rename(Level=Region,Level.Name=Name.1)

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


##summarise recovery

RPI.reference<-RPI.reference %>%
  ungroup%>%
  group_by(Name, Depth) %>%
  arrange(Name, Depth, Year) %>%
  tidyr::fill(Median, Upper, Lower)

##summary per shelf

rp.brg<-RPI.reference.distribution%>%
  ungroup()%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  # filter(!is.na(Median))%>%
  select(Level,Level.Name, Shelf,Year,Indicator, Metric,Reference,rescale.dist.met)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,Shelf,  Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median, HDInterval::hdi, .args=list(na.rm=T, credMass=0.9))%>%
  mutate( Depth=NA,
          Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Name,Shelf, Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c<-RPI.reference%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  group_by(Level,Level.Name,Shelf, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.brg<-rp.brg%>%left_join(rp.c, by=c("Level","Name","Shelf","Year"))



##summary without Shelf

rp.brg.s<-RPI.reference.distribution%>%
  ungroup()%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  # filter(!is.na(Median))%>%
  select(Level,Level.Name, Year,Indicator, Metric,Reference,rescale.dist.met)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median, HDInterval::hdi, .args=list(na.rm=T, credMass=0.9))%>%
  mutate(Shelf="All", Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  ungroup%>%
  select(Level,Year,Shelf,Name,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c.s<-RPI.reference%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  mutate(Shelf="All")%>%
  group_by(Level,Level.Name,Shelf, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.brg.s<-rp.brg.s%>%left_join(rp.c.s, by=c("Level","Name","Shelf","Year"))

##Baseline Indicator 
rp.b<-RPI.reference%>%bind_rows(rp.brg, rp.brg.s)

#Critical Indicator
rp<-read_csv("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/outputs/RPI.critical.index.temporal.csv")%>%select(-"...1")%>%
  mutate(Indicator="Recovery.performance", Reference="Critical",
         Name=str_replace(string=Name,pattern = "Farquarson Reef", replacement ="Farquharson Reef"))

reef.shelf<-RPI.reference%>%
  select(Name, Shelf)%>%
  unique()

reef.shelf<-scores%>%
  filter(Level=="reef")%>%
  select(Name, Shelf)%>%
  unique()

##Critical by Shelf
rp.crg<-rp%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  left_join(reef.shelf, by="Name")%>%
  filter(!is.na(Median))%>%
  select(Level,Level.Name,Shelf, Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name, Shelf, Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median,HDInterval::hdi,.args=list(credMass=0.9))%>%
  # posterior::summarise_draws(Hmisc::smean.cl.boot )%>%
  mutate(Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  # rename(Name=Level.Name, Median=Mean)%>%
  ungroup%>%
  select(Level,Year,Name,Shelf,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c<-rp%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  left_join(reef.shelf, by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  group_by(Level,Level.Name, Shelf,Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.crg<-rp.crg%>%left_join(rp.c, by=c("Level","Name","Shelf","Year"))

##Critical across Shelf
rp.crg.s<-rp%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  filter(!is.na(Median))%>%
  select(Level,Level.Name, Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name, Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median,HDInterval::hdi,.args=list(credMass=0.9))%>%
  # posterior::summarise_draws(Hmisc::smean.cl.boot )%>%
  mutate(Shelf="All",Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  # rename(Name=Level.Name, Median=Mean)%>%
  ungroup%>%
  select(Level,Year,Name,Shelf,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

rp.c.s<-rp%>%
  select(-Level)%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  ungroup%>%
  filter(!is.na(Median))%>%
  mutate(Shelf="All")%>%
  group_by(Level,Level.Name, Shelf,Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5))%>%
  rename(Name=Level.Name)

rp.crg.s<-rp.crg.s%>%left_join(rp.c.s, by=c("Level","Name","Shelf","Year"))

reefs.coord<-scores%>%
  filter(Level=="reef")%>%
  select(Name, Depth,Shelf, Latitude,Longitude)%>%
  unique()

rp.c<-rp%>%
  unique()%>%
  ungroup%>%
  select(-c(Latitude, Longitude))%>%
  left_join(reefs.coord, by=c("Name", "Depth"))%>%
  filter(!is.na(Latitude))%>%
  bind_rows(rp.crg)

#Recovery indicator
rp<-rp.b%>%mutate(Year=as.numeric(as.character(Year)))%>%bind_rows(rp.c)

# rp<-rp.b%>%bind_rows(old_scores)

scores<-scores%>%bind_rows(rp)



##COMUNITY COMPOSITION ###
#within shelfs
cc.r<-scores%>%
  filter(Indicator=="Community.composition", Level=="reef")%>%
  select(-c(Level,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  # left_join(reef.shelf, by="Name")%>%
  select(Level,Level.Name, Shelf,Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name,Shelf, Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median,HDInterval::hdi,.args=list(credMass=0.9, na.rm=T))%>%
  # posterior::summarise_draws(Hmisc::smean.cl.boot )%>%
  mutate(Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  # rename(Name=Level.Name, Median=Mean)%>%
  ungroup%>%
  select(Level,Year,Name,Shelf,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

reefs.coord<-scores%>%
  filter(Level=="reef")%>%
  select(Name, Depth,Shelf, Latitude,Longitude)%>%
  unique()



cc.c<-scores%>%
  filter(Indicator=="Community.composition", Level=="reef")%>%
  ungroup%>%
  select(-c(Level))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  # left_join(reef.shelf, by="Name")%>%
  ungroup%>%
  group_by(Level,Level.Name,Shelf,Metric, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5, na.rm = T))%>%
  rename(Name=Level.Name)

cc<-cc.r%>%
  left_join(cc.c, by=c("Level","Name","Shelf","Metric","Year"))

#Across shelfs
cc.r.s<-scores%>%
  filter(Indicator=="Community.composition", Level=="reef")%>%
  select(-c(Level,Shelf,Latitude,Longitude))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  select(Level,Level.Name, Year,Indicator, Metric,Reference,Median)%>%
  posterior::as_draws() %>%
  group_by(Level,Level.Name, Year, Indicator, Metric,Reference)%>%
  posterior::summarise_draws(median,HDInterval::hdi,.args=list(credMass=0.9, na.rm=T))%>%
  # posterior::summarise_draws(Hmisc::smean.cl.boot )%>%
  mutate(Shelf="All",Depth=NA,
         Latitude=NA, Longitude=NA)%>%
  rename(Name=Level.Name, Median=median, Lower=lower,Upper=upper)%>%
  # rename(Name=Level.Name, Median=Mean)%>%
  ungroup%>%
  select(Level,Year,Name,Shelf,Latitude,Longitude,Depth,Indicator,Metric,Reference,Median,Lower,Upper)

reefs.coord<-scores%>%
  filter(Level=="reef")%>%
  select(Name, Depth,Shelf, Latitude,Longitude)%>%
  unique()



cc.c.s<-scores%>%
  filter(Indicator=="Community.composition", Level=="reef")%>%
  ungroup%>%
  select(-c(Level, Shelf))%>%
  left_join(reef.df, relationship = 'many-to-many', by="Name")%>%
  left_join(reef.shelf, by="Name")%>%
  ungroup%>%
  group_by(Level,Level.Name,Metric, Year)%>%
  summarise(tn.reefs=n(), n.below=sum(Upper<0.5, na.rm = T))%>%
  rename(Name=Level.Name)%>%
  mutate(Shelf="All")

cc.s<-cc.r.s%>%
  left_join(cc.c.s, by=c("Level","Name","Shelf","Metric","Year"))


cc<-scores%>%
  filter(Indicator=="Community.composition", Level=="reef")%>%bind_rows(cc, cc.s)


scores<-scores%>%
  filter(Indicator!="Community.composition")%>%
  bind_rows(cc)
###Fill-in missing values
to_complete<-scores%>%ungroup%>%select(Indicator, Metric,Reference)%>%unique()

# 
to_complete<-scores%>%
  ungroup%>%
  filter(Level!="BIOREGION.agg",Year>=2007)%>%
  dplyr::select(c(Level,Year,Shelf,Name, Depth))%>%
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



write_csv(scores,"C:\\Users/mgonzale/Documents/GitHub/ReefCondition_Dashboard/Indices.csv")

