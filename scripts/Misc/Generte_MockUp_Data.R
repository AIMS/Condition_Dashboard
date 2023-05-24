##Generate Mock-up data for Dashboard 

rm(list=ls())

library(tidyverse)
library(sf)
library(dataaimsr)
library(gisaimsr)

setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")
source("scripts/Misc/HighLevel_Classification.R")
##Template
df<-expand.grid(
  Level=factor("", levels=c('Reef','NRM','TUMRA','GBRMPA.Mgmt','GBR')),
  Year=integer(),
  Name=factor(), # ReefName or Region Name
  # Latitute=numeric(), #Use NA for anything other than Reef
  # Longitude=numeric(), #Use NA for anything other than Reef
  Depth=factor("", levels=c(NA,'Shallow','Deep')), #Need to discuss how we are using this, so I am including NA in case we dont use it
  Indicator=factor("", levels=c('Coral.Cover','Macroalgae.Cover','Juvenile','Recovery.Performance','Community.Composition')),
  Reference=factor("", levels=c('Baseline','Critical')),
  Median=numeric(),
  Upper=numeric(),
  Lower=numeric(),
  tn.reefs=integer(),
  n.below=integer() #Number of reefs below reference threshold (e.g., Lower < 0.5)
)


setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")

##Load combined data from CoralIndex
load("outputs/indicators.reef.Rdata")
i.df<-indicators.reef
rm(indicators.reef)

all <- i.df %>%
  select(Name, Year, Depth, Indicator,indicator)%>%
  add_row(Name=unique(i.df$Name)[1], 
          Year=min(i.df$Year),
          Indicator="Juvenile"
  )%>%
  expand(Name, Year, Indicator)%>%
  mutate(Surveyed=F)

indicators.reef<-i.df%>%
  filter(Depth=="Deep")%>%
  select(Name, Year, Indicator, Score,.lower, .upper)%>%  
  add_row(Name=unique(i.df$Name)[1], 
          Year=min(i.df$Year),
          Indicator="Juvenile",
          Score=NA,.lower=NA,.upper=NA
  )%>%
  group_by(Name, Year, Indicator, Score, .upper)%>%
  mutate(
    Indicator=str_replace(Indicator," ","."),
    Score=case_when(
      is.na(Score) ~ 0.5,
      .default = Score),
    .lower=case_when(
      is.na(.lower) ~ Score,
      .default = .lower),
    .upper=case_when(
      is.na(.upper) ~ Score,
      .default = .upper)
    
  )%>%
  select(Name, Year, Indicator,Score,.lower, .upper)%>%
  ungroup()%>%
  complete(Name, Year, Indicator)
# spread(Indicator,.lower, fill=NA)%>%
# mutate(Juvenile=NA)%>%
# gather("Indicator",".lower", -c(Name,Year, Score,.upper))%>%
# mutate(
#   crit=case_when(
#     .lower>=0.5 ~T,
#     .default=F
#   ))

ns<-indicators.reef%>%
  group_by(Name, Year)%>%
  mutate(v=is.na(Score))%>%
  summarise(s=sum(v))%>%
  mutate(Surveyed=case_when(
    s==5 ~ FALSE,
    .default = TRUE
  ))


indicators.reef<-indicators.reef%>%
  left_join(ns, by=c("Name", "Year"))%>%
  mutate(Score=case_when(
    (Surveyed==TRUE & is.na(Score)) ~ 0.5,
    .default = Score),
    .lower=case_when(
      (Surveyed==TRUE & is.na(.lower)) ~ Score,
      .default = .lower),
    .upper=case_when(
      (Surveyed==TRUE & is.na(.upper) & .lower==1) ~ 1,
      (Surveyed==TRUE & is.na(.upper) & .lower!=1) ~ Score,
      .default = .upper)
  )

indicators.reef<-indicators.reef%>%
  filter(Surveyed==T)%>%
  mutate(sd=(.upper-.lower)/1.96)%>%
  group_by(Name,Year,Indicator)%>%
  expand(Score=rnorm(50,mean=Score,sd=sd))

indicators.reef<-indicators.reef%>%mutate(
  Reference='Baseline',Depth=NA)


# all.s <- anti_join(all,indicators.reef,by=c("Name", "Year"))
# indicators.reef<-indicators.reef%>%
#   bind_rows(all.s%>%
#               mutate(Score=NA,.lower=NA,.upper=NA ))

load("GIS/reef.LatLongs.RData")
reefs=list(reef.LatLongs$REEF)

gbrmpa<-st_read("GIS/Great_Barrier_Reef_Marine_Park_Boundary.geojson",quiet = T)%>%
  mutate(Region="GBR", Name="GBRMP")%>%
  select(Name,Region, geometry)
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
  mutate(Region="GBRMPA_Management")
nrm<-nrm_regions%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  mutate(Region= "NRM")
reefs<-reef.LatLongs%>%
  select(-DEPTH)%>%
  rename(Name=REEF)%>%
  group_by(Name)%>%
  summarise(LATITUDE=mean(LATITUDE), LONGITUDE= mean(LONGITUDE))%>%
  st_as_sf(., coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

regions=ma%>%bind_rows(nrm,tumra, gbrmpa)%>%
  st_make_valid()%>%
  st_transform(4326)



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

idf.r<-indicators.reef%>%mutate(Level='Reef')%>%
  group_by(Level,Name, Year,Indicator,Reference,Depth)%>%
  summarise(ci = list(enframe(Hmisc::smean.cl.boot(Score)))) %>% 
  unnest(cols=c(ci)) %>% 
  spread(name, value)%>%
  mutate(Level=factor("Reef", 
                      levels=c('Reef','NRM','TUMRA','GBRMPA_Management','GBR')),
         Name=as.factor(Name),
         Depth=as.factor(Depth),
         Reference=as.factor(Reference)
         )

reef.df<-reefs%>%
  st_intersection(regions)%>%
  st_drop_geometry()%>%
  rename(Level=Region,Level.Name=Name.1)
  
  
idf.rg<-indicators.reef%>%
  right_join(reef.df, relationship = 'many-to-many')%>%
  group_by(Level,Level.Name, Year, Indicator, Reference, Depth)%>%
  summarise(ci = list(enframe(Hmisc::smean.cl.boot(Score)))) %>% 
  unnest(cols=c(ci)) %>% 
  spread(name, value)%>%
  rename(Name=Level.Name)%>%
  mutate(Level=factor(Level, 
                      levels=c('Reef','NRM','TUMRA','GBRMPA_Management','GBR')),
         Name=as.factor(Name),
         Depth=as.factor(Depth),
         Reference=as.factor(Reference)
  )


indicators.reef<-idf.r%>%
  bind_rows(idf.rg)%>%
  mutate(Lower=case_when(
    is.na(Lower)~Mean,
    .default=Lower),
    Upper=case_when(
      is.na(Upper)~Mean,
      .default=Upper))


save(indicators.reef, file=file.path("outputs","Mocked_indicators.reef.Rdata"))

r.con<-Cond.Class(indicators.reef)
indicators.reef<-indicators.reef%>%
  left_join(r.con, by=c('Name','Year'))

save(indicators.reef, file=file.path("outputs","Mocked_indicators.reef.Rdata"))
