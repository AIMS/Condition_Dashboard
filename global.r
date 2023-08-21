library(shinydashboard)
library(leaflet)
library(leaflet.esri)
library(tidyverse)
library(readr)
library(sf)
library(gisaimsr)
library(data.table)
library(fontawesome)
library(dataaimsr)
library(shinythemes)
library(shinyWidgets)
# library(shiny)
# library(shinydashboard)

# library(gisaimsr)
# library(sf)
# library(leaflet)

# library(shiny)

#Ancilliary Functions
source("scripts/plotting_functions/reef_mapping.R")
source("scripts/plotting_functions/radial.plot.summary.R")
source("scripts/plotting_functions/temporal_plots.R")
source("scripts/plotting_functions/comp_change.R")
source("scripts/plotting_functions/DonutCond.R")
source("scripts/Misc/HighLevel_Classification.R")
source("scripts/Misc/Rep_3.1_Regional_autoText_working.R")
source("scripts/Misc/Rep_3.1_Reef_autoText.R")

get_reef<-function(reefName){
  reef<-reefs%>%
    filter(Name==reefName)
  return(reef)
}

get_reefs<-function(selVal="Townsville/Whitsunday Management Area"){
  if(length(selVal)==0){
    this.reefs=NA
  }else{
    
    this.region<-regions%>%
      filter(Name==selVal)%>%
      st_make_valid()%>%
      st_transform(4326)
    this.reefs<-reefs%>%
      st_filter(x = ., y=this.region)
  }
  return(this.reefs)
}
# ##DEBUGING
# input=list(region_select="NRM",
#            report_year=2022,
#            value_select="Fitzroy",
#            reference="Critical",
# shelf="Inshore")
# input=list(region_select="Zones",
#            report_year=2021,
#            value_select="Central",
#            reference="Baseline",
# shelf="All")


# input=list(region_select="GBRMPA.MA",
# report_year=2021,
# value_select="Townsville/Whitsunday Management Area",
# shelf="All")
# 
# input=list(region_select="reef",
#            report_year=2022,
#            value_select="Gannet Cay (deep slope)")
# output=list()
# input=list(region_select="TUMRA",
#            report_year=2022,
#            value_select="Darumbal",
# shelf="All")




##Load Files####
# load("GIS/reef.LatLongs.RData")
# reefs=list(reef.LatLongs$REEF)
gbrmpa<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Great_Barrier_Reef_Marine_Park_Boundary/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  mutate(Region="GBR", Name="GBRMP")%>%
  select(Name,Region, geometry)
tumra<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Traditional_Use_of_Marine_Resources_Agreement_areas/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  group_by(Name)%>%
  mutate(Region="TUMRA")
ma<-st_read("https://services8.arcgis.com/ll1QQ2mI4WMXIXdm/arcgis/rest/services/Management_Areas_of_the_Great_Barrier_Reef_Marine_Park/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",quiet = T)%>%
  select(AREA_DESCR, geometry)%>%rename(Name=AREA_DESCR)%>%
  mutate(Region="GBRMPA_Management")
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

##Additional TUMRA (Durambal) not available from GBRMPA geoportal
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
D
tumra<-tumra%>%bind_rows(D)

regions=ma%>%bind_rows(nrm,tumra,zones, gbrmpa)

##Load data
#Placeholder while Recovery Data gets integrated by Murray
scores<-read_csv("Indices.csv")%>%
  mutate(Name=case_when(Level=="reef" ~ paste0(Name," (",Depth,")"),
                        .default=Name))
comp<-read_csv("Composition_change.csv")%>%
  mutate(REEF= paste0(REEF," (",DEPTH.f,")"))

reefs<-scores%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Depth,Latitude,Longitude))%>%
  unique()%>%
  filter(!is.na(Latitude))%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)




