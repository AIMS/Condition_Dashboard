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
#            report_year=2021,
#            value_select="Fitzroy",
#            reference="Critical")

# input=list(region_select="GBRMPA.MA",
# report_year=2021,
# value_select="Townsville/Whitsunday Management Area")

# input=list(region_select="Reef",
# report_year=2021,
# value_select="Carter Reef (deep)")
# output=list()
# input=list(region_select="TUMRA",
#            report_year=2021,
#            value_select="Lama Lama")




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
regions=ma%>%bind_rows(nrm,tumra, gbrmpa)
##Load data
scores<-read_csv("scores_ci90.csv")

reefs<-scores%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Depth,Latitude,Longitude))%>%
  unique()%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)




