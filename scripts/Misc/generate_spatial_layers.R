library(sf)
library(tidyverse)
library(gisaimsr)

###LOAD GEOSPATIAL DATA

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
  mutate(Region= "NRM")|> 
  st_transform(crs=st_crs(ma))
bior<-st_read("C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/GBRMPA/Great_Barrier_Reef_Marine_Park_Marine_Bioregions_202_-800268112198236042.geojson") |> 
  select(BIOREGION, geometry)%>%rename(Name=BIOREGION)%>%
  mutate(Region= "BIOREGION.agg") |> 
  st_transform(crs=st_crs(ma)) |> 
  mutate(Name=as.character(Name)) |> 
  group_by(Region,Name) |> 
  st_make_valid() |> 
  summarise()
##Merge Bioregions that where combined because of the limited amount of monitoring sites
# load("indicators_dataframe.Rdata")
# mbio<-i.df |>  filter(Aggregation=="BIOREGION.agg", str_detect(Name, ":") ) |> select(Name) |> unique()
bmerged<-bior |> 
  filter(Name %in% c("35", "36", "4","3")) |>
  mutate(agg=case_when(Name %in% c("35", "36") ~ 1,
                       .default=2)) |> 
  group_by(Region,agg) |> 
  summarise() |> 
  mutate( Name=case_when(agg ==1 ~ "35:36",
                         .default="4:3")) |> 
  select(-agg)

bior<- bior |> 
  bind_rows(bmerged)

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

tumra<-tumra%>%bind_rows(D)

##Create Reef Site locations from monitoring data
load("scores.Rdata")
scores<-read_csv("Indices.csv")%>%
  mutate(Name=case_when(Level=="reef" ~ paste0(Name," (",Depth,")"),
                        .default=Name))
reefs<-scores |> filter(Level=='reef') |> select(Level,Name, Latitude, Longitude) |> rename(Region=Level) |> unique() 
reefs<-st_as_sf(reefs,coords = c("Latitude","Longitude"), crs=st_crs(gbrmpa))

#Combine all spatial aggregation layers
regions=ma%>%bind_rows(nrm,tumra,zones, gbrmpa, bior, reefs)

##Save file
write_sf(regions, "spatial_aggregations.geojson",delete_layer = T)
