
library(gridExtra)
library(patchwork)
library(cowplot)
library(dataaimsr)
library(gisaimsr)
library(sf)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tidyverse)
library(paletteer)
library(ggtext)
source("scripts/Misc/HighLevel_Classification.R")
source("scripts/plotting_functions/radial.plot.summary.R")


## Load Data ####
load("indices.RData")
indices<-indices |> 
filter(!Reference == "Combined") |> #This is the straight average of the baseline and critical scores. Removed to avoid confusion, since we're not using it
  #since the original code probably uses the term 'Combined' throughout, set the correct Reference level to be called 'Combined'
  mutate(Reference=ifelse(Reference=="Combined_adjusted", "Combined", Reference)) |>
  mutate(fYEAR=Year, Year=as.numeric(as.character(Year)),
         Reference=case_when(
          #  (Reference=="Combined") & 
          #    (Indicator %in% c("Community.composition", "Recovery.performance")) ~ "old_combined",
           (Reference=="Baseline") &
             (Indicator == "Community.composition") ~ "Combined",
           .default=Reference),
         Median=ifelse(is.na(Median), 0.5, Median),
         Upper=ifelse(is.na(Upper), 0.5, Upper),
         Lower=ifelse(is.na(Lower), 0.5, Lower))
regions<-nrm_regions%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  mutate(Region= "NRM")
nrm<-
  st_read("C://Users/mgonzale/OneDrive - Australian Institute of Marine Science/GIS_Datasets/NRM_MarineRegions/NRM_MarineRegions.shp")
  #st_read("data/spatial/NRM Regions/NRM_MarineRegions.shp")

reefs<-indices%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Latitude,Longitude))%>%
  unique()%>%
  filter(!is.na(Latitude))%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)

##Ancillary Functions and Formats ####
scale_fill_class.c <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#FC6500" , "#48C617", "#48C617"), 
                      c( "Below","Within", "Above")),
    drop=F,
    ...
  )
}

get_reefs<-function(selVal="Fitzroy"){
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

# Define a shift function
shift_location <- function(x, offset) {
  y=x
  # Add the offset to the geometry column
  st_geometry(y) <- st_geometry(y) + c(offset,0)
  y=y |> st_set_crs(st_crs(x))
  
  return(y)
}

my_theme=
  theme(
    axis.text = element_text(size=16, vjust = 0.5,family = "Bell MT"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.title = element_text(size=18, face = "bold", family = "Bell MT", margin = margin(r=10, unit="mm")),
  )
#Figure 3 - Case Study 3 ####### 
## GBR NRMs ####
## Panel A: Map
ref="Combined"
conf_thsld<-0.8
regional<-indices |> filter(Level=="NRM", Reference=="Combined", Year==2024) |> 
  group_by(Year, Name, Shelf)|>
  summarise(#Cond=Cond.Class(
             Cond=Cond.Class.D(
    df=data.frame(Name,Depth,Year,Indicator, Median, Upper, Lower, p_below_0.5),conf=conf_thsld )$Class
  )

#Define inshore/offshore NRMs
nrm.offshore<- nrm |> group_by(NAME) |> 
  st_intersection(wbodies |> filter(MarineWate=="Offshore")  |> 
                    st_transform(crs = st_crs(nrm))) |> 
  mutate(Shelf="Offshore") |>  st_collection_extract(
    type = c("POLYGON"),
    warn = FALSE
  )

nrm.inshore<-nrm |>  group_by(NAME) |> st_difference(nrm.offshore) |> filter(NAME==NAME.1) |> 
  st_collection_extract(
    type = c("POLYGON"),
    warn = FALSE
  ) |> 
  mutate(Shelf="Inshore")

nrm<-nrm.offshore |> bind_rows(nrm.inshore) |> 
  select(NAME,Shelf) |> 
  rename(Name=NAME)

nrm<-nrm |> left_join(regional) |> 
  mutate(Cond=factor(Cond, levels=c("Good","Watch","Warning I" ,"Warning II","Critical" )))

nrm<-nrm |> st_make_valid()
##Map condition for NRMs in 2024
nrm_map_pad<-+c(+1, +2.5,+2.11,0)
nrm_map<-
  tm_shape(nrm, bbox = st_bbox(nrm)+nrm_map_pad)+
  tm_polygons(fill="Cond",
              fill.scale=tm_scale(values=
                                    c("#00496FFF", "#0F85A0FF", "#EDD746FF", "#ED8B00FF", "#DD4124FF")),
              fill.legend =  tm_legend(
                title = "Reef Habitat\ncondition",
                orientation ="portrait",
                width = 4,height = 10,
                title.size = 0.8,margins = c(0,0,0,0),
                title.padding = c(0.5,0,0.5,0),frame = FALSE,
                bg = FALSE,
                position = tm_pos_in("left", "bottom")
              )) +
  tm_borders(lwd = 3, col = "white") +
  tm_shape(gbr_feat |>  filter(FEAT_NAME=="Mainland"), bbox = st_bbox(nrm)+c(+1, +1,+1,0))+
  tm_polygons(fill="lightgray")+
  tm_shape(nrm |> filter(Shelf=="Offshore") |> 
             mutate(Name=str_wrap(Name,width=10, whitespace_only = TRUE)))+
  tm_labels( text= "Name",size = 0.9, xmod = +1.8,ymod=1,
             options = opt_tm_labels(
    point.label = FALSE,point_per = "largest",
    remove_overlap = TRUE ))+
  tm_layout(
    component.autoscale = FALSE,
    frame = FALSE,
    asp = NA, # Adjust aspect ratio to data
    outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
    inner.margins = c(0.00, 0.00, 0.00, 0.00)) 
nrm_map
### Panel B: Indicator Scores 

cond<-indices |> 
  filter(Year==2024, Level=="NRM", Reference==ref, 
         Shelf=="Inshore", Name=="Fitzroy") |> 
  mutate(Classification=case_when(
    Lower > 0.5 ~ "Above",
    Upper < 0.5 ~ "Below",
    is.na(Lower) ~ NA,
    .default="Within"))

cond.plot<-radial.plot.summary(dat= cond, ref=ref)+
  scale_fill_class.c()+
  scale_x_discrete(labels = label_wrap_gen(width = 10)) + # Wrap x-axis labels at 10 characters
  labs(title =  nrm |> filter(Name=="Fitzroy", Shelf=="Inshore") |> pull(Cond) )+
  my_theme+
  theme(axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y= element_blank(),
        plot.title = element_textbox_simple(
          fill = "#ED8B00FF", # Background color of the textbox
          color = "white",    # Text color
          box.color = NULL, # Border color of the textbox
          r = unit(8, "pt"),   # Radius for rounded corners 
          padding = unit(c(5, 5, 5, 5), "pt"), # Padding around the text
          width = NULL,size = 16,
          margin = unit(c(0, 0, 0, 0), "pt"),  # Margin around the textbox
          halign = 0.5,        # Horizontal alignment of the text
          hjust = 1
        )
  )
  
cond.plot

## Panel C: Map of reefs in Critical condition within Fitzroy
r<-"Fitzroy"
sites<-get_reefs(r)
rc<-indices |> 
  filter(Year==2024, Level=="reef", Name %in% sites$Name, Reference==ref, Shelf=="Inshore") |> 
  mutate((across(.cols = c(Median, Lower, Upper), .fns = ~ ifelse(is.na(.), 0.5, .), .names = "{.col}"))) |> 
  #Cond.Class(conf=conf_thsld) |>
  Cond.Class.D(conf=conf_thsld) |> 
  mutate(Class=factor(Class, levels=c("Good","Watch","Warning I", "Warning II", "Critical")),
         label=case_when(
           isTRUE(str_detect(Name, "Keppel")) ~ "Keppels",
           .default=Name)
         ) |> 
  left_join(sites) |> 
  ungroup() |> 
  st_as_sf()

fitz.map.pad<-c(-1.08,+0.15,0.18,0.1)
fitz.map<-
  tm_shape(gbr_feat |>  filter(FEAT_NAME %in% c("Mainland", "Island")),
           bbox = st_bbox(rc)+fitz.map.pad)+
  tm_polygons(fill="lightgray")+
  tm_shape(gbr_feat |>  filter(FEAT_NAME %in% c("Reef")),
           bbox = st_bbox(rc)+fitz.map.pad)+
  tm_fill(fill="#E6E6E6")+
  # tm_shape(rc |> filter(Depth=="shallow slope"),
  tm_shape(st_jitter(rc,0.03),
                   st_bbox(rc)+fitz.map.pad)+
  tm_symbols(fill="Class", size = 0.62,fill_alpha = 0.7, 
                                      fill.scale=tm_scale(values=c("#00496FFF", "#0F85A0FF", "#EDD746FF", "#ED8B00FF", "#DD4124FF")),
                                      fill.legend =  tm_legend(
                                        title = "Reef Habitat\ncondition (Fitzroy NRM)",
                                        orientation ="portrait",
                                        width = 4,height = 10,
                                        title.size = 0.9,margins = c(0,0,0,0),
                                        title.padding = c(0.5,0,0.5,0),frame = FALSE,
                                        text.size = 0.8,
                                        bg = FALSE,
                                        position = tm_pos_in("left", "bottom")
                                      ))+
  # tm_borders(col = NA)+
  tm_shape(rc |> filter(Depth=="shallow slope", Class %in% c("Critical", "Warning I", "Warning II")))+
  tm_labels( text= "label",size = 0.9, options = opt_tm_labels(
    point.label=TRUE,
    clustering = TRUE, remove_overlap = FALSE))+
  tm_layout(
    component.autoscale = F,
    frame = FALSE,
    asp = NA, # Adjust aspect ratio to data
    outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
    inner.margins = c(0.00, 0.00, 0.00, 0.00)) 

fitz.map


Fig3<-plot_grid(tmap_grob(nrm_map),NULL,tmap_grob(fitz.map), NULL,cond.plot,
          nrow = 1, ncol = 5,
          labels = c("A",  NA,"B",NA,"C"),
          label_colour = "black", 
          rel_widths = c(1, -0.05, 1, 0, 0.8))

#ggsave("scripts/case_studies/Fig3.png", Fig3, width = 380, height = 140,units = "mm")
ggsave("scripts/case_studies/Fig3_DendroD.png", Fig3, width = 380, height = 140,units = "mm")

# cond<-indices |> 
#   filter(Year==2024, Level=="NRM", Reference==ref, 
#          Shelf=="Inshore", Name=="Fitzroy") |> 
#   mutate(Classification=case_when(
#     Lower > 0.5 ~ "Above",
#     Upper < 0.5 ~ "Below",
#     is.na(Lower) ~ NA,
#     .default="Within"))
# 
# cond.plot<-radial.plot.summary(dat= cond, ref=ref)+
#   scale_fill_class.c()+
#   scale_x_discrete(labels = label_wrap_gen(width = 10)) + # Wrap x-axis labels at 10 characters
#   theme(axis.text.x = element_text(size=16, vjust = 0.5, face="bold", family = "Bell MT"))

# #COTS control in Fitzroy #####
# r<-"Wet Tropics"
# sites<-get_reefs(r)
# rc<-scores |> 
#   filter(Year==2024, Level=="reef", Name %in% sites$Name, Reference=="Baseline", Depth=="deep slope" ) |> 
#   mutate((across(.cols = c(Median, Lower, Upper), .fns = ~ ifelse(is.na(.), 0.5, .), .names = "{.col}"))) |> 
#   Cond.Class() |> 
#   mutate(Class=factor(Class, levels=c("Good","Watch","Warning I", "Warning II", "Critical"))) |> 
#   left_join(sites) |> 
#   ungroup() |> 
#   st_as_sf()
# rs.b<-scores |> 
#   filter(Year==2024, Level=="reef", Name %in% sites$Name, Reference=="Baseline", Depth=="deep slope" ) |> 
#   mutate((across(.cols = c(Median, Lower, Upper), .fns = ~ ifelse(is.na(.), 0.5, .), .names = "{.col}")),
#          status=case_when(
#            round(Upper,2) < 0.5 ~ "Below",
#            round(Lower,2) > 0.5 ~ "Above",
#            .default="Within"
#          )) |> 
#   select(Year, Name, Indicator, status) |> 
#   left_join(sites) |> 
#   ungroup() |> 
#   st_as_sf()
# 
# 
# my_bbox=st_bbox(sites)+c(-0.2,-0.15,0.2,0.15)
# 
# class.map<-tm_shape(gbr_feat, bbox = my_bbox)+
#   tm_fill(fill="lightgrey")+
#   tm_shape(rc, bbox=my_bbox)+tm_symbols(fill="Class", fill_alpha = 0.7, 
#                                         fill.scale=tm_scale(values=c("#00496FFF", "#0F85A0FF", "#EDD746FF", "#ED8B00FF", "#DD4124FF")),
#                                         fill.legend =  tm_legend(
#                                           title = "Overall \ncondition",
#                                           orientation ="portrait",
#                                           width = 4,height = 10,
#                                           title.size = 0.8,margins = c(0,0,0,0),
#                                           title.padding = c(0.5,0,0.5,0),frame = FALSE,
#                                           bg = FALSE,
#                                           position = tm_pos_in("left", "bottom")
#                                         ))+
#   tm_layout(
#     component.autoscale = FALSE,
#     frame = FALSE,
#     asp = NA, # Adjust aspect ratio to data
#     outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
#     inner.margins = c(0.00, 0.00, 0.00, 0.00)) # Small inner margins
# 
# 
# print(class.map)
# 
# ind.map<-tm_shape(regions|> filter(Name==r))+
#   tm_borders()+
#   tm_shape(rs.b)+tm_symbols(fill="status")+
#   tm_facets_wrap(by="Indicator", nrow=2)
# 
# class.map.coral<-tm_shape(regions|> filter(Name==r))+
#   tm_borders()+
#   tm_shape(rc |> filter(Coral.cover=="No"))+tm_symbols(fill="red")+
#   # tm_title("B. Hard Coral Cover",just = "left",padding = 0,size = 1,
#   # position = tm_pos_in(pos.h = 'left', pos.v='top'))+
#   tm_layout(component.autoscale = TRUE,
#             frame = TRUE,
#             asp = 0, # Adjust aspect ratio to data
#             outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00)) # Small inner margins
# 
# class.map.rec<-tm_shape(regions|> filter(Name==r))+
#   tm_borders()+
#   tm_shape(rc |> filter(Recovery.performance=="No"))+tm_symbols(fill="red")+
#   tm_layout(component.autoscale = TRUE,
#             frame = TRUE,
#             asp = 0, # Adjust aspect ratio to data
#             outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00)) # Small inner margins
# 
# class.map.pro<-tm_shape(regions|> filter(Name==r))+
#   tm_borders()+
#   tm_shape(rc |> filter(Processes %in% c("AtLeastOne.No", "All.No")))+tm_symbols(fill="red")+
#   tm_layout(component.autoscale = TRUE,
#             frame = TRUE,
#             asp = 0, # Adjust aspect ratio to data
#             outer.margins = c(0.00, 0.00, 0.00, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00)) # Small inner margins
# 
# 
# # Option 1 ####
# ##using Cowplot to arrange panels 
# #Nesting plot grids
# # right_column<-plot_grid(tmap_grob(class.map.coral), tmap_grob(class.map.rec), tmap_grob(class.map.pro),
# #                         labels=c('B','C','D'), label_size = 12, ncol=1, nrow=3 )
# # Fig3<-plot_grid(tmap_grob(class.map), right_column, labels=c('A',''), label_size = 12, ncol=2, nrow = 1,
# #                 rel_widths = c(3,1))
# # Fig3
# # 
# # #Option 2 ####
# # critical.coral<-  tm_shape(gbr_feat |> filter(FEAT_NAME =="Reef"), bbox = my_bbox)+
# #   tm_fill(fill="lightgrey", lwd = NA)+
# #   tm_shape(rs.b |> filter(Indicator=="Coral.cover", status=="Below"), 
# #            bbox = my_bbox)+
# #   tm_symbols(fill="red", fill_alpha = 0.5)+
# #   tm_layout(component.autoscale = FALSE,
# #             frame = FALSE,
# #             asp = NA, # Adjust aspect ratio to data
# #             outer.margins = c(0.01, 0.00, 0.01, 0.00), # Reduce outer margins
# #             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# # 
# # critical.juv<-  tm_shape(gbr_feat |> filter(FEAT_NAME =="Reef"), bbox = my_bbox)+
# #   tm_fill(fill="lightgrey", lwd = NA)+
# #   tm_shape(rs.b |> filter(Indicator=="Juvenile.density", status=="Below"), 
# #            bbox = my_bbox)+
# #   tm_symbols(fill="red", fill_alpha = 0.5)+
# #   tm_layout(component.autoscale = FALSE,
# #             frame = FALSE,
# #             asp = NA, # Adjust aspect ratio to data
# #             outer.margins = c(0.01, 0.00, 0.01, 0.00), # Reduce outer margins
# #             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# # 
# # critical.ma<-  tm_shape(gbr_feat |> filter(FEAT_NAME =="Reef"), bbox = my_bbox)+
# #   tm_fill(fill="lightgrey", lwd = NA)+
# #   tm_shape(rs.b |> filter(Indicator=="Macroalgae", status=="Below"), 
# #            bbox = my_bbox)+
# #   tm_symbols(fill="red", fill_alpha = 0.5)+
# #   tm_layout(component.autoscale = FALSE,
# #             frame = FALSE,
# #             asp = NA, # Adjust aspect ratio to data
# #             outer.margins = c(0.01, 0.00, 0.01, 0.00), # Reduce outer margins
# #             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# # Fig3<-plot_grid(tmap_grob(class.map),NULL, tmap_grob(critical.coral),NULL, tmap_grob(critical.juv),NULL, tmap_grob(critical.ma), 
# #                 ncol=7, nrow=1,
# #                 rel_widths = c(1,-0.5,1,-0.5,1,-0.5, 1),
# #                 -0.5,
# #                 rel_heights = c(1,1,1,1,1,1,1),labels = c("A","","B","","C","","D"),
# #                 margin = c(0, 0.1, 0, 0.1)
# #                 )
# # Fig3
# 
# 
# #Option 3 ####
# offset<-1.3
# offset2<-1.3
# 
# class.map<-tm_shape(gbr_feat, bbox = my_bbox+c(0,0,offset+2.2*offset2, 0))+
#   tm_fill(fill="lightgrey")+
#   tm_shape(rc, bbox=my_bbox+c(0,0,2.2*offset, 0))+tm_symbols(fill="Class", fill_alpha = 0.7, 
#                                                              fill.scale=tm_scale(values=c("#00496FFF", "#0F85A0FF", "#EDD746FF", "#ED8B00FF", "#DD4124FF")),
#                                                              fill.legend =  tm_legend(
#                                                                title = "Classification",
#                                                                orientation ="portrait",
#                                                                width = 4,height = 10,
#                                                                title.size = 0.8,margins = c(0,0,0,0),
#                                                                title.padding = c(0.5,0,0.5,0),frame = FALSE,
#                                                                bg = FALSE,
#                                                                position = tm_pos_in("left", "bottom")
#                                                              ))+
#   tm_title("A. Reef Condition",fontface = "bold",size = 0.7,
#            position = tm_pos_in(pos.h = 0, pos.v=1.1))+
#   tm_layout(
#     component.autoscale = FALSE,
#     frame = FALSE,
#     asp = NA, # Adjust aspect ratio to data
#     outer.margins = c(0.01, 0.0, 0.1, 0.00), # Reduce outer margins
#     inner.margins = c(0.00, 0.00, 0.00, 0.00)) # Small inner margins
# 
# critical.coral<-  tm_shape(shift_location(gbr_feat |> filter(FEAT_NAME =="Reef"), offset)  , bbox = my_bbox+c(offset, 0, offset, 0))+
#   tm_fill(fill="lightgrey", lwd = NA)+
#   tm_shape(shift_location(rs.b |> filter(Indicator=="Coral.cover", status=="Below"),offset),
#            bbox = my_bbox+c(offset, 0, offset, 0))+
#   tm_title("B. Coral Cover",fontface = "bold",size = 0.7,
#            position = tm_pos_in(pos.h = 0.25, pos.v=1.1))+
#   tm_symbols(fill="red", fill_alpha = 0.5)+
#   tm_layout(component.autoscale = FALSE,
#             frame = FALSE,
#             asp = NA, # Adjust aspect ratio to data
#             outer.margins = c(0.01, 0.0, 0.1, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# 
# critical.juv<-  tm_shape(shift_location(gbr_feat |> filter(FEAT_NAME =="Reef"),offset+offset2), bbox = my_bbox+c(offset+offset2, 0, offset+offset2, 0))+
#   tm_fill(fill="lightgrey", lwd = NA)+
#   tm_shape(shift_location(rs.b |> filter(Indicator=="Juvenile.density", status=="Below"), offset+offset2), 
#            bbox = my_bbox+c(offset+offset2, 0, offset+offset2, 0))+
#   tm_symbols(fill="red", fill_alpha = 0.5)+
#   tm_title("C. Juvenile density",fontface = "bold",size = 0.7,
#            position = tm_pos_in(pos.h = 0.45, pos.v=1.1))+
#   tm_layout(component.autoscale = FALSE,
#             frame = FALSE,
#             asp = NA, # Adjust aspect ratio to data
#             outer.margins = c(0.01, 0.0, 0.1, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# 
# critical.ma<-  tm_shape(shift_location(gbr_feat |> filter(FEAT_NAME =="Reef"),offset+2*offset2), bbox = my_bbox+c(offset+offset2, 0, offset+2*offset2, 0))+
#   tm_fill(fill="lightgrey", lwd = NA)+
#   tm_shape(shift_location(rs.b |> filter(Indicator=="Macroalgae", status=="Below"),offset+2*offset2), 
#            bbox = my_bbox+c(offset+2*offset2, 0, offset+2*offset2, 0))+
#   tm_symbols(fill="red", fill_alpha = 0.5)+
#   tm_title("D. Macroalgae prevalence",fontface = "bold",size = 0.7,
#            position = tm_pos_in(pos.h = 0.70, pos.v=1.1))+
#   tm_layout(component.autoscale = FALSE,
#             frame = FALSE,
#             asp = NA, # Adjust aspect ratio to data
#             outer.margins = c(0.01, 0.0, 0.1, 0.00), # Reduce outer margins
#             inner.margins = c(0.00, 0.00, 0.00, 0.00))
# 
# Fig3<-class.map+
#   critical.coral+
#   critical.juv+
#   critical.ma
# Fig3
# 
# tmap_save(Fig3, filename = "scripts/case_studies/Fig3.png")
# 
