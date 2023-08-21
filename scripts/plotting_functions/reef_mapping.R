map.reefs<-function(sf.frame, reef, b, y){
  
  qpal <- colorFactor(palette = c("#FFFFFF","#EF3A14" ,"#F3841C", "#EFC214",'#8ace7e', "#309143"),
                      levels = c("Insuficient data","Critical", "Warning II","Warning I", "Watch", "Good"),
                      reverse = F, 
                      # sf.frame$Class,
                      # domain=c("Critical", "WarningII","WarningI", "Watch", "Good"),
                      ordered=T,
                      na.color = "#808080")
  
  lval=factor(c("Insuficient data","Critical", "Warning II","Warning I", "Watch", "Good"),
              levels=c("Insuficient data","Critical", "Warning II","Warning I", "Watch", "Good"))
  
  Name.Labels =function(Name){
    sprintf(
    "<strong>%s</strong><br/>",
    sf.frame$Name
  ) %>% lapply(htmltools::HTML)
  }
  
  factop <- function(x) {
    ifelse(is.na(x), 0.1, 0.6)
  }
  
  if(dim(sf.frame%>%filter(Year==y,
                           Reference=="Baseline"
                           ))[1]==0){ #Display "no data" box if the reef/region are not surveyed
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", group="Dark")%>%
      addProviderTiles("Esri.WorldImagery", group="Satellite")%>%
      addLayersControl(baseGroups = c("Dark","Satellite"))
    
    
  }else{
    
    sf.frame<-sf.frame%>%filter(Year==y,
                                Reference=="Baseline")%>% 
                                # Depth %in% c(NA,"deep slope"))%>%
      select(-c(Latitude,Longitude))%>%
      distinct()%>%
      ungroup()%>%Cond.Class()%>%
      unique()%>%
      mutate(Class=factor(Class,levels=c("Insuficient data","Critical", "Warning II","Warning I", "Watch", "Good")))
    
    sf.frame<-reef%>%left_join(sf.frame,by=c("Name","Depth"))

      # 
    
    leaflet(sf.frame) %>%
      # addMarkers(label=~Name)%>%
      addProviderTiles("Esri.WorldImagery", group="Satellite")%>%      
      addProviderTiles("CartoDB.DarkMatter", group="Dark")%>%
      addCircleMarkers(data=sf.frame, label = ~Name, fillColor =  ~qpal(Class), 
                       # popup = ~c(Name,Class),
                       popup = ~paste(Name.Labels(Name),
                                      "<br>Coral Cover above reference:",Coral.cover,
                                      "<br>Recovery above reference:",Recovery.performance,
                                      "<br>Processes above reference:", Processes),
                       fillOpacity = ~factop(Class),
                       stroke=F,
                       fill=T)%>%
      # addLegend("bottomright", pal = qpal, values = sf.frame$Score[!is.na(sf.frame$Score)],
      addLegend("bottomright", pal = qpal, values = lval,
                title = "Overall Condition",
                opacity = 1)%>%
      # Use addLayersControl to allow users to toggle between basemaps
      addLayersControl(baseGroups = c("Satellite","Dark"))%>%# groupOptions("Satellite", zoomLevels = 8:15)
      addPolygons(data=b%>%
                    st_cast(.,"POLYGON")%>%
                    st_transform(4326),
                  fill=F,
                  weight=2,
                  opacity=0.9,
                  color="white",
                  dashArray = "3"
      )
    
    
  }
}


## Map indicators
map.ind<-function(df, I,c, r){
  
  qpal <- colorFactor(palette = c("#EF3A14" , "#EFC214", "#56B703"),
                      reverse = F, x,
                      levels = c( "Below","Within", "Above"),
                      ordered=T,
                      na.color = "#808080")
  
  
  
  Name.Labels <- sprintf(
    "<strong>%s</strong><br/>",
    sf.frame$Name
  ) %>% lapply(htmltools::HTML)
  
  factop <- function(x) {
    ifelse(is.na(x), 0.1, 0.6)
  }
  
  if(dim(sf.frame)[1]==0){ #Display "no data" box if the reef/region are not surveyed
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter", group="Dark")%>%
      addProviderTiles("Esri.WorldImagery", group="Satellite")%>%
      addLayersControl(baseGroups = c("Dark","Satellite"))
    
    
  }else{
    
    # i.df.r<-i.df.r%>%filter(Year==input$report_year,
    #                         Reference=="Baseline", is.na(Depth))%>%
    #   mutate(Classification=case_when(
    #     Lower > 0.5 ~ "Above",
    #     Upper < 0.5 ~ "Below",
    #     is.na(Lower) ~ NA,
    #     .default="Within"))%>%
    #   select(c(Level,Name,Year,Indicator, Classification))%>%
    # spread(key=Indicator,value=Classification)
    
    
    leaflet(df) %>%
      # addMarkers(label=~Name)%>%
      addProviderTiles("CartoDB.DarkMatter", group="Dark")%>%
      addProviderTiles("Esri.WorldImagery", group="Satellite")%>%
      # addEsriTiledMapLayer(
      #   url = "https://tiles.arcgis.com/tiles/ll1QQ2mI4WMXIXdm/arcgis/rest/services/SSR_Sentinel_2018/MapServer",
      #   group="Satellite")%>%
      addCircleMarkers(data=sf.frame, label = ~Name, fillColor =  ~qpal(Class), 
                       # popup = ~c(Name,Class),
                       popup = ~paste(Name.Labels,
                                      "<br>Coral Cover above reference:",Coral.cover,
                                      "<br>Recovery above reference:",Recovery.performance,
                                      "<br>Processes above reference:", Processes),
                       fillOpacity = ~factop(Class),
                       stroke=F,
                       fill=T)%>%
      # addLegend("bottomright", pal = qpal, values = sf.frame$Score[!is.na(sf.frame$Score)],
      addLegend("bottomright", pal = qpal, values = levels(sf.frame$Class),
                title = "Overall Condition",
                opacity = 1)%>%
      # Use addLayersControl to allow users to toggle between basemaps
      addLayersControl(baseGroups = c("Dark","Satellite"))%>%
      addPolygons(data=this.region%>%
                    st_cast(.,"POLYGON")%>%
                    st_transform(4326),
                  fill=F,
                  weight=2,
                  opacity=0.9,
                  color="white",
                  dashArray = "3"
      )
    # groupOptions("Satellite", zoomLevels = 8:15)
    
  }
}

# leaflet()%>%
# addTiles()%>%
