#auto text for regional summaries of Indicators

# summaries assume multiple depths at reefs are independent surveys

# general concept is to create sentences for each indicator that consider the primarily instances where distance from baseline scores deviate below 0.5
# within regions deviations below critical thresholds are also considered
# When combine these sentences provide a brief summary that expands on the overall classification of coral community condition.

##for development 
# setwd("C:\\Users/mgonzale/OneDrive - Australian Institute of Marine Science/projects/RIMReP/Habitat_Indicators/")
# input=list(region_select="GBRMPA.MA",
# report_year=2017,
# value_select="Cairns/Cooktown Management Area")
# load("outputs/scores.RData")
# i.df<-scores%>%
#   filter(Name==input$value_select)%>%
#   droplevels()

#' Format character vector into HTML bulleted list
#' 
#' @param char a character vector. Each element will be a bullet
#' @param ordered logical (T/F). If `TRUE`, return numbered list.
#' 
#' @keywords internal
format_html_list <- function(char, ordered = FALSE){
  
  seps <- c("<li>", "</li>")
  html_wrapper <-  if(ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")
  
  bullets <- paste0(seps[1], char, seps[2], collapse = "")
  
  html_list <- paste0(html_wrapper[1], bullets, html_wrapper[2])
  
  return(html_list)
}


sum.tx<-function(i.df, y){
  require(tidyverse)
  source("scripts/Misc/HighLevel_Classification.R")
  Indi<-i.df %>% 
    dplyr::select(Indicator) %>% 
    unique %>%
    mutate(Ind.desc=case_match(Indicator, "Coral.cover"~"hard coral cover",
                               "Macroalgae" ~"proportion of macroalgae in the algal cover",
                               "Juvenile"~"density of juvenile hard corals",
                               "Performance"~ "recovery rate of hard coral cover",
                               "Composition"~"coral community composition"))
  
  
  #************************************
  # Overall classification of condition
  #************************************
  
  Cl<-i.df%>%filter(Year==y, Reference=="Baseline")%>%Cond.Class()%>%pull(Class)

  Autotext.class="In <b>%s</b>, the overal condition reef habitats was classified as <b>%s</b>." 
  sent.class=
    sprintf(Autotext.class,
            y,
            Cl)
  
  #***********************
  # Hard coral cover
  #***********************
  
  
  
  data<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Baseline") %>% 
    droplevels() %>%
    arrange(Year) %>%
    mutate(Low=ifelse(Upper<0.5,1,0),
           runID.l=data.table::rleid(Low))
  
  data.hc.c<-i.df %>% 
    filter(Indicator=="Coral.cover" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  start.run<-as.character(data %>% 
                            filter(runID.l==max(runID.l)) %>%
                            summarise(runStart=first(Year)))
  
  low.years<-nrow(data %>% filter(runID.l==max(runID.l) & Low=='1'))
  prev.low<-data %>%
    filter(Year<y,Low==1)%>%
    summarise(prev.low=min(Year))%>%
    pull(prev.low)
    
  data.hc<-data %>%
    filter(Year==y)
  
  Low.hc<-data.hc$Upper<0.5
  
  Indi.desc<-as.character(data.hc %>% left_join(Indi) %>%
                            pull(Ind.desc))
  
  n.conseq<-as.character(data.hc.c %>%  pull(n.below))
  n.reefs<-as.character(data.hc %>%  pull(tn.reefs))
  n.no.conseq=as.character(as.numeric(n.reefs)-as.numeric(n.conseq))
  
  # Regional cover of hard corals  declined to below historical reference levels, and below consequence at at least one reef. 
  Autotext1="The %s declined to be below historical reference levels. At current levels, hard corals are unlikely to be supporting positive reef accretion at %s of the reefs monitored."
  
  # Regional cover of hard corals declined to below historical reference levels, but above consequence at all reefs.       
  Autotext2="The %s declined to be below historical reference levels. However, at current levels the cover hard corals are likely to be supporting positive reef accretion at all reefs monitored."
  
  # Regional cover of hard corals remain below historical reference levels, and below consequence at at least one reef. 
  Autotext3="The %s has remained below historical reference levels since %s. At %s levels hard corals are unlikely to be supporting positive reef accretion at %s of %s the reefs monitored."
  
  # Regional cover of hard corals remain below historical reference levels, but above consequence at all reefs.       
  Autotext4="The %s has remained below historical reference levels since %s. However, at %s levels the cover hard corals are likely to be supporting positive reef accretion at all reefs monitored."
  
  # Regional cover of hard corals at or above historical reference levels, at least one reef below consequence. 
  Autotext5="The %s is at or above historical reference levels. At current levels hard corals are likely to be supporting positive reef accretion at %s of %s the reefs monitored."
  
  # Regional cover of hard corals at or above historical reference levels, all reefs above consequence.       
  Autotext6="The %s is at or above historical reference levels. At current levels hard corals are likely to be supporting positive reef accretion at all reefs monitored"
  
  sent.hc=
    ifelse((isTRUE(Low.hc) & (as.numeric(n.conseq) >0) & low.years<=1),
           sprintf(Autotext1,
                   #y,
                   Indi.desc,
                   n.conseq),
           ifelse((isTRUE(Low.hc) & as.numeric(n.conseq)==0 & low.years==1),
                  sprintf(Autotext2,
                          #y,
                          Indi.desc),
                  ifelse((isTRUE(Low.hc) & as.numeric(n.conseq) >0 & low.years>1),
                         sprintf(Autotext3,
                                 Indi.desc,
                                 prev.low,
                                 y,
                                 n.conseq, n.reefs),
                         ifelse(isTRUE(Low.hc) & as.numeric(n.conseq==0) & low.years>1,
                                sprintf(Autotext4,
                                        Indi.desc,
                                        prev.low,
                                        y),
                                ifelse(isFALSE(Low.hc) & as.numeric(n.conseq >0),
                                       sprintf(Autotext5,
                                               #y,
                                               Indi.desc,
                                               n.no.conseq, n.reefs),
                                       if(isFALSE(Low.hc) & as.numeric(n.conseq==0)){
                                         sprintf(Autotext6,
                                                 #y,
                                                 Indi.desc)})
                         )
                  )
           )
    )
  
  #*********************************#
  # Performance Indicator  ##########
  #*********************************#
  
  data.p<-i.df %>% 
    filter(Indicator=="Performance" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.p.c<-i.df %>% 
    filter(Indicator=="Performance" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.pe<-data.p$Upper<0.5
  
  Indi.desc.pe<-as.character(Indi %>% 
                               filter(Indicator=="Performance") %>% 
                               pull(Ind.desc))
  
  n.reef.pe<-as.character(data.p %>%  pull(tn.reefs))
  n.reef.pe.low<-as.character(data.p.c %>%  pull(n.below))
  
  
  ##  Separate sentences for Regional and reef level summary to allow combination as appropriate
  
  # Regional Performance below expectations, HC at or above, all reefs performance is low 
  Autotext7="In contrast to scores for %s, the median %s across the region is lower than expected based on the rate typical for each reef's bioregion."
  
  # Regional Performance below expectations, HC low 
  Autotext8="Compounding low %s scores, the median %s is also lower than expected based on the rate typical for each reef's bioregion."
  
  # Regional Performance at expectations, HC at or above 
  Autotext9="Supporting scores for %s, the median %s was consistent with rates typical for each reef's bioregion."
  
  # Regional Performance at expectations, HC low
  Autotext10="Although %s is low, the median %s was consistent with rates typical for each reef's bioregion."
  
  # Reef level Critical Performance all low, and Baseline performance low
  Autotext11="In addition to the slow rate of recovery relative to historical values, the %s has declined at all reefs relative to their recent recovery trajectories."
  
  # Reef level Critical Performance some low and Baseline performance low
  Autotext11a="In addition to the slow rate of recovery relative to historical values, the %s has declined at %s reefs relative to their recent recovery trajectories."
  
  # Reef level Critical Performance some low and Baseline performance ok  
  Autotext12="Despite the rate of recovery remaining similar to historical values across the region, the %s has declined at %s reefs relative to their recent recovery trajectories."
  
  # Reef level Critical Performance all matching or exceeding
  Autotext13="Currently, the %s remains similar to recently observed rates at all reefs monitored."
  
  
  sent.pe=
    ifelse(isTRUE(Low.pe) & isFALSE(Low.hc),
           sprintf(Autotext7,
                   Indi.desc,
                   Indi.desc.pe),
           ifelse(isTRUE(Low.pe) & isTRUE(Low.hc),
                  sprintf(Autotext8,
                          Indi.desc,
                          Indi.desc.pe),
                  ifelse(isFALSE(Low.pe) & isFALSE(Low.hc),
                         sprintf(Autotext9,
                                 Indi.desc,
                                 Indi.desc.pe),
                         if(isFALSE(Low.pe) & isTRUE(Low.hc)){
                           sprintf(Autotext10,
                                   Indi.desc,
                                   Indi.desc.pe)})
           )
    )
  
  sent.pe.reefs=
    ifelse(n.reef.pe==n.reef.pe.low & isTRUE(Low.pe),
           sprintf(Autotext11,
                   Indi.desc.pe),
           ifelse(n.reef.pe.low>0 & as.numeric(n.reef.pe.low)<as.numeric(n.reef.pe) & isTRUE(Low.pe),
                  sprintf(Autotext11a,
                          Indi.desc.pe,
                          n.reef.pe.low),
                  ifelse(n.reef.pe.low>0 & as.numeric(n.reef.pe.low)<as.numeric(n.reef.pe) & isFALSE(Low.pe),
                         sprintf(Autotext12,
                                 Indi.desc.pe,
                                 n.reef.pe.low),
                         if(n.reef.pe.low==0){
                           sprintf(Autotext13,
                                   Indi.desc.pe)})
           )
    )
  
  #********************##
  # MA and Juveniles ####
  #*******************###    
  #Ma data
  data.m<-i.df %>% 
    filter(Indicator=="Macroalgae" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.m.c<-i.df %>% 
    filter(Indicator=="Macroalgae" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.m<-data.m$Upper<0.5
  
  Indi.desc.m<-as.character(Indi %>% 
                              filter(Indicator=="Macroalgae") %>% 
                              pull(Ind.desc))
  
  n.reef.m<-as.character(data.m %>%  pull(tn.reefs))
  n.reef.m.low<-as.character(data.m.c %>%  pull(n.below))
  
  #JUv data
  data.j<-i.df%>% 
    filter(Indicator=="Juvenile" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.j.c<-i.df %>% 
    filter(Indicator=="Juvenile" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.j<-data.j$Upper<0.5
  
  Indi.desc.j<-as.character(Indi %>% 
                              filter(Indicator=="Juvenile") %>% 
                              pull(Ind.desc))
  
  n.reef.j<-as.character(data.j %>%  pull(tn.reefs))
  n.reef.j.low<-as.character(data.j.c %>%  pull(n.below))
  
  # Both MA and Juv Low, Performance and HC ok -Watch/Warning1
  Autotext14="Despite the observed cover and performance of hard coral communities, low scores of %s and %s are of concern." 
  
  # Both MA and Juv Low, Performance low, HC ok -Warning1/Warning2
  # Both MA and Juv Low, Performance ok, HC low -Warning1/Warning2
  # Both MA and Juv Low, Performance and HC low -Critical
  Autotext15="Further limiting the assessemnt of coral communities are that %s are below, and %s are above historical reference levels." 
  
  # Both MA and Juv ok, Performance and HC low -Warning2/Critical
  Autotext16="In contrast, scores for %s and %s suggest potential for recovery."
  
  # Both MA and Juv ok, Performance low, HC ok -Watch/Warning1
  Autotext17="Although %s is low, scores for %s and %s suggest ongoing recovery potential."
  
  # Both MA and Juv ok, Performance ok, HC low -Watch/Warning1
  Autotext18="Although %s is low, scores for %s and %s, in combination with the %s suggest ongoing recovery potential."
  
  # Both MA and Juv ok, Performance and HC ok -Good/Watch
  Autotext19="Scores for %s and %s add support to the ongoning resilience of coral communities demonstrated by the Coral cover and Performance Indicators."
  
  #One of MA or Juv ok, Performance and HC ok -Watch
  Autotext20="While %s does not deviate from historical reference levels, this is not the case for %s."
  
  # One of Ma or Juv ok, Performance low, HC ok -Warning1
  Autotext21="While %s does not deviate from historical reference levels, this is not the case for %s and adds to the concern for maintaininece of recovery potential."
  
  # One of Ma or Juv ok, Performance ok, HC low -Warning1
  Autotext22="While %s does not deviate from historical reference levels, this is not the case for %s and raises some concern for the maintaininece of recovery processes."
  
  # One of Ma or Juv ok, Performance and HC low -Critical
  Autotext23="While %s does not deviate from historical reference levels, this is not the case for %s and this raises further concern for the maintaininece of recovery processes."
  
  
  sent.jma=
    
    ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & isTRUE(Low.j) & isTRUE(Low.m),
           sprintf(Autotext14,
                   Indi.desc.j,
                   Indi.desc.m),
           
           ifelse(isFALSE(Low.pe) & isTRUE(Low.hc) & isTRUE(Low.j) & isTRUE(Low.m),
                  sprintf(Autotext15,
                          Indi.desc.j,
                          Indi.desc.m),
                  
                  ifelse(isTRUE(Low.pe) & isFALSE(Low.hc) & isTRUE(Low.j) & isTRUE(Low.m),
                         sprintf(Autotext15,
                                 Indi.desc.j,
                                 Indi.desc.m),
                         
                         ifelse(isTRUE(Low.pe) & isTRUE(Low.hc) & isTRUE(Low.j) & isTRUE(Low.m),
                                sprintf(Autotext15,
                                        Indi.desc.j,
                                        Indi.desc.m),
                                
                                ifelse(isTRUE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.j) & isFALSE(Low.m),
                                       sprintf(Autotext16,
                                               Indi.desc.j,
                                               Indi.desc.m),
                                       
                                       ifelse(isTRUE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.j) & isFALSE(Low.m),
                                              sprintf(Autotext17,
                                                      Indi.desc.pe,
                                                      Indi.desc.j,
                                                      Indi.desc.m),
                                              
                                              ifelse(isFALSE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.j) & isFALSE(Low.m),
                                                     sprintf(Autotext18,
                                                             Indi.desc,
                                                             Indi.desc.j,
                                                             Indi.desc.m,
                                                             Indi.desc.pe),
                                                     
                                                     ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.j) & isFALSE(Low.m),
                                                            sprintf(Autotext19,
                                                                    Indi.desc.j,
                                                                    Indi.desc.m),
                                                            
                                                            ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.j) & isTRUE(Low.m),
                                                                   sprintf(Autotext20,
                                                                           Indi.desc.j,
                                                                           Indi.desc.m),
                                                                   
                                                                   ifelse(isTRUE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.j) & isTRUE(Low.m),
                                                                          sprintf(Autotext21,
                                                                                  Indi.desc.j,
                                                                                  Indi.desc.m),
                                                                          
                                                                          ifelse(isFALSE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.j) & isTRUE(Low.m),
                                                                                 sprintf(Autotext22,
                                                                                         Indi.desc.j,
                                                                                         Indi.desc.m),
                                                                                 
                                                                                 ifelse(isTRUE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.j) & isTRUE(Low.m),
                                                                                        sprintf(Autotext23,
                                                                                                Indi.desc.j,
                                                                                                Indi.desc.m),
                                                                                        
                                                                                        ifelse(isFALSE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.m) & isTRUE(Low.j),
                                                                                               sprintf(Autotext20,
                                                                                                       Indi.desc.m,
                                                                                                       Indi.desc.j),
                                                                                               
                                                                                               ifelse(isTRUE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.m) & isTRUE(Low.j),
                                                                                                      sprintf(Autotext21,
                                                                                                              Indi.desc.m,
                                                                                                              Indi.desc.j),
                                                                                                      
                                                                                                      ifelse(isFALSE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.m) & isTRUE(Low.j),
                                                                                                             sprintf(Autotext22,
                                                                                                                     Indi.desc.m,
                                                                                                                     Indi.desc.j),
                                                                                                             
                                                                                                             if(isTRUE(Low.pe) & isTRUE(Low.hc) & isFALSE(Low.m) & isTRUE(Low.j)){
                                                                                                               sprintf(Autotext23,
                                                                                                                       Indi.desc.m,
                                                                                                                       Indi.desc.j)})
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
  
  
  # Juvenile consequence at least 1 low
  Autotext24="The density of <i>Acropora</i> juveniles at %s reefs are low and likley to be limiting recovery."
  
  #Juvenile consequence all low
  Autotext25="Current densities of <i>Acropora</i> juveniles are likely to be limiting recovery at all reefs."
  
  #Juvenile consequence ok all
  Autotext26="Current densities of <i>Acropora</i> juveniles are sufficient to promote recovery at all reefs."
  
  # Macroalgae consequence at least 1 low, MA regionally low
  Autotext27="In addition to having increased relative to reference levels, macroalgae are sufficiently common to limit coral community resilience at %s reefs."
  
  # Macroalgae consequence at least 1 low, MA regionally ok
  Autotext28="However, although on average macroalgae do not exceeded reference levels, they are at levels likely to limit coral community recovery at %s reefs."
  
  # Macroalgae consequence all low, MA regionally low
  Autotext29="In addition to having increased beyond reference levels, macroalgae are at levels likley to limit coral community resilience at all reefs monitored."
  
  # Macroalgae all all MA regionally ok
  Autotext30="Although macroalgae do not exceeded reference levels, they are at levels likely to limit coral community recovery at all reefs monitored."
  
  # Macroalgae all MA regionally ok and not above critical
  Autotext31="Macroalage are below levels likely to limit coral community recovery at all reefs monitored."
  
  
  sent.j.reefs=
    ifelse(as.numeric(n.reef.j.low)>0 & as.numeric(n.reef.j.low)<as.numeric(n.reef.j),
           sprintf(Autotext24,
                   n.reef.j.low),
           ifelse(n.reef.j==n.reef.j.low,
                  Autotext25,
                  if(as.numeric(n.reef.j.low)==0){
                    Autotext26})
    )
  
  sent.m.reefs=
    ifelse(as.numeric(n.reef.m.low)>0 & as.numeric(n.reef.m.low)<as.numeric(n.reef.m) & isTRUE(Low.m),
           sprintf(Autotext27,
                   n.reef.m.low),
           ifelse(as.numeric(n.reef.m.low)>0 & as.numeric(n.reef.m.low)<as.numeric(n.reef.m) & isFALSE(Low.m),
                  sprintf(Autotext28,
                          n.reef.m.low),
                  ifelse(n.reef.m==n.reef.m.low & isTRUE(Low.m),
                         Autotext29,
                         ifelse(n.reef.m==n.reef.m.low & isFALSE(Low.m),
                                Autotext30,
                                if(n.reef.m.low ==0 & isFALSE(Low.m)){
                                  Autotext31}))
           )
           
    )
  
  #************#####
  # Composition#####
  #************#####
  #comp data
  data.co<-i.df %>% 
    filter(Indicator=="Composition" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.co.c<-i.df %>% 
    filter(Indicator=="Composition" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.co<-data.co$Upper<0.5
  
  Indi.desc.co<-as.character(Indi %>% 
                               filter(Indicator=="Composition") %>% 
                               pull(Ind.desc))
  
  n.reef.co<-as.character(data.co %>%  pull(tn.reefs))
  n.reef.co.low<-as.character(data.co.c %>%  pull(n.below))
  
  # comp low - hc ok.
  Autotext32="Despite the cover of hard corals remaining at or above reference levels, there is evidence that the composition of communities have changed. Such changes are evident at %s reefs."
  
  # comp ok - hc ok, no reef level change
  Autotext33="In addition to the cover of hard corals remaining within reference levels there is no evidence for region-wide change in the taxnomic composition of coral communities."
  
  # comp ok - hc ok, some reef level change
  Autotext34="In addition to the cover of hard corals remaining within reference levels there is no evidence for region-wide change in the taxnomic composition of coral communities. However, community composition shifts from that historically observed are evident at %s reefs."
  
  # comp low - hc low.
  Autotext35="In addition to low cover of hard corals, there is evidence that the composition of communities have changed within the region. Such changes are evident at %s reefs."
  
  # comp ok - hc low
  Autotext36="Although the cover of hard corals is low, there is no evidence for region-wide change in the taxnomic composition of coral communities."
  
  
  sent.co<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.hc),
           sprintf(Autotext32,
                   n.reef.co.low),
           ifelse(isFALSE(Low.co) & isFALSE(Low.hc) & n.reef.co.low==0,
                  Autotext33,
                  ifelse(isFALSE(Low.co) & isFALSE(Low.hc) & as.numeric(n.reef.co.low)>0,
                         sprintf(Autotext34,
                                 n.reef.co.low),
                         ifelse(isTRUE(Low.co) & isTRUE(Low.hc),
                                sprintf(Autotext35,
                                        n.reef.co.low),Autotext36)
                  )
           )
    )
  

  
  
  #********************************************************************************************
  # Sampling summary lead in for context and to reduce repetition within indicator sentences####
  #********************************************************************************************
  not.j=as.numeric(n.reefs)-as.numeric(n.reef.j)
  not.comp=as.numeric(n.reefs)-as.numeric(n.reef.co)
  not.pe=as.numeric(n.reefs)-as.numeric(n.reef.pe)
  not.ma=as.numeric(n.reefs)-as.numeric(n.reef.m)
  
  sample.autotext="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, all indicators were assessed for each those locations."  
  sample.autotext.n1="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, the scores for %s was not available in %s of those locations."
  sample.autotext.n2="This synopsis was derived from indicators estimated at %s locations, across reefs and survey depths. In %s, the scores for %s and %s were not avaialble in %s and %s of these locations, respectively."
  sample.autotext.n3="This synopsis reflects the indicator scores observed at %s reef and depth combinations. In %s,the scores for %s, %s and %s were not avaliable in %s, %s and %s of these locations, respectively."
  
  sent.samp=
    ifelse(max(not.comp,not.j,not.pe,not.ma)==0,
           sprintf(sample.autotext,
                   n.reefs,
                   y),
           ifelse(not.j>0 & max(not.comp,not.pe,not.ma)==0,
                  sprintf(sample.autotext.n1,
                          n.reefs,
                          y,
                          Indi.desc.j,
                          not.j),
                  ifelse(not.comp>0 & max(not.j,not.pe,not.ma)==0,
                         sprintf(sample.autotext.n1,
                                 n.reefs,
                                 y,
                                 Indi.desc.co,
                                 not.comp),
                         ifelse(not.pe>0 & max(not.j,not.comp,not.ma)==0,
                                sprintf(sample.autotext.n1,
                                        n.reefs,
                                        y,
                                        Indi.desc.pe,
                                        not.pe),
                                ifelse(not.ma>0 & max(not.j,not.pe,not.comp)==0,
                                       sprintf(sample.autotext.n1,
                                               n.reefs,
                                               y,
                                               Indi.desc.ma,
                                               not.ma),
                                       ifelse(not.j>0 & not.comp>0 & max(not.ma,not.pe)==0,
                                              sprintf(sample.autotext.n2,
                                                      n.reefs,
                                                      y,
                                                      Indi.desc.j,
                                                      Indi.desc.co,
                                                      not.j,
                                                      not.comp),
                                              ifelse(not.j>0 & not.ma>0 & max(not.comp,not.pe)==0,
                                                     sprintf(sample.autotext.n2,
                                                             n.reefs,
                                                             y,
                                                             Indi.desc.j,
                                                             Indi.desc.ma,
                                                             not.j,
                                                             not,ma),
                                                     ifelse(not.j>0 & not.pe>0 & max(not.comp,not.ma)==0,
                                                            sprintf(sample.autotext.n2,
                                                                    n.reefs,
                                                                    y,
                                                                    Indi.desc.j,
                                                                    Indi.desc.pe,
                                                                    not.j,
                                                                    not.pe),
                                                            ifelse(not.pe>0 & not.ma>0 & max(not.comp,not.j)==0,
                                                                   sprintf(sample.autotext.n2,
                                                                           n.reefs,
                                                                           y,
                                                                           Indi.desc.pe,
                                                                           Indi.desc.ma,
                                                                           not.pe,
                                                                           not,ma),
                                                                   ifelse(not.pe>0 & not.comp>0 & max(not.ma,not.j)==0,
                                                                          sprintf(sample.autotext.n2,
                                                                                  n.reefs,
                                                                                  y,
                                                                                  Indi.desc.pe,
                                                                                  Indi.desc.co,
                                                                                  not.pe,
                                                                                  not.comp),
                                                                          ifelse(not.ma>0 & not.comp>0 & max(not.pe,not.j)==0,
                                                                                 sprintf(sample.autotext.n2,
                                                                                         n.reefs,
                                                                                         y,
                                                                                         Indi.desc.ma,
                                                                                         Indi.desc.co,
                                                                                         not.ma,
                                                                                         not.comp),
                                                                                 ifelse(not.ma>0 & not.comp>0 & not.j>0 & not.pe==0,
                                                                                        sprintf(sample.autotext.n3,
                                                                                                n.reefs,
                                                                                                y,
                                                                                                Indi.desc.ma,
                                                                                                Indi.desc.co,
                                                                                                Indi.desc.j,
                                                                                                not.ma,
                                                                                                not.comp,
                                                                                                not.j),
                                                                                        ifelse(not.ma>0 & not.pe>0 & not.j>0 & not.comp==0,
                                                                                               sprintf(sample.autotext.n3,
                                                                                                       n.reefs,
                                                                                                       y,
                                                                                                       Indi.desc.ma,
                                                                                                       Indi.desc.pe,
                                                                                                       Indi.desc.j,
                                                                                                       not.ma,
                                                                                                       not.pe,
                                                                                                       not.j),
                                                                                               ifelse(not.ma>0 & not.comp>0 & not.pe>0 & not.j==0,
                                                                                                      sprintf(sample.autotext.n3,
                                                                                                              n.reefs,
                                                                                                              y,
                                                                                                              Indi.desc.ma,
                                                                                                              Indi.desc.co,
                                                                                                              Indi.desc.pe,
                                                                                                              not.ma,
                                                                                                              not.comp,
                                                                                                              not.pe),
                                                                                                      ifelse(not.pe>0 & not.comp>0 & not.j>0 & not.ma==0,
                                                                                                             sprintf(sample.autotext.n3,
                                                                                                                     n.reefs,
                                                                                                                     y,
                                                                                                                     Indi.desc.pe,
                                                                                                                     Indi.desc.co,
                                                                                                                     Indi.desc.j,
                                                                                                                     not.pe,
                                                                                                                     not.comp,
                                                                                                                     not.j)
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
                  )
           )
    )
  
  
  #combine sentences####   
  caption<-format_html_list(c(sent.class,sent.hc,sent.pe,
                              sent.pe.reefs,sent.j.reefs,sent.m.reefs, sent.co),
                            ordered = F)
  note<-paste0("<i>Disclaimer:</i>", sent.samp)
  
  
  return(list(caption, note))
  
}

