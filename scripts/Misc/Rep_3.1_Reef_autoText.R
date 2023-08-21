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


sum.reef.tx<-function(i.df, y){
  require(tidyverse)
  source("scripts/Misc/HighLevel_Classification.R")
  Indi<-i.df %>% 
    dplyr::select(Indicator) %>% 
    unique %>%
    mutate(Ind.desc=case_match(Indicator, "Coral.cover"~"hard coral cover",
                               "Macroalgae" ~"proportion of macroalgae in the algal cover",
                               "Juvenile.density"~"density of Juvenile.density hard corals",
                               "Recovery.performance"~ "recovery rate of hard coral cover",
                               "Community.composition"~"coral community composition"))
  
  
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
  # Hard coral cover #####
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
  if(low.years>0){
    first.low<-data %>%
    filter(Year<y,Low==1)%>%
    summarise(first.low=min(Year))%>%
    pull(first.low)
  }
  
  data.hc<-data %>%
    filter(Year==y)
  
  Low.hc<-data.hc$Upper<0.5
  Low.hc.c<-data.hc.c$Upper<0.5
  
  Indi.desc<-as.character(data.hc %>% left_join(Indi) %>%
                            pull(Ind.desc))
  # Reef cover of hard corals  declined to below historical reference levels, and below consequence
  Autotext1="The %s declined to be below historical reference levels. At current levels hard corals are unlikely to be supporting positive reef accretion."
  
  # Reef cover of hard corals declined to below historical reference levels, but above consequence.       
  Autotext2="The %s declined to be below historical reference levels, however remains at levels sufficient for the maintainance of positive reef accretion."
  
  # Reef cover of hard corals remain below historical reference levels, and below consequence. 
  Autotext3="The %s has remained below historical reference levels since %s. At current levels, hard corals are unlikely to be supporting positive reef accretion."
  
  # Reef cover of hard corals remain below historical reference levels, but above consequence at all reefs.       
  Autotext4="The %s has remained below historical reference levels since %s, however at current levels is sufficient to support positive reef accretion."
  
  # Reef cover of hard corals at or above historical reference levels, but below consequence. 
  Autotext5="The %s is at or above historical reference levels but lower than that required for positive reef accretion."
  
  # Reef cover of hard corals at or above historical reference levels, and above consequence.       
  Autotext6="The %s is at or above historical reference levels and sufficent to maintain positive reef accretion."
  
  sent.hc=
    ifelse(isTRUE(Low.hc) & low.years==1 & isTRUE(Low.hc.c),
           sprintf(Autotext1,
                   Indi.desc),
           ifelse(isTRUE(Low.hc) & low.years==1 & isFALSE(Low.hc.c),
                  sprintf(Autotext2,
                          Indi.desc),
                  ifelse(isTRUE(Low.hc) & low.years>1 & isTRUE(Low.hc.c),
                         sprintf(Autotext3,
                                 Indi.desc,
                                 first.low),
                         ifelse(isTRUE(Low.hc) & low.years>1 & isFALSE(Low.hc.c),
                                sprintf(Autotext4,
                                        Indi.desc,
                                        first.low),
                                ifelse(isFALSE(Low.hc) & isTRUE(Low.hc.c),
                                       sprintf(Autotext5,
                                               Indi.desc),
                                       if(isFALSE(Low.hc) & isFALSE(Low.hc.c)){
                                         sprintf(Autotext6,
                                                 Indi.desc)})
                         )
                  )
           )
    )
  
  #*********************************#
  # Recovery.performance Indicator  ##########
  #*********************************#
  
  data.p<-i.df %>% 
    filter(Indicator=="Recovery.performance" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.p.c<-i.df %>% 
    filter(Indicator=="Recovery.performance" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.pe<-data.p$Upper<0.5
  Low.pe.c<-data.p.c$Upper<0.5
  
  Indi.desc.pe<-as.character(Indi %>% 
                               filter(Indicator=="Recovery.performance") %>% 
                               pull(Ind.desc))
  ##  Separate sentences for Regional and reef level summary to allow combination as appropriate
  
  # Reef Recovery.performance baseline low, HC ok
  Autotext7="In contrast to scores for %s, the %s is lower than expected for reefs in this bioregion."
  
  # Reef Recovery.performance below expectations, HC low 
  Autotext8="Compounding low %s scores, the %s is also lower than expected for reefs in this bioregion."
  
  # Reef Recovery.performance at expectations, HC at or above 
  Autotext9="Supporting scores for %s, the %s was consistent with that expected for reefs in this bioregion."
  
  # Reef Recovery.performance at expectations, HC low
  Autotext10="Although %s is low, the %s was consistent with that expected for reefs in this bioregion."
  
  # Reef level Critical Recovery.performance low, and Baseline Recovery.performance low
  Autotext11="In addition to the slow rate of recovery relative to historical values, the %s has declined relative the reef's recent recovery trajectory."
  
  # Reef level Critical Recovery.performance low and Baseline Recovery.performance ok  
  Autotext12="Despite the rate of recovery remaining similar to historical values for this bioregion, the %s has declined relative the reef's recent recovery trajectory."
  
  # Reef level Critical Recovery.performance ok
  Autotext13="The current %s remains similar the reef's recent recovery trajectory."
  
  Autotext13a="The recent recovery trajectory for this reef could not be estimated due to lack of sufficient number of observations"
  Autotext13b="The expected recovery for reefs in this bioregion could not be estimated due to lack of sufficient number of observations"
  
  
  # Recovery not estimated or other adjustment made?   
  Autotext.no.Recovery.performance.estimate=""     ##########---------Yet to be decided 
  
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
                                   Indi.desc.pe)}else{Autotext13b})
           )
    )
  
  sent.pe.c=
    ifelse(isTRUE(Low.pe.c) & isTRUE(Low.pe),
           sprintf(Autotext11,
                   Indi.desc.pe),
           ifelse(isTRUE(Low.pe.c) & isFALSE(Low.pe),
                  sprintf(Autotext12,
                          Indi.desc.pe),
                  if(isFALSE(Low.pe.c)){
                    sprintf(Autotext13,
                            Indi.desc.pe)} else{Autotext13a})
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
  Low.m.c<-data.m.c$Upper<0.5
  
  Indi.desc.m<-as.character(Indi %>% 
                              filter(Indicator=="Macroalgae") %>% 
                              pull(Ind.desc))
  

  #JUv data
  data.j<-i.df%>% 
    filter(Indicator=="Juvenile.density" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  
  data.j.c<-i.df %>% 
    filter(Indicator=="Juvenile.density" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.j<-data.j$Upper<0.5
  Low.j.c<-data.j.c$Upper<0.5
  
  Indi.desc.j<-as.character(Indi %>% 
                              filter(Indicator=="Juvenile.density") %>% 
                              pull(Ind.desc))
  
  # Both MA and Juv Low, Recovery.performance and HC ok -Watch/Warning1
  Autotext14="Despite the observed cover and Recovery.performance of hard coral communities, low scores of %s and %s are of concern." 
  
  # Both MA and Juv Low, Recovery.performance low, HC ok -Warning1/Warning2
  # Both MA and Juv Low, Recovery.performance ok, HC low -Warning1/Warning2
  # Both MA and Juv Low, Recovery.performance and HC low -Critical
  Autotext15="Further limiting the assessemnt of coral communities are that %s is below, and %s above historical reference levels." 
  
  # Both MA and Juv ok, Recovery.performance and HC low -Warning2/Critical
  Autotext16="In contrast to the cover and Recovery.performance of hard coral communities, that %s is below and %s are above historical reference levels is a positive sign."
  
  # Both MA and Juv ok, Recovery.performance low, HC ok -Watch/Warning1
  Autotext17="Although %s is low, that %s is below and %s are above historical reference levels is a positive sign."
  
  # Both MA and Juv ok, Recovery.performance ok, HC low -Watch/Warning1
  Autotext18="Although %s is low, neither %s, %s or %s, are in worse condition relative to historical reference levels."
  
  # Both MA and Juv ok, Recovery.performance and HC ok -Good/Watch
  Autotext19="Scores for %s and %s add support to the ongoning resilience of coral communities demonstrated by the Coral cover and Recovery.performance Indicators."
  
  #One of MA or Juv ok, Recovery.performance and HC ok -Watch
  Autotext20="While %s does not deviate from historical reference levels, this is not the case for %s."
  
  # One of Ma or Juv ok, Recovery.performance low, HC ok -Warning1
  Autotext21="While %s does not deviate from historical reference levels, this is not the case for %s and adds to the concern raised by the low Recovery.performance score."
  
  # One of Ma or Juv ok, Recovery.performance ok, HC low -Warning1
  Autotext22="While %s does not deviate from historical reference levels, this is not the case for %s and raises some concern for future recovery."
  
  # One of Ma or Juv ok, Recovery.performance and HC low -Critical
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
                                               Indi.desc.m,
                                               Indi.desc.j),
                                       
                                       ifelse(isTRUE(Low.pe) & isFALSE(Low.hc) & isFALSE(Low.j) & isFALSE(Low.m),
                                              sprintf(Autotext17,
                                                      Indi.desc.pe,
                                                      Indi.desc.m,
                                                      Indi.desc.j),
                                              
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
  
  
  
  # Juvenile.density consequence low, baseline ok
  Autotext24="Despite the score for %s, the density of <i>Acropora</i> juveniles is low and this may limit the future rate of coral cover recovery."
  
  #Juvenile.density consequence low, baseline low
  Autotext25="Compounding low scores for %s is the low density of <i>Acropora</i> juveniles, a group important for maintaining high rates of coral cover recovery."
  
  #Juvenile.density consequence ok, baseline ok
  Autotext26="In addition to the scores for %s, the density of <i>Acropora</i> juveniles are sufficient to promote coral cover recovery."
  
  #Juvenile.density consequence ok, baseline low
  Autotext26="Although the %s is below historical reference levels the density of <i>Acropora</i> juveniles are sufficient to promote coral cover recovery."
  
  # Macroalgae consequence low, baseline low
  Autotext27="In addition to having increased relative to reference levels, %s are at levels likely to limit coral community recovery potential."
  
  # Macroalgae consequence  low, baseline ok
  Autotext28="Although %s does not exceeed historical reference levels, they are at levels likely to limit coral community recovery potential."
  
  # Macroalgae consequence ok, baseline low
  Autotext29="Despite %s having increased beyond historical reference levels, macroalgae remain at levels unlikley to severely impact coral communities."
  
  # Macroalgae consequence ok, baseline ok
  Autotext30="%s does not exceeded historical reference levels and remains at levels unlikely to severely impact coral communities."
  
  
  sent.j.c=
    ifelse(isTRUE(Low.j.c) & isFALSE(Low.j),
           sprintf(Autotext24,
                   Indi.desc.j),
           ifelse(isTRUE(Low.j.c) & isTRUE(Low.j),
                  sprintf(Autotext25,
                          Indi.desc.j),
                  ifelse(isFALSE(Low.j.c) & isFALSE(Low.j),
                         sprintf(Autotext26,
                                 Indi.desc.j),
                         if(isFALSE(Low.j.c) & isTRUE(Low.j)){
                           sprintf(Autotext27, Indi.desc.m)})
           )
    )
  
  sent.m.c=
    ifelse(isTRUE(Low.m.c) & isTRUE(Low.m),
           sprintf(Autotext27),
           ifelse(isTRUE(Low.m.c) & isFALSE(Low.m),
                  sprintf(Autotext28,
                          Indi.desc.m),
                  ifelse(isFALSE(Low.m.c) & isTRUE(Low.m),
                         sprintf(Autotext29,
                                 Indi.desc.m),
                         if(isFALSE(Low.m.c) & isFALSE(Low.m)){
                           sprintf(Autotext27,
                                   Indi.desc.m)})
           )
    )
  
  #************#####
  # Composition#####
  #************#####
  #comp data
  data.co<-i.df %>% 
    filter(Indicator=="Community.composition" &  Reference=="Baseline" & Year==y) %>% 
    droplevels() 
  

  
  data.co.low<-i.df%>%
    filter(Indicator=="Community.composition" & Reference=="Baseline") %>% 
    droplevels() %>%
    arrange(Year) %>%
    mutate(Low=ifelse(Upper<0.5,1,0),
           runID.l=data.table::rleid(Low))
  
  start.run.co<-as.character(data.co.low %>% 
                               filter(runID.l==max(runID.l)) %>%
                               summarise(runStart=first(Year)))
  
  low.years.co<-nrow(data.co.low %>% filter(runID.l==max(runID.l) & Low=='1'))
  
  data.co.c<-i.df %>% 
    filter(Indicator=="Community.composition" & Reference=="Critical" & Year==y) %>% 
    droplevels()
  
  Low.co<-data.co$Upper<0.5
  Low.co.c<-data.co.c$Upper<0.5
  
  Indi.desc.co<-as.character(Indi %>% 
                               filter(Indicator=="Community.composition") %>% 
                               pull(Ind.desc))
  

  # comp low current year only - hc ok. 
  Autotext32="Despite the cover of hard corals remaining at or above reference levels, there is evidence that the composition of communities have changed."
  
  # comp low >1 year - hc ok. 
  Autotext32a="Despite the cover of hard corals remaining at or above reference levels, since %s the composition of communities have remained distinct from those historically observed."
  
  # comp ok - hc ok, 
  Autotext33="In addition to the cover of hard corals remaining within reference levels there is no evidence of a substantial change in the composition of coral communities."
  
  # comp low - hc low. current year
  Autotext34="In addition to the low cover of hard corals, the community composition has changed compared to the historical reference."
  
  # comp low >1 year - hc low.
  Autotext35="In addition to the low cover of hard corals, the composition of communities have remianed distinct from those historically observed since %s."
  
  # comp ok - hc low
  Autotext36="Although the cover of hard corals is low, there is no evidence of a substantial change in the composition of coral communities."
  
  # comp low - hc high
  Autotext36a="Although the cover of hard corals is at or above the historical reference for this reef, the composition of coral communities has changed substantially."
  
  
  sent.co<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.hc)& low.years.co==1,
           sprintf(Autotext32),
           ifelse(isTRUE(Low.co) & isFALSE(Low.hc) & low.years.co>0,
                  sprintf(Autotext32a,
                          start.run.co),
                  ifelse(isFALSE(Low.co) & isFALSE(Low.hc) & low.years.co==0,
                         Autotext33,
                         ifelse(isTRUE(Low.co) & isTRUE(Low.hc) & low.years.co==0,
                                Autotext34,
                                ifelse(isTRUE(Low.co) & isTRUE(Low.hc)& low.years.co>0,
                                       sprintf(Autotext35,
                                               start.run.co),
                                       if(isFALSE(Low.co) & isTRUE(Low.hc)){Autotext36}
                                       else{Autotext36a})
                         )
                  )
           )
    )
  
  # new stable state
  Autotext37="While the taxonomic composition of coral communities remains distinct from those observed during the historic reference period they are similar to those more recently observed, suggesting a persitent shift."
  
  # new stable state
  Autotext38="In addition to the taxonomic composition of coral communities remaining distinct from those observed during the historic reference period they are also distinct from those more recently observed, suggesting ongoing changes in community composition."
  

  
  sent.k3<-                                           
    ifelse(isTRUE(Low.co) & isFALSE(Low.co.c),
           sprintf(Autotext37,
                   n.reef.co.low),
           ifelse(isTRUE(Low.co) & isTRUE(Low.co.c),
                  Autotext38,""))
  if(sent.k3==""){sent.k3=NULL}

  #********************************************************************************************
  # Sampling summary lead in for context and to reduce repetition within indicator sentences####
  #********************************************************************************************
  CC='Coral.cover' %in% (i.df$Indicator)
  M="Macroalgae" %in% (i.df$Indicator)
  C="Community.composition" %in% (i.df$Indicator)
  J="Juvenil" %in% (i.df$Indicator)
  P="Recovery.performance" %in% (i.df$Indicator)
  
  sample.autotext="This classification reflects the assesement of indicators relative to their historical baselines"  
  sample.autotext.n1="A clasification could not be made for this reef as estimates for all five indicators were not available"
  
  sent.samp=
    ifelse(isTRUE(CC&C&J&P&M), sample.autotext,sample.autotext.n1)
  
 
  #combine sentences####   
  caption<-format_html_list(c(sent.class,sent.hc,sent.pe,
                              sent.pe.c,sent.jma,sent.j.c,sent.m.c, sent.co, sent.k3),
                            ordered = F)
  note<-paste0("<i>Disclaimer:</i>", sent.samp)
  
  
  return(list(caption, note))
  
}

