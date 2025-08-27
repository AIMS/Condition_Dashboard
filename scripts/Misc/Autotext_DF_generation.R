rm(list=ls())
### Generate Autotext into Indicators Dataframe
library(tidyverse)
library(sf)
library(gisaimsr)
library(sf)
source("scripts/Misc/Rep_3.1_Reef_autoText.R")
source("scripts/Misc/Rep_3.1_Regional_autoText.R")
source("scripts/Misc/HighLevel_Classification.R")

##LOAD DATA
scores<-read_csv("Indices.csv")%>%
  mutate(Name=case_when(Level=="reef" ~ paste0(Name," (",Depth,")"),
                        .default=Name))

#Ensure all reefs have Lat and Long values
reefs<-scores |> select(Level,Name,Latitude,Longitude, Depth) |>  unique() |> 
  filter(Level=="reef", !is.na(Latitude))

scores <- scores |> select(-c(Latitude, Longitude)) |> left_join(reefs)

#REfine definitions
scores<-scores%>%mutate(
  Reference=case_when(Metric %in% c("critical","consequence.metric") ~ "Critical",
                      Metric %in% c("reference") ~ "Baseline",
                      .default=Reference),
  Indicator=case_when(is.na(Indicator) ~ "Recovery.performance", .default=Indicator))%>%
  group_by(Level,Name,Year,Shelf, Indicator,Metric,Reference)%>%
  filter(row_number()==1)%>%
  ungroup()

comp<-read_csv("Composition_change.csv")%>%
  mutate(REEF.d=str_replace(REEF.d, "deep slope", "(deep slope)"),
         REEF.d=str_replace(REEF.d, "shallow slope", "(shallow slope)"))

taxaLookup<-read.csv(file="scripts/Misc/taxaLookup.csv")



regions<-st_read("spatial_aggregations.geojson")

##Test for missing indicator scores
test<-scores%>%
  group_by(Level,Name,Year,Shelf, Depth)%>%
  summarise(n=length(unique(Indicator)
  ))%>%
  filter(n<5
  )

#Add NA to missing indicators
test2<-scores |> 
  select(Level,Name,Year,Shelf,Latitude, Longitude, Depth, Reference,Indicator) |>
  group_by(Level, Name, Year,Shelf, Latitude, Longitude, Depth,Reference, Indicator) |> 
  summarise(val=length(Indicator)) |> 
  ungroup() |> 
  pivot_wider(names_from=Indicator, values_from=val,values_fill = 0) |> 
  pivot_longer(!c(Level, Year, Name, Shelf, Latitude, Longitude, Depth, Reference), names_to = "Indicator", values_to = "count")


indicator.ref<- scores |> group_by(Indicator) |> select(Reference,Metric) |> unique()

test2<-test2 |> left_join(indicator.ref)

scores<-scores |>right_join(test2 |> select(!count) )

#Check that every combinantion of sites/years has 10 indicator scores
test4<-scores%>%
  group_by(Level,Name,Year,Shelf, Depth)%>%
  summarise(n=length(Indicator)) |> 
filter(n!=10)

##Separate 

# s.df<-scores |> 
#   anti_join(test, by=c("Level","Name","Year","Shelf", "Depth")) |> 
  

s.df<-scores |> filter(Level!="GBRMPA.MA")

save(s.df, file="indicators_dataframe.Rdata")


i.df.documentation<-list(Aggregation="Spatial aggregation of Indicator scores",
                         Name="Name of the spatial aggregation feature (e.g., reef name, Bioregion number, etc)",
                         # Spatial.Layer="Simple feature (sf) dataframe including geometry and survey depth. For Aggregation beyond reef level, depth is NA",
                         Shelf="Shelf habitat within the spatial aggregation (e.g., Inshore, Offshore, All)",
                         Year="Report year for reef condition assessments. This accounts for surveys across summer within financial year cycles",
                         Scores=list(Description="Indicator score dataframe",
                                     Field.description=list(
                                       Indicator="Coral Reef Condition Indicator evaluated",
                                       Metric="Metric used to calculate the indicator score",
                                       Reference="Threhsold reference used to calculate the indicator score (i.e., Critial, Baseline)",
                                       Median="Median of the indicator score value for a given Aggregation, Shelf and Report Year",
                                       Lower="Lower end of credible intervals with a 90% probability",
                                       Upper="Upper end of the credible intervals with a 90% probability",
                                       tn.reef="Total number of reefs assessed within a given Aggregation, Shelf and Report Year",
                                       n.below="Number of reefs showing indicator score values below a given Refence value and Indicator"
                              )),
                         Comm.Change=list( Description="Observed changes in community composition of Hard and Soft corals",
                                           Field.description=list(
                                             Taxon="Taxonomic identification",
                                             meanDiff="Mean difference in proportional cover (0-1) for a given taxa in a given Aggregation, Shelf and Year, compared to the historical reference",
                                             varDiff="Variance of the Difference obeserved in proportional cover",
                                             k="Number of neightbours used to estimated outliers in community composition"
                                           )),
                         Description="Auto-generated text to assisst in the interpretation of indicator scores",
                         Disclaimer="Additional information relevant to assist in the interpretation of the indicator scores and descriptive text"
)

i.df.documentation.json<-jsonlite::toJSON(i.df.documentation, auto_unbox = TRUE) %>% jsonlite::prettify()

write(i.df.documentation.json, file="indicators_dataframe_documentation.json")



i.df=tibble(Aggregation=character(), Name=character(), Shelf=character(),
            Year=integer(),
            Scores=list(),Comm.Change=list(),Description=character(),
            Disclaimer=character())
# i="Agincourt Reef No.1 (deep slope)"
for(i in unique(s.df$Name)){
  df.n<-s.df |> filter(Name==i)
  l=unique(df.n$Level)
  for (s in unique(df.n$Shelf)){
    df<-df.n |> filter(Shelf==s)
    y=unique(df$Year)
    for (x in y){
      if(l=="reef"){
        txt=tryCatch(sum.reef.tx(i.df = df, y=x), error = function(msg){
          message("Not enough monitoring data")
          return(list("There is not enough monitoring data to provide a synthesis",
                     "<i>Disclaimer:</i>This synopsis reflects the assessment of indicators relative to their historical baselines.")
          )
        })
      }else{
        txt=tryCatch(sum.tx.r(i.df = df, y=x), error = function(msg){
          message("Not enough monitoring data")
          return(list("There is not enough monitoring data to provide a synthesis",
                      "<i>Disclaimer:</i>This synopsis reflects the assessment of indicators relative to their historical baselines.")
          )
        })
      }
      
      # t.i<-as_tibble(df |> 
      #                  filter(Year==x) |> 
      #                  select(-c(Level, Year, Name, Shelf, Latitude, Longitude, Depth))) |> 
      #   mutate(Aggregation=l,
      #          Name=i, Year=x,Shelf=s,
      #          Comm.Change=list(comp |> filter(REEF.d==i, REPORT_YEAR==x) |> select(COMP_2021_DESCRIPTION,Taxon,meanDiff,varDiff, k) |> 
      #            unique() |> 
      #            rename(Taxon_Code=Taxon, Taxon=COMP_2021_DESCRIPTION)),
      #          Description=txt[[1]], Disclaimer=txt[[2]])
      t.i<-tibble(Aggregation=l,
                  Name=i, Year=x,Shelf=s,
                  Comm.Change=list(comp |> filter(REEF.d==i, REPORT_YEAR==x) |> select(COMP_2021_DESCRIPTION,Taxon,meanDiff,varDiff, k) |> 
                                     unique() |> 
                                     rename(Taxon_Code=Taxon, Taxon=COMP_2021_DESCRIPTION)),
                  Description=txt[[1]], Disclaimer=txt[[2]],
                  Scores=list(df |> 
                       filter(Year==x) |> 
                       select(-c(Level, Year, Name, Shelf, Latitude, Longitude, Depth)))
      )
      
      i.df<-bind_rows(i.df,t.i)
    }
  }
}

i.df.json<-jsonlite::toJSON(i.df, auto_unbox = TRUE) %>% jsonlite::prettify()

write(i.df.json, file="indicators_dataframe.json")

