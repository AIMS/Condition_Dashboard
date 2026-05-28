getwd()
library(arrow)
library(dplyr)
library(ggplot2)

##LOAD DATA
indices <- readRDS("data/GCRMN_CaseStudy_indicators_DMS.RDS")
load("data/GCRMN_CaseStudy_HC_reefs.RData")

# FOR REFERENCE - Original code to filter the data for the case study
#Indicator data for GCRMN Case Study
load("indices.Rdata")
#Data for 3 GCRN Case Study examples
GCRMN_indices<- indices |>
    filter(Name %in% c("Snapper North", 
    "Lady Musgrave Island", 
    "Agincourt Reef No.1"),
    Reference=="Baseline",
    )|>
    droplevels() |>
    arrange(Name, Year, Indicator)

saveRDS(GCRMN_indices, file = "data/GCRMN_CaseStudy_indicators_DMS.RDS")

#Example years specifically
GCRM_CaseStudy_exampleYears_DMS<- GCRMN_indices |>
    filter(Name=="Snapper North (shallow slope)" & Year== "2019"|
    Name=="Lady Musgrave Island (deep slope)" & Year=="2014"|
    Name=="Agincourt Reef No.1 (deep slope)" & Year == "2023") |>
    droplevels()


#Get modelled hard coral cover from DMS
# aims_monitoring_data<- "s3://gbr-dms-data-public/aims-ltmp-mmp-coralreef-model/data.parquet"

# connect_aims_monitoring_data <- s3_bucket(aims_monitoring_data)

# df <- open_dataset(aims_monitoring_data)
# df$schema
# GCRMN_CaseStudy <- df |>
#   collect()

# save(GCRMN_CaseStudy, file = "GCRMN_CaseStudy.RData")

load("GCRMN_CaseStudy.RData")

reef_HC<- GCRMN_CaseStudy |>
filter(domain_category=="reef",
variable=="HARD CORAL", purpose=="GROUP_LEVEL")

GCRMN_CaseStudy_HC_reefs<- reef_HC |>
filter(domain_name %in% c("LADY MUSGRAVE ISLAND", "AGINCOURT REEF NO.1","SNAPPER ISLAND"),
data_type=="photo-transect", 
depth %in% c(9,2),
!reef_zone == "South") |>
droplevels() |>
arrange(domain_name, report_year)

save(GCRMN_CaseStudy_HC_reefs, file = "GCRMN_CaseStudy_HC_reefs.RData")

library(ggplot2)
ggplot(GCRMN_CaseStudy_reefs, aes(x=report_year, y=median, color=domain_name)) +
  geom_point()+
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(width=0.5)) +
  geom_line() +
  facet_wrap(~domain_name, ncol=1) +
  labs(title="GCRMN Case Study - Reef Health Indicators",
       x="Year",
       y="Indicator Value") +
  theme_minimal()