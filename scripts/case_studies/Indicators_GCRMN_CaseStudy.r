getwd()
library(arrow)
library(dplyr)
library(ggplot2)

#**********
#Get indicator scores from DMS
# # Providing S3 URL address for dataset of interest
# indicators<-"s3://gbr-dms-data-public/aims-reef-indicators-framework/data.parquet"
# # Connecting to S3 bucket
# s3_conn <- s3_bucket(indicators)
# # Accessing dataset
# indicators_all <- open_dataset(indicators) |>
# collect()

# save(indicators_all, file = "indicators_all.Rdata")
#***********

#***********
#Note that Juvenile scores are not right in the DMS and need to be fixed
#***********

#***********
#Indicator data for GCRMN Case Study
load("indicators_all.Rdata")
# indicators_all<-read.csv("indicator_scores_revised_11092025.csv") |> mutate(Level="reef")
glimpse(indicators_all)

#Look for examples where 3 out of the 4 main indicators are below 0.5
concern_any3 <- indicators_all |>
        filter(
            Level == "reef",
            Reference == "Baseline",
            Indicator %in% c("Juvenile.density", "Macroalgae", "Recovery.performance", "Coral.cover"),
            Upper < 0.5,
            Year > 2010
        ) |>
        group_by(Name, Year) |>
        filter(n_distinct(Indicator) == 3) |>
        ungroup() |>
        droplevels()


#Example for demonstrating how coral cover recovery can be deceiving
#Coral cover is recovering, but very slowly and Macroalgae is high

#Snapper North shallow
Snapper_nth_shallow_DMS<- indicators_all |>
    filter(Name == "Snapper North (shallow slope)",
    Reference=="Baseline",
    Year %in% c(2019, 2021, 2022))|>
    droplevels()

#Data for 3 GCRN Case Study examples
GCRMN_CaseStudy_indicators_DMS<- indicators_all |>
    filter(Name %in% c("Snapper North (shallow slope)", 
    "Lady Musgrave Island (deep slope)", 
    "Agincourt Reef No.1 (deep slope)"),
    Reference=="Baseline",
    )|>
    droplevels() |>
    arrange(Name, Year, Indicator)

save(GCRMN_CaseStudy_indicators_DMS, file = "GCRMN_CaseStudy_indicators_DMS.RData")

#Example years specifically
GCRM_CaseStudy_exampleYears_DMS<- GCRMN_CaseStudy_indicators_DMS |>
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