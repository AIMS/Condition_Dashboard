### Assesing the likelihood of reef maintaining a Good condition over time

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
library(brms)
library(rstan)
library(tidybayes)
library(patchwork)
library(DHARMa)
library(HDInterval)
library(emmeans)
library(paletteer)
source('scripts/Misc/helperFunctions.R')
source("scripts/Misc/HighLevel_Classification.R")

## Load Data ####
scores<-read.csv("indicator_scores_revised_28092025.csv") |> mutate(Level="reef")
regions<-nrm_regions%>%
  select(NAME, geometry)%>%rename(Name=NAME)%>%
  mutate(Region= "NRM")

##Check data
check<-scores |> filter(Level=="reef", !is.na(Median)) |> group_by(Year, Name, Reference,Indicator) |> tally() |> 
  spread(key=Indicator, value = n, fill = 0) |> 
  group_by(Year, Reference) |> 
  summarise(reefs=length(unique(Name)),
            across(c(Community.composition, Coral.cover, Juvenile.density, Macroalgae, Recovery.performance), ~sum(.x)/reefs) 
                   ) |> 
  pivot_longer(cols=c(Community.composition, Coral.cover, Juvenile.density, Macroalgae, Recovery.performance),
               names_to = "Indicator",
               values_to = "Proportion") |> 
  ggplot(aes(x=Year, y=Proportion, fill=Reference)) +
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap("Indicator") + 
  theme_classic()

test.crt<-scores |> 
  group_by(fYEAR, Name, Indicator, Reference) |> 
  mutate(below=case_when(Upper< 0.5 ~ 1, Upper>=0.5 ~0, .default=NA)) |> 
  ungroup() |> 
  select(c(fYEAR, Name, Indicator, Reference, below)) |> 
  pivot_wider(names_from= Reference, values_from = below, values_fill = NA) |> 
  mutate(Critical=case_when(
    (Critical==1 & Baseline==0) ~ TRUE,
    .default= FALSE
  ),
  Baseline=case_when(
    (Critical==1 & Baseline==0) ~ FALSE,
    .default= TRUE
  ),
  # Ensure we always use baseline from community composition
  Critical=case_when(
    Indicator=="Community.composition" ~ FALSE,
    .default=Critical),
  Baseline=case_when(
    Indicator=="Community.composition" ~ TRUE,
    .default=Baseline)
  
  ) |> 
  pivot_longer(!c(fYEAR, Name, Indicator), names_to = "Reference", values_to = "Use")


scores<-scores |> 
  left_join(test.crt) |> 
  filter(!is.na(Median), Use==TRUE) |> 
  select(-c(X, Use,Metric, Reference))

### Spatial data layers

GBRMP<-gisaimsr::gbr_bounds |> st_buffer( 1)

## Zones
northern.bbox <- st_bbox(c(xmin = 142, xmax = 155, ymin = -15.4, ymax = 0)) %>%
  st_as_sfc() %>%
  st_sf(crs = st_crs(GBRMP)) %>%
  mutate(Zone = 'Northern')
central.bbox <- rbind(c(142,-20.7),
                      c(148.7,-20.7),
                      c(152,-19.6),
                      c(152,-15.4),
                      c(142,-15.4)) %>%
  st_linestring() %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = st_crs(GBRMP)) %>%
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
  st_sfc(crs = st_crs(GBRMP)) %>%
  st_sf() %>%
  mutate(Zone = 'Southern')
zones.bbox <- rbind(northern.bbox, central.bbox, southern.bbox)

zones <- GBRMP %>%
  st_intersection(zones.bbox) %>%
  mutate(Region = 'ZONE',
         Name = Zone) %>%
  dplyr::select(Zone, geometry) %>%
  suppressMessages() %>%
  suppressWarnings() |> 
  st_transform(4326)

reefs<-read_csv("Indices.csv")%>%ungroup%>%
  filter(Level=="reef")%>%
  select(c(Name,Shelf,Depth,Latitude,Longitude))%>%
  unique()%>%
  filter(!is.na(Latitude))|> 
  mutate(Name = str_remove_all(Name, "\\(|\\)")) |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

##Ancillary Functions ####
scale_fill_class.c <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#fc8d59" , "#91bfdb", "#91bfdb"), 
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


rc<-scores |> 
  filter(Level=="reef", Name %in% reefs$Name) |> 
  left_join(reefs) |> 
  group_by(Name, Year, Indicator) |> 
  # mutate((across(.cols = c(Median, Lower, Upper), .fns = ~ ifelse(is.na(.), 0.5, .), .names = "{.col}"))) |> 
  # summarise((across(.cols = c(Median, Lower, Upper), .fns = ~ mean(., na.rm=TRUE), .names = "{.col}"))) |> 
  Cond.Class() |> 
  mutate(Class=factor(Class, levels=c("Good","Watch","Warning I", "Warning II", "Critical", "Insuficient data"))) |> 
  left_join(reefs) |> 
  st_as_sf() |> 
  ungroup() |> 
  st_join(zones) |> 
  st_join(regions |> rename(NRM=Name) |> st_transform(st_crs(reefs)) |> st_make_valid()) |> 
  select(Zone,Shelf, NRM,Name, Depth, Year, Class, geometry) 
  


###  Tally total number of reef per class
rc.s<- 
  rc|> 
  filter(Class != "Insuficient data") |> 
  st_drop_geometry() |> 
  filter(Year>2015) |> 
  mutate(Zone=ifelse(is.na(Zone), "Southern", Zone),
         fYear=factor(Year)) |> 
  group_by(Zone, Shelf,NRM, Year, fYear, Class) |> 
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(Class) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 


## fill in missing classes

Class <- rc.s |> filter(Class != "Insuficient data") |> droplevels() |> pull(Class) |> unique()
data.filler <- rc.s %>%
  filter(Class != "Insuficient data") |> 
  dplyr::select(
    Zone,Shelf, NRM,Year, fYear) |> 
  distinct() |> 
  tidyr::crossing(Class = Class) 

rc.s <-
  rc.s|> filter(Class != "Insuficient data") |> 
  full_join(data.filler) |>
  group_by(Zone,Shelf,NRM, Year, fYear) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE))

##EXPLORATORY PLOTS

#Good and Watch
rc.s|> filter(Class %in% c("Good", "Watch")) |> 
  group_by(Zone, Shelf, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, color=Class))+
  geom_point()+
  geom_line()+
  facet_grid(Zone~Shelf)+
  scale_y_continuous("Reef in Good condition(%)", labels = scales::label_number(scale =  100)) +
  geom_text(aes(x=Year, y=0.85, label=as.character(TOTAL)))+
  theme_classic()

#Good and Watch NRM
rc.s|> filter(Class %in% c("Good", "Watch")) |> 
  group_by(NRM, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, color=Class))+
  geom_point()+
  geom_line()+
  facet_wrap("NRM")+
  scale_y_continuous("Reef in Good condition(%)", labels = scales::label_number(scale =  100)) +
  geom_text(aes(x=Year, y=0.85, label=as.character(TOTAL)))+
  theme_classic()


#BarPlot Zones and Shelf
rc.s|> 
  group_by(Zone, Shelf, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, fill=Class))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  facet_grid(Zone~Shelf)+
  scale_y_continuous("Reef Condition (%)", labels = scales::label_number(scale =  100)) +
  scale_fill_paletteer_d("lisa::OskarSchlemmer")+
  geom_text(aes(x=Year, y=1.1, label=as.character(TOTAL)))+
  theme_classic()

#BarPlot GBR
rc.s|> 
  group_by(Shelf, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, fill=Class))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  facet_grid(.~Shelf)+
  scale_y_continuous("Reef Condition (%)", labels = scales::label_number(scale =  100)) +
  geom_text(aes(x=Year, y=1.1, label=as.character(TOTAL)))+
  scale_fill_paletteer_d("lisa::OskarSchlemmer")+
  theme_classic()


## MODELING ####
#Define priors
rc.s |> filter(Class=="Good") |> 
  mutate(PROP = COUNT/TOTAL) |> 
  group_by(Year) |>
  summarise(
    qlogis(mean(PROP)),
    qlogis(sd(PROP)))

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 1), class =  "b") 
# +
#   prior(student_t(3, 0, 2.5), class = "sd")

##fit model
form <- bf(COUNT | trials(TOTAL) ~ fYear*(Zone+Shelf),
           family =  binomial(link =  "logit"))

model1 <- brm(form,
              data = rc.s |> filter(Class=="Good"),
              prior = priors,
              sample_prior = "yes",
              iter =  5000,
              warmup =  1000,
              chains =  3,
              cores =  3,
              thin =  5,
              refresh = 0
)




g1 <-
  model1 |>
  emmeans(~fYear+Zone+Shelf) |>
  gather_emmeans_draws() |>
  mutate(fit = plogis(.value)) |>
  group_by(fYear, Zone, Shelf) |> 
  summarise(median_hdci(fit, .width = c(0.66, 0.95))) |> 
  ggplot(aes(y = y, x = fYear)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = as.numeric(fYear),
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_grid(Shelf~Zone)+
  scale_y_continuous("Reef in Good condition(%)", labels = scales::label_number(scale =  100)) +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

g2 <-
  model1 |>
  emmeans(~fYear*Zone) |>
  gather_emmeans_draws() |>
  mutate(fit = plogis(.value)) |>
  group_by(fYear, Zone) |> 
  summarise(median_hdci(fit, .width = c(0.66, 0.95))) |> 
  ggplot(aes(y = y, x = fYear)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = as.numeric(fYear),
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  geom_text(data=rc.s |> filter(Class=="Good") |> 
              group_by(fYear, Zone) |> 
              summarise(TOTAL=sum(TOTAL)), 
            aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_grid(.~Zone)+
  scale_y_continuous("Reef in Good condition(%)", labels = scales::label_number(scale =  100)) +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

g3 <-
  model1 |>
  emmeans(~fYear*Shelf) |>
  gather_emmeans_draws() |>
  mutate(fit = plogis(.value)) |>
  group_by(fYear, Shelf) |> 
  summarise(median_hdci(fit, .width = c(0.66, 0.95))) |> 
  ggplot(aes(y = y, x = fYear)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = as.numeric(fYear),
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  geom_text(data=rc.s |> filter(Class=="Good") |> 
              group_by(fYear, Shelf) |> 
              summarise(TOTAL=sum(TOTAL)), 
            aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_grid(.~Shelf)+
  scale_y_continuous("Reef in Good condition(%)", labels = scales::label_number(scale =  100)) +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()



rc.p<-rc |> group_by(Shelf, Year, Class) |> 
  filter(Year>2010) |> 
  summarise(count_in_group = n()) |> 
  mutate(proportion = count_in_group / sum(count_in_group)) |> 
  st_drop_geometry()




### Modelling Likelihood #####

rc.g<-rc |> 
  filter( Year>2015, Class!="Insuficient data") |> 
  mutate(Good=case_when( Class %in% c("Good") ~1,
                         .default=0),
         Zone=ifelse(is.na(Zone), "Southern", Zone),
         Shelf=factor(Shelf), Zone=factor(Zone), fYear=factor(Year)
         ) |> 
  st_drop_geometry()

priors2 <- brms::set_prior("normal(0, 5)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") +
  brms::set_prior("student_t(3, 0, 2.5)", class = "sd")

model2<- brm(Good ~ fYear + (1|Zone/Shelf/Name), family=bernoulli(link = "logit"),
             data=rc.g, iter = 4e3, sample_prior = "yes", prior = priors2,
             cores = 4, control = list(adapt_delta = 0.99))

priors3 <- brms::set_prior("normal(0, 5)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") 
model3<- brm(Good ~ Year*(Zone+Shelf), family=bernoulli(link = "logit"),
             data=rc.g, iter = 4e3, sample_prior = "yes", prior = priors3,
             cores = 4, control = list(adapt_delta = 0.99))


# plot predictions

nd.f <- expand.grid(Zone = rc.p |> pull(Zone) |> unique(),
                  Shelf=  rc.p |> pull(Shelf) |> unique(),
                  fYear=levels(rc.p$fYear))
nd <- expand.grid(Zone = rc.p |> pull(Zone) |> unique(),
                  Shelf=  rc.p |> pull(Shelf) |> unique(),
                  Year=unique(rc.p$Year))



good_pred<-nd |> 
  add_epred_draws(model2,re_formula = NA) %>%
  group_by(fYear, Zone, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  ggplot(aes(x = fYear, y = y), color="light grey", alpha=0.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = as.numeric(fYear),
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_grid(Shelf~Zone)+
  scale_y_continuous("Likelihood") +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

good_pred


good_pred3<-nd |> 
  add_epred_draws(model3,re_formula = NA) %>%
  group_by(Year, Zone, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  ggplot(aes(x = Year, y = y), color="light grey", alpha=0.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = Year,
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_grid(Shelf~Zone)+
  scale_y_continuous("Likelihood") +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

good_pred3

good_gbr<-expand.grid(Zone = NA,
                      fYear=rc.p |> pull(fYear)|> unique(), Shelf=NA) |> 
  add_epred_draws(model2,re_formula = NA) %>%
  mutate(fit = plogis(.epred)) |> 
  group_by(fYear) |> 
  summarise(median_hdci(fit, .width = c(0.66, 0.95))) |> 
  ggplot(aes(x = fYear, y = y), color="light grey", alpha=0.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = fYear,
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = FALSE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  scale_y_continuous("Likelihood") +
  scale_x_discrete("") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()
good_gbr


### Modelling Likelihood fitzroy #####


rc.fitzroy<-rc |> 
  filter( Year>2015, Class!="Insuficient data", NRM=="Fitzroy") |> 
  mutate(Good=case_when( Class %in% c("Good") ~1,
                         .default=0),
         Shelf=factor(Shelf), Zone=factor(Zone), fYear=factor(Year)
  ) |> 
  st_drop_geometry()

priors3 <- brms::set_prior("normal(0, 5)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") 
model3<- brm(Good ~ fYear + Shelf + (1|Name), family=bernoulli(link = "logit"),
             data=rc.fitzroy, iter = 4e3, sample_prior = "yes", prior = priors3,
             cores = 4, control = list(adapt_delta = 0.99))


# plot predictions


nd <- expand.grid(Shelf=  rc.fitzroy |> pull(Shelf) |> unique(),
                  fYear=levels(rc.fitzroy$fYear),
                  Year=unique(rc.fitzroy$Year))



good_pred<-nd |> 
  add_epred_draws(model3,re_formula = NA) %>%
  group_by(fYear, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  ggplot(aes(x = fYear, y = y), color="light grey", alpha=0.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = fYear,
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = TRUE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_wrap("Shelf")+
  scale_y_continuous("Likelihood of a reef \nbeing in Good Condition") +
  scale_x_discrete("Year") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

good_pred


#BarPlot GBR
mp.fitzrow<-rc.s|> 
  filter(NRM=="Fitzroy") |> 
  group_by(Shelf, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, fill=Class))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  facet_grid(.~Shelf)+
  scale_y_continuous("Reef Condition (%)", labels = scales::label_number(scale =  100)) +
  geom_text(aes(x=Year, y=1.1, label=as.character(TOTAL)))+
  scale_fill_paletteer_d("lisa::OskarSchlemmer")+
  theme_classic()

#Likelhood per indicator (above)
scores.fitzroy<-scores |> 
  filter(Level=="reef", Name %in% reefs$Name, Year>2015, !is.na(Upper)) |> 
  left_join(reefs) |> 
  st_as_sf() |> 
  ungroup() |> 
  st_join(zones) |> 
  st_join(regions |> rename(NRM=Name) |> st_transform(st_crs(reefs)) |> st_make_valid()) |> 
  st_drop_geometry() |> 
  filter(NRM=="Fitzroy") |> 
  select(Zone,Shelf, NRM,Name, Depth, Year, Indicator, Median, Upper, Lower) |> 
  group_by(Year, Name, Indicator) |> 
  mutate(Above=case_when(Upper<0.5 ~ 0,
                         .default = 1)
         )

priors3 <- brms::set_prior("normal(0, 1)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") 

# +
#   brms::set_prior("student_t(3, 0, 2.5)", class = "sigma")

model3.ma<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
             data=scores.fitzroy |> 
               filter(Indicator=="Macroalgae"), iter = 4e3, sample_prior = "yes", prior = priors3,
             cores = 4, control = list(adapt_delta = 0.99))

model3.ju<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.fitzroy |> 
                  filter(Indicator=="Juvenile.density" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))

model3.cc<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.fitzroy |> 
                  filter(Indicator=="Coral.cover"), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))
model3.rp<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.fitzroy |> 
                  filter(Indicator=="Recovery.performance" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))
model3.co<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.fitzroy |> 
                  filter(Indicator=="Community.composition" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))


# plot predictions

nd <- expand.grid(Shelf=  rc.fitzroy |> pull(Shelf) |> unique(),
                  Year=unique(rc.fitzroy$Year))

Ind.fitzroy<-nd |> 
  add_epred_draws(model3.ma,re_formula = NA) %>%
  group_by(Year, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  mutate(Indicator="Macroalgae") |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.ju,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Juveniles") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.cc,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Coral_cover") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.rp,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Recovery.performance") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.co,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator= "Community.composition") 
  ) |> 
  ggplot(aes(x = Year, y = y, group=Indicator), color="light grey", alpha=0.5) +
  # geom_ribbon(aes(ymin = ymin, ymax = ymax, x = Year,
  #                 group = .width, alpha = factor(.width), fill=Indicator),  show.legend = TRUE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width), color=Indicator), show.legend = TRUE) +
  geom_line(aes(color=Indicator))+
  facet_wrap("Shelf")+
  scale_y_continuous("Likelihood of a reef\n being be within expectations") +
  scale_x_continuous("Year") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

# 
# 
# Ind.fitzroy<-scores |> 
#   filter(Year>2015, Level=="NRM", Name=="Fitzroy", Shelf!="All", Reference=="Baseline", 
#          Indicator %in% c("Macroalgae", "Juvenile.density", "Coral.cover", "Recovery.performance")) |> 
#   # ggplot(aes(x = Year, y = n.below/tn.reefs, group=Indicator), color="light grey", alpha=0.5) +
#   ggplot(aes(x = Year, y = Median, color=Indicator))+
#   geom_pointrange(aes(x=Year, ymin=Lower, ymax=Upper, color=Indicator))+
#   # geom_bar(aes(fill=Indicator), stat = "identity", position="dodge") +
#   geom_point(aes(color=Indicator))+
#   geom_line(aes(color=Indicator))+
#   # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
#   facet_wrap("Shelf")+
#   scale_y_continuous("Indicator Score") +
#   scale_x_discrete("") +
#   theme_classic()
# Ind.fitzroy


plot_grid(mp.fitzrow,good_pred,Ind.fitzroy,nrow = 3, ncol=1, labels = c("A","B","C"))



### Modelling Likelihood Central GBR #####


rc.central<-rc |> 
  filter( Year>2015, Class!="Insuficient data", Zone=="Central") |> 
  mutate(Good=case_when( Class %in% c("Good") ~1,
                         .default=0),
         Shelf=factor(Shelf), Zone=factor(Zone), fYear=factor(Year),
         Name=factor(Name)
  ) |> 
  st_drop_geometry()

priors3 <- brms::set_prior("normal(0, 5)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") 

model3<- brm(Good ~ fYear+Shelf + (1|Name), family=bernoulli(link = "logit"),
             data=rc.central, iter = 4e3, sample_prior = "yes", prior = priors3,
             cores = 4, control = list(adapt_delta = 0.99))


# plot predictions


nd <- expand.grid(Shelf=  rc.central |> pull(Shelf) |> unique(),
                  # Year=unique(rc.central$Year),
                  fYear=levels(rc.central$fYear),
                  Name=NA)



good_pred<-nd |> 
  add_epred_draws(model3,re_formula = NA) %>%
  group_by(fYear, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  ggplot(aes(x = fYear, y = y), color="light grey", alpha=0.5) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, x = fYear,
                  group = .width, alpha = factor(.width)), fill = "skyblue", show.legend = TRUE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width)), show.legend = FALSE) +
  # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
  facet_wrap("Shelf")+
  scale_y_continuous("Likelihood of a reef \nbeing in Good Condition") +
  scale_x_discrete("Year") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

good_pred


#BarPlot GBR Central
mp.central<-rc.s|> 
  filter(Zone=="Central") |> 
  group_by(Shelf, Year, Class) |> 
  summarise(COUNT=sum(COUNT), TOTAL=sum(TOTAL)) |> 
  ggplot(aes(x=Year, y=COUNT/TOTAL, fill=Class))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  facet_grid(.~Shelf)+
  scale_y_continuous("Reef Condition (%)", labels = scales::label_number(scale =  100)) +
  geom_text(aes(x=Year, y=1.1, label=as.character(TOTAL)))+
  scale_fill_paletteer_d("lisa::OskarSchlemmer")+
  theme_classic()

#Likelhood per indicator (above)
scores.central<-scores |> 
  filter(Level=="reef", Name %in% reefs$Name, Year>2015, !is.na(Upper)) |> 
  left_join(reefs) |> 
  st_as_sf() |> 
  ungroup() |> 
  st_join(zones) |> 
  st_join(regions |> rename(NRM=Name) |> st_transform(st_crs(reefs)) |> st_make_valid()) |> 
  st_drop_geometry() |> 
  filter(Zone=="Central") |> 
  select(Zone,Shelf, NRM,Name, Depth, Year, Indicator, Median, Upper, Lower) |> 
  group_by(Year, Name, Indicator) |> 
  mutate(Above=case_when(Upper<0.5 ~ 0,
                         .default = 1)
  )

priors3 <- brms::set_prior("normal(0, 1)", class = "Intercept") +
  brms::set_prior("normal(0, 1)", class = "b") 

# +
#   brms::set_prior("student_t(3, 0, 2.5)", class = "sigma")

model3.ma<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.central |> 
                  filter(Indicator=="Macroalgae"), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))

model3.ju<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.central |> 
                  filter(Indicator=="Juvenile.density" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))

model3.cc<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.central |> 
                  filter(Indicator=="Coral.cover"), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))
model3.rp<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.central |> 
                  filter(Indicator=="Recovery.performance" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))
model3.co<- brm(Above ~ Year*Shelf + (1/Name), family=bernoulli(link = "logit"),
                data=scores.central |> 
                  filter(Indicator=="Community.composition" ), iter = 4e3, sample_prior = "yes", prior = priors3,
                cores = 4, control = list(adapt_delta = 0.99))


# plot predictions

nd <- expand.grid(Shelf=  rc.central|> pull(Shelf) |> unique(),
                  Year=unique(rc.central$Year))

Ind.central<-nd |> 
  add_epred_draws(model3.ma,re_formula = NA) %>%
  group_by(Year, Shelf) |> 
  summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
  mutate(Indicator="Macroalgae") |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.ju,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Juveniles") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.cc,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Coral_cover") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.rp,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator="Recovery.performance") 
  ) |> 
  bind_rows(
    nd |> 
      add_epred_draws(model3.co,re_formula = NA) %>%
      group_by(Year, Shelf) |> 
      summarise(median_hdci(.epred, .width = c(0.66, 0.95))) |> 
      mutate(Indicator= "Community.composition") 
  ) |> 
  ggplot(aes(x = Year, y = y, group=Indicator), color="light grey", alpha=0.5) +
  # geom_ribbon(aes(ymin = ymin, ymax = ymax, x = Year,
  #                 group = .width, alpha = factor(.width), fill=Indicator),  show.legend = TRUE) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax,
                      linewidth = factor(.width), color=Indicator), show.legend = TRUE) +
  geom_line(aes(color=Indicator), show.legend = FALSE)+
  facet_wrap("Shelf")+
  scale_y_continuous("Likelihood of a reef\n being be within expectations") +
  scale_x_continuous("Year") +
  scale_alpha_manual(values = c(0.5, 0.2)) +
  scale_linewidth_manual(values = c(1, 0.5)) +
  theme_classic()

# 
# 
# Ind.fitzroy<-scores |> 
#   filter(Year>2015, Level=="NRM", Name=="Fitzroy", Shelf!="All", Reference=="Baseline", 
#          Indicator %in% c("Macroalgae", "Juvenile.density", "Coral.cover", "Recovery.performance")) |> 
#   # ggplot(aes(x = Year, y = n.below/tn.reefs, group=Indicator), color="light grey", alpha=0.5) +
#   ggplot(aes(x = Year, y = Median, color=Indicator))+
#   geom_pointrange(aes(x=Year, ymin=Lower, ymax=Upper, color=Indicator))+
#   # geom_bar(aes(fill=Indicator), stat = "identity", position="dodge") +
#   geom_point(aes(color=Indicator))+
#   geom_line(aes(color=Indicator))+
#   # geom_text(data=rc.s |> filter(Class=="Good"), aes(x=fYear, y=0.75, label=as.character(TOTAL)))+
#   facet_wrap("Shelf")+
#   scale_y_continuous("Indicator Score") +
#   scale_x_discrete("") +
#   theme_classic()
# Ind.fitzroy


plot_grid(mp.central,good_pred,Ind.central,nrow = 3, ncol=1, labels = c("A","B","C"))

