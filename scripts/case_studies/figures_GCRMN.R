source("scripts/plotting_functions/radial.plot.summary.R")
source("scripts/plotting_functions/comp_change.R")
source("scripts/Misc/HighLevel_Classification.R")

library(gridExtra)
library(patchwork)
library(cowplot)

scores<-read.csv("Indices.csv")
load("scripts/case_studies/GCRMN_CaseStudy_HC_reefs.RData")
df.comp<-read_csv("Composition_change.csv")%>%
  mutate(REEF.d=str_replace(REEF.d, "deep slope", "(deep slope)"),
         REEF.d=str_replace(REEF.d, "shallow slope", "(shallow slope)"))


scale_fill_class.c <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#fc8d59" , "#91bfdb", "#91bfdb"), 
                      c( "Below","Within", "Above")),
    drop=F,
    ...
  )
}

#Figure 1 ##### 
#Case Study 1: Agincourt Reef
df1<-scores |> filter(Level=="reef", Year==2023,Name=="Agincourt Reef No.1 (deep slope)") |> 
  mutate(Classification=case_when(
    Lower > 0.5 ~ "Above",
    Upper < 0.5 ~ "Below",
    is.na(Lower) ~ NA,
    .default="Within"))
ref="Baseline"

f1.cond<-radial.plot.summary( df1, ref)+
  scale_fill_class.c()
f1.cond

f1.temp<-GCRMN_CaseStudy_HC_reefs |> filter(domain_name=="AGINCOURT REEF NO.1") |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2023, y = 0.42, xend = 2023, yend = 0.38),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#EC7014",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", y="Hard Coral Cover (%) +/- CI")+
  theme_classic()+  
  theme(axis.title = element_text(size=12, face = "bold"),
                          axis.text.y = element_text(size=10))

f1.temp

f1.comp<-comp_plot(df.comp, df1, y=2023, s="All")+
  labs(y="Mean Change in Cover (%)", x="Taxa")+
  scale_fill_manual(values=c("#a1d76a","#e9a3c9"))+
  theme_classic()+
  theme(axis.title = element_text(size=12, face = "bold"),
        axis.text.y = element_text(size=10, face = "italic"))


f1.comp


f1<-plot_grid(f1.temp, plot_grid(f1.cond, f1.comp, nrow=2, labels=c("B", "C")), labels=c("A"))
f1
ggsave(plot=f1, path = "scripts/case_studies/",filename = "Fig1.png",width = 35, height = 20,units = "cm")
ggsave(plot=f1, path = "scripts/case_studies/",filename = "Fig1.svg",width = 35, height = 20,units = "cm")



#Figure 2 #####
f2a.temp<-GCRMN_CaseStudy_HC_reefs |> filter(domain_name=="SNAPPER ISLAND") |> 
  mutate(cond.range=case_when(report_year>2019 ~ "After", .default="Before")) |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year, color=cond.range), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper, color=cond.range), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2019, y = 0.2, xend = 2019, yend = 0.13),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#EC7014",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("grey", "black"))+
  labs(x="Year", y="Hard Coral Cover (%) +/- CI")+
  theme_classic()+  
  theme(axis.title = element_text(size=12, face = "bold"),
        axis.text.y = element_text(size=10), legend.position = "none")

f2a.temp

df2b<-scores |> filter(Level=="reef", Year==2019,Name=="Snapper North (shallow slope)") |> 
  mutate(Classification=case_when(
    Lower > 0.5 ~ "Above",
    Upper < 0.5 ~ "Below",
    is.na(Lower) ~ NA,
    .default="Within"))

f2b.cond<-radial.plot.summary( df2b, ref)+
  scale_fill_class.c()
f2b.cond


f2c.temp<-GCRMN_CaseStudy_HC_reefs |> filter(domain_name=="LADY MUSGRAVE ISLAND") |> 
  mutate(cond.range=case_when(report_year>2015 ~ "After", .default="Before")) |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year, color=cond.range), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper, color=cond.range), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2015, y = 0.45, xend = 2015, yend = 0.35),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#EC7014",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("grey", "black"))+
  labs(x="Year", y="Hard Coral Cover (%) +/- CI")+
  theme_classic()+  
  theme(axis.title = element_text(size=12, face = "bold"),
        axis.text.y = element_text(size=10), legend.position = "none")

f2c.temp


df2d<-scores |> filter(Level=="reef", Year==2015,Name=="Lady Musgrave Island (deep slope)") |> 
  mutate(Lower=ifelse(is.na(Lower),0.5, Lower ),
         Upper=ifelse(is.na(Upper),0.5, Upper),
         Median=ifelse(is.na(Median),0.5, Median ),
         Classification=case_when(
           Lower > 0.5 ~ "Above",
           Upper < 0.5 ~ "Below",
           is.na(Lower) ~ NA,
           .default="Within"))

f2d.cond<-radial.plot.summary( df2d, ref)+
  scale_fill_class.c()
f2d.cond


f2<-plot_grid(f2a.temp, f2b.cond, f2c.temp, f2d.cond, nrow=2, ncol=2, labels=c("A","B","C","D"))
 f2             
ggsave(plot=f2, path = "scripts/case_studies/",filename = "Fig2.png",width = 35, height = 25,units = "cm")
ggsave(plot=f2, path = "scripts/case_studies/",filename = "Fig2.svg",width = 35, height = 25,units = "cm",device = "svg")
