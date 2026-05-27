source("scripts/plotting_functions/radial.plot.summary.R")
source("scripts/plotting_functions/comp_change.R")
source("scripts/Misc/HighLevel_Classification.R")

library(gridExtra)
library(patchwork)
library(cowplot)
library(scales)
library(ggtext)

#Load Data and define parameters ####
load("indices.RData")
conf_thsld<-0.8
scores<-indices |> filter(Level=="reef") |> 
  filter(!Reference == "Combined") |> #This is the straight average of the baseline and critical scores. Removed to avoid confusion, since we're not using it
  #since the original code probably uses the term 'Combined' throughout, set the correct Reference level to be called 'Combined'
  mutate(Reference=ifelse(Reference=="Combined_adjusted", "Combined", Reference)) |>
  mutate(fYEAR=Year, Year=as.numeric(as.character(Year)),
         Reference=case_when(
           (Reference=="Baseline") & (Indicator == "Community.composition") ~ "Combined",
           .default=Reference),
         Median=ifelse(is.na(Median), 0.5, Median),
         Upper=ifelse(is.na(Upper), 0.5, Upper),
         Lower=ifelse(is.na(Lower), 0.5, Lower))

load("scripts/case_studies/GCRMN_CaseStudy_HC_reefs.RData") ##modelled coral cover
load("data/2015_baseline/GBR.LOF.table.RData") ## Composition change


##Anciliary functions and formats ####
scale_fill_class.c <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#FC6500" , "#48C617", "#48C617"), 
                      #                   
                      # values = setNames(c("#fc8d59" , "#91bfdb", "#91bfdb"), #FFBE18
                      c( "Below","Within", "Above")),
    drop=F,
    ...
  )
}

my_theme=
  theme(
  axis.text = element_text(size=16, vjust = 0.5,family = "Bell MT"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  axis.title = element_text(size=18, face = "bold", family = "Bell MT", margin = margin(r=10, unit="mm")),
)

#Figure 1 ##### 
#Case Study 1: Agincourt Reef
ref="Combined"
df1<-scores |> 
  filter(Level=="reef", Year==2024,Name=="Agincourt Reef No.1", Reference==ref) |> 
  mutate(Classification=case_when(
    Lower > 0.5 ~ "Above",
    Upper < 0.5 ~ "Below",
    is.na(Lower) ~ NA,
    .default="Within"),
    Shelf="Offshore", Depth="Deep slope")
#Class.df1<-Cond.Class(df1, conf_thsld)
Class.df1<-Cond.Class.D(df1, conf_thsld)

f1.cond<-radial.plot.summary( df1, ref)+
  scale_fill_class.c()+
  scale_x_discrete(labels = label_wrap_gen(width = 15)) + # Wrap x-axis labels at 10 characters
  labs(title =  Class.df1|> pull(Class) )+
  my_theme+
  theme(axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y= element_blank(),
        plot.title = element_textbox_simple(
          fill = "#0F85A0FF", # Background color of the textbox
          color = "white",    # Text color
          box.color = NULL, # Border color of the textbox
          r = unit(8, "pt"),   # Radius for rounded corners 
          
          padding = unit(c(5, 5, 5, 5), "pt"), # Padding around the text
          width = NULL,size = 16,
          # maxwidth = unit(120, "pt"),
          margin = unit(c(0, 0, 0, 0), "pt"),  # Margin around the textbox
          halign = 0.5,        # Horizontal alignment of the text
          hjust = 1
        )
  )
f1.cond

f1.temp<-GCRMN_CaseStudy_HC_reefs|> filter(domain_name=="AGINCOURT REEF NO.1") |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2024, y = 0.5, xend = 2024, yend = 0.44),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#EC7014",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5))+
  labs(x="Year", y="Hard Coral Cover (%)")+
  theme_classic()+
  my_theme

f1.temp

f1.comp<-comp_plot(comp = GBR.LOF.table, i.df.r = df1 |> mutate(Reference="Baseline",
                                                                Name=paste(Name, tolower(Depth), sep=" ")), y=2024, s="Offshore")+
  labs(y="Change (%)", x="Taxa")+
  scale_fill_manual(values=c("#a1d76a","#e9a3c9"))+
  theme_classic()+
  my_theme +
  theme(axis.text.y = element_text(face = "italic"))


f1.comp


f1<-plot_grid(f1.temp, plot_grid(f1.cond, f1.comp, nrow=2, labels=c("B", "C")), rel_widths = c(1.2, 0.8),labels=c("A"))
f1
#ggsave(plot=f1, path = "scripts/case_studies/",filename = "Fig1.png",width = 35, height = 20,units = "cm")
ggsave(plot=f1, path = "scripts/case_studies/",filename = "Fig1_DendroD.png",width = 35, height = 20,units = "cm")
#ggsave(plot=f1, path = "scripts/case_studies/",filename = "Fig1.svg",width = 35, height = 20,units = "cm")



#Figure 2 #####
f2a.temp<-GCRMN_CaseStudy_HC_reefs |> filter(domain_name=="SNAPPER ISLAND") |> 
  mutate(cond.range=case_when(report_year>2018 ~ "After", .default="Before")) |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year, color=cond.range), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper, color=cond.range), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2018, y = 0.25, xend = 2018, yend = 0.15),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#ED8B00FF",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("grey", "black"))+
  labs(x="Year", y="Hard Coral Cover (%)")+
  theme_classic()+  
  my_theme+
  theme(legend.position = "none")

f2a.temp

df2b<-scores |> 
  filter(Level=="reef", Year==2018,Name=="Snapper North", Reference=="Combined", Depth=="shallow slope") |> 
  mutate(Classification=case_when(
    Lower > 0.5 ~ "Above",
    Upper < 0.5 ~ "Below",
    is.na(Lower) ~ NA,
    .default="Within"))

Class.df2b<-#Cond.Class(df2b |> 
             Cond.Class.D(df2b |>
                         filter(Reference==ref) |>
                         mutate(across(c(Median, Lower, Upper), ~case_when(is.na(.x) ~ 0.5, .default=.x))),
                       conf=conf_thsld)

f2b.cond<-radial.plot.summary( df2b, ref)+
  scale_fill_class.c()+
  scale_x_discrete(labels = label_wrap_gen(width = 15)) + # Wrap x-axis labels at 10 characters
  labs(title =  Class.df2b|> pull(Class) )+
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
          # maxwidth = unit(120, "pt"),
          margin = unit(c(0, 0, 0, 0), "pt"),  # Margin around the textbox
          halign = 0.5,        # Horizontal alignment of the text
          hjust = 1
        )
  )
f2b.cond


f2c.temp<-GCRMN_CaseStudy_HC_reefs |> filter(domain_name=="LADY MUSGRAVE ISLAND") |> 
  mutate(cond.range=case_when(report_year>2012 ~ "After", .default="Before")) |> 
  ggplot()+
  geom_line(aes(y=median,x=report_year, color=cond.range), linewidth=1)+
  geom_pointrange(aes(y=median,x=report_year, ymin=lower, ymax=upper, color=cond.range), size=1, linewidth=0.2, linetype="dashed")+
  geom_segment(aes(x = 2012, y = 0.25, xend = 2012, yend = 0.15),
               lineend = "butt", 
               linejoin = "mitre",
               linewidth = 5, 
               colour = "#EC7014",
               arrow = arrow(length = unit(0.5, "cm")))+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("grey", "black"))+
  labs(x="Year", y="Hard Coral Cover (%)")+
  theme_classic()+  
  my_theme+
  theme(legend.position = "none")

f2c.temp


df2d<-scores |> filter(Level=="reef", Year==2012,Name=="Lady Musgrave Island", Reference==ref) |> 
  mutate(Lower=ifelse(is.na(Lower),0.5, Lower ),
         Upper=ifelse(is.na(Upper),0.5, Upper),
         Median=ifelse(is.na(Median),0.5, Median ),
         Classification=case_when(
           Lower > 0.5 ~ "Above",
           Upper < 0.5 ~ "Below",
           is.na(Lower) ~ NA,
           .default="Within"), Shelf="Inshore", Depth="Deep slope")

Class.df2d<-#Cond.Class(df2d |> 
             Cond.Class.D(df2d |>
                         filter(Reference==ref) |>
                         mutate(across(c(Median, Lower, Upper), ~case_when(is.na(.x) ~ 0.5, .default=.x))),
                       conf=conf_thsld)

f2d.cond<-radial.plot.summary( df2d, ref)+
  scale_fill_class.c()+
  scale_x_discrete(labels = label_wrap_gen(width = 15)) + # Wrap x-axis labels at 10 characters
  labs(title =  Class.df2d|> pull(Class) )+
  theme(
    axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y= element_blank(),
    plot.title = element_textbox_simple(
      fill = "#0F85A0FF", # Background color of the textbox
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

f2d.cond


f2<-plot_grid(f2a.temp, f2b.cond, f2c.temp, f2d.cond, nrow=2, ncol=2, labels=c("A","B","C","D"))
f2             
#ggsave(plot=f2, path = "scripts/case_studies/",filename = "Fig2.png",width = 35, height = 25,units = "cm")
ggsave(plot=f2, path = "scripts/case_studies/",filename = "Fig2_DendroD.png",width = 35, height = 25,units = "cm")
#ggsave(plot=f2, path = "scripts/case_studies/",filename = "Fig2.svg",width = 35, height = 25,units = "cm",device = "svg")
