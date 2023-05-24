library(tidyverse)
library(ggplot2)
library(readr)

df.LOF<-read_csv("data/community/allGBR_LOF.csv")
reef.CCI%>%
  ungroup()%>%
  filter(k==6)%>%
  tidyr::separate(Reef,sep = "_", into=c("Reef","Depth.str"))%>%
  rename("Name"="Reef","Score"=LOF)%>%
  mutate(Indicator="Community Composition", indicator="community.composition.k6", 
         .lower=NA, .upper=NA, .width=0.95, .point="mean", .interval="hdci",
         Depth=case_when(
           Depth.str == NA ~ "Deep",
           Depth.str == "5m" ~ "Deep",
           Depth.str == "2m" ~ "Shallow",
           TRUE ~ "Deep"
         ),
         Classification=case_when(
           Score > 0.5 ~ "Good",
           Score > 0.4 & Score <= 0.5 ~ "Moderate",
           Score <= 0.4 & Score > 0.2 ~ "Poor",
           # Score < 0.2 ~ "Very Poor",
           TRUE ~ "Very Poor")
  )%>%
  select(Name,Year,Depth, Indicator, indicator, Score, .lower,.upper,.width, .point, .interval, Classification)


load("data/coral/reef.index.summary.RData")

df.cc<-reef.index.summary



reef.name<-"Reef 21-529"
pool.k=6

Comb.plot <- function(df.cc, df.LOF, reef.name, pool.k=6, depth="Deep") {
  cc<-df.cc%>%
    filter(REEF==reef.name)
  
  lof<-df.LOF%>%
    tidyr::separate(Reef,sep = "_", into=c("Reef","Depth"))%>%
    mutate(Depth=if_else(is.na(Depth),"Deep",Depth))%>%
    mutate(Depth=case_when(Depth == "2m" ~ "Shallow",
                           Depth == "5m" ~ "Deep",
                           TRUE ~ "Deep"))%>%
    filter(Reef==reef.name, k==pool.k, Depth==depth)
  
  Dissimilarity.outplot <- 
    ggplot() +
    # geom_ribbon(data=cc,
    #             aes(x=fYEAR, ymin=reef.year.value.025, ymax=reef.year.value.975), fill='light grey') +
    geom_line(data=cc,
              aes(x=fYEAR, y=mean.reef.year.value), color='black', size=1,
              linetype="dashed") +
    geom_line(data=lof,
              aes(x=Year, y=LOF), color='dodgerblue3', size=2) +
    geom_hline(yintercept=0.5, linetype=2) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(name='Coral community indicator index',limits = c(0,1),
                       sec.axis=sec_axis(~.*100, name='Coral cover')) +
    theme_bw() +
    theme(panel.border=element_rect(color='black',fill=NA, size=1),
          # axis.title.x=element_blank(),
          # axis.title.y=element_blank(),
          plot.title=element_text(hjust=0),
          axis.title = element_text(size = 14, face="bold"),
          axis.text= element_text(size = 12) 
          
          ) +
    xlab('Year') +
    ylab('Coral community indicator index')
  
  Dissimilarity.outplot
  # NMDS.outplot <- ggplot(lof) +
  #   geom_segment(aes(x=unique(lof[,c('Year','NMDS1','NMDS2')])[1L,'NMDS1'], 
  #                    xend=((unique(lof[,c('Year','NMDS1','NMDS2')])[1L,'NMDS1']) +
  #                            (unique(lof[,c('Year','NMDS1','NMDS2')])[2L,'NMDS1']))/2,
  #                    y=unique(lof[,c('Year','NMDS1','NMDS2')])[1L,'NMDS2'],
  #                    yend=((unique(lof[,c('Year','NMDS1','NMDS2')])[1L,'NMDS2']) +
  #                            (unique(lof[,c('Year','NMDS1','NMDS2')])[2L,'NMDS2']))/2),
  #                arrow=arrow(type='closed', length=unit(5,'mm'))
  #                ) +
  #   geom_path(data=unique(df[df$Reef==reef.name,
  #                            c('Year','NMDS1','NMDS2')]),
  #             aes(x=NMDS1, y=NMDS2)) +
  #   geom_point(data=unique(df[df$Reef==reef.name,
  #                             c('Year','NMDS1','NMDS2')]),
  #              aes(x=NMDS1, y=NMDS2)) +
  #   geom_point(data=df[df$Reef==reef.name &
  #                        df$k==3 &
  #                        df$threshold_detection=='Yes',],
  #              aes(x=NMDS1, y=NMDS2), color='coral', size=5) +
  #   geom_point(data=df[df$Reef==reef.name &
  #                        df$k==6 &
  #                        df$threshold_detection=='Yes',],
  #              aes(x=NMDS1, y=NMDS2), color='dodgerblue3', size=3) +
  #   geom_text_repel(data=unique(df[df$Reef==reef.name,
  #                                  c('Year','NMDS1','NMDS2')]),
  #                   aes(x=NMDS1, y=NMDS2, label=Year)) +
  #   theme_Publication() +
  #   theme(panel.border=element_rect(color='black',fill=NA, size=1),
  #         # axis.title.x=element_blank(),
  #         # axis.title.y=element_blank(),
  #         plot.title=element_text(hjust=0)) +
  #   xlab('NMDS1') +
  #   ylab('NMDS2')
  # 
  # outplot <- (Dissimilarity.outplot | NMDS.outplot)
  # 
  # outplot <- outplot +
  #   plot_annotation(title=reef.name) &
  #   theme(text=element_text(size=15, face='bold'))
  # 
  # plot(outplot)
  # 
  # ggsave(filename=paste0('/Users/uqskim14/Analyses/AIMS community index/Figure/Mackay_Whitsundays/',
  #                        reef.name, '.png'),
  #        outplot, height=150, width=300,
  #        units='mm', device='png', dpi=300)
  
  return(Dissimilarity.outplot)
}
