library(stringr)
library(tidyverse)
require(RColorBrewer)
library(geomtextpath)

# scale_fill_class <- function(...){
#   ggplot2:::manual_scale(
#     'fill', 
#     values = setNames(c("#EF3A14" ,"#F3841C", "#EFC214", "#56B703"), 
#                       c("Very Poor", "Poor","Moderate", "Good")),
#     drop=F,
#     ...
#   )
# }

scale_fill_class.c <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#EF3A14" , "#EFC214", "#56B703"), 
                      c( "Below","Within", "Above")),
    drop=F,
    ...
  )
}
radial.plot.summary<-function(dat,ref){
  
  require(stringr)
  require(tidyverse)
  require(RColorBrewer)
  require(elementalist) # devtools::install_github("teunbrand/elementalist")
  
  if(dim(dat)[1]==0){ #Display "no data" box if the reef/region are not surveyed
    dat=data.frame(x=0.5,y=0.5, lab="No Survey\ndata available")
    p.eg=ggplot(dat, aes(x,y,label=lab))+geom_text(size=14, color="grey55")+
      theme_void()+
      theme(
        legend.background = element_rect_round(radius = unit(0.2, "snpc")),
        legend.key = element_rect_round(radius = unit(0.4, "snpc")),
        panel.background = element_rect_round(radius = unit(1, "cm"), fill = "grey97",color = "grey78"),
        strip.background = element_rect_round(radius = unit(8, "pt")),
        plot.background  = element_rect_round(fill = "grey97")
      )
    
  }else{
    # dat<-dat%>%
    #   group_by(Name,Year, Indicator, Classification)%>%
    #   summarise(Score=mean(Score, na.rm = T))%>%
    #   ungroup()%>%
    #   # mutate(Classification=case_when(
    #   #   Score > 0.5 ~ "Good",
    #   #   Score > 0.4 & Score <= 0.5 ~ "Moderate",
    #   #   Score <= 0.4 & Score > 0.2 ~ "Poor",
    #   #   # Score < 0.2 ~ "Very Poor",
    #   #   TRUE ~ "Very Poor")
    #   # )%>%
    #   mutate(Classification=factor(Classification, levels=c("Very Poor","Poor","Moderate","Good")))
    
    p.eg<-dat%>%
      ungroup%>%
      filter(Reference==ref)%>%
      mutate(Indicator=recode(Indicator, 
             Coral.cover="Coral cover",
             Macroalgae="Macroalgae cover",
             Recovery.performance="Recovery performance",
             Juvenile.density="Juvenile abundance",
             Community.composition="Community composition"))%>%
      # mutate(Indicator=str_replace(Indicator, "[.]", " "))%>%
      ggplot()+
      geom_col(
        aes(
          # x = str_wrap(Indicator, 5, whitespace_only = T),
          x=Indicator,
          y = Median,
          fill = Classification
        ),
        position = "dodge2",
        show.legend = TRUE,
        alpha = .9
      ) +
      geom_errorbar(
        aes(
          x=Indicator,
          ymin = Lower,
          ymax=Upper,
        ),
        position = "dodge2",
        show.legend = TRUE,
        alpha = .9,
        width=0.2
      )  +
      # Make custom panel grid
      geom_hline(
        aes(yintercept = seq(0,1,0.25)), 
        color = "gray87"
      ) + 
      geom_hline(
        aes(yintercept = 0.5), 
        color = "gray60",
        linewidth=0.75,
        linetype="dashed"
        
      ) + 
      geom_vline(xintercept = 1:6 - 0.5, color = "gray90") +
      # Add bars to represent the cumulative track lengths
      # str_wrap(region, 5) wraps the text so each line has at most 5 characters
      # (but it doesn't break long words!)

    # # Lollipop shaft for mean gain per region
    #   geom_segment(
    #     aes(
    #       x = str_wrap(Indicator, 5),
    #       y = 0,
    #       xend = str_wrap(Indicator, 5, whitespace_only = T),
    #       yend = 1
    #     ),
    #     linetype = "dashed",
    #     color = "gray87"
    #   ) + 
      
      # Make it circular!
      coord_curvedpolar()+
      theme_bw()+
      theme(panel.border = element_blank(),
            axis.text.x = element_text(size = 14),
            axis.title.x = element_blank(),
            panel.grid.major = element_blank())
      # coord_polar()
    
    
    ##Add Annotations and Legend
    
    p.eg <- p.eg +
      # Annotate custom scale inside plot
      # annotate(
      #   x = 0.5, 
      #   y = 1, 
      #   label = "1", 
      #   geom = "text", 
      #   color = "gray80", 
      #   family = "Bell MT"
      # ) +
      # annotate(
      #   x = 0.5, 
      #   y = 0.5, 
      #   label = "0.5", 
      #   geom = "text", 
      #   color = "gray80", 
      #   family = "Bell MT",
      #   fontface=2
      #   
      # ) +
      # annotate(
      #   x = 0.5,
      #   y =0.75,
      #   label = "0.75",
      #   geom = "text",
      #   color = "gray80",
      #   family = "Bell MT"
      # ) +
      # Scale y axis so bars don't start in the center
      scale_y_continuous(
        limits = c(-0.1, 1.1),
        expand = c(0, 0),
        breaks = c(0, 0.25, 0.5, 0.75, 1)
      ) + 
      # New fill and legend title for number of tracks per region
      scale_fill_class.c()+
      #   "Condition",
      #   values = brewer.pal(name="RdYlGn", n=3)
      # ) +
      # Make the guide for the fill discrete
      # guides(
      #   fill = guide_colorsteps(
      #     barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
      #   )
      # ) +
      theme(
        # Remove axis ticks and text
        axis.title = element_blank(),
        # axis.ticks = element_blank(),
        # axis.text.y = element_blank(),
        # Use gray text for the region names
        axis.text.x = element_text(color = "gray20", size = 12, ),
        # Move the legend to the bottom
        legend.position = 'none',
      )
    p.eg
    p.eg<- p.eg + 
      # Add labels
      # labs(
      #   title = sprintf("%s: %i", dat$Name[1], dat$Year[1])
      # )+
      theme(
        
        # Set default color and font family for the text
        text = element_text(color = "gray12", family = "Bell MT"),
        
        # Customize the text in the title, subtitle, and caption
        plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
        plot.subtitle = element_text(size = 14, hjust = 0.05),
        plot.caption = element_text(size = 10, hjust = .5),
        
        # Make the background white and remove extra grid lines
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }
  # Use `ggsave("plot.png", p.eg, width=9, height=12.6)` to save it as in the output
  p.eg
  
}

