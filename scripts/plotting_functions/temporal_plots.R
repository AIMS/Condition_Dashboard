scale_color_class <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(c("#EF3A14" , "#EFC214", "#56B703"), 
                      c( "Below","Within", "Above")),
    drop=F,
    ...
  )
}

ind.temp.summary<-function(reef.df){
  require(tidyverse)
  require(plotly)
  require(stringr)
  
  if(dim(reef.df)[1]==0){ #Display "no data" box if the reef/region are not surveyed
    dat=data.frame(x=0.5,y=0.5, lab="No Survey\ndata available")
    p=ggplot(dat, aes(x,y,label=lab))+geom_text(size=14, color="grey55")+
      theme_void()+
      theme(
        legend.background = element_rect_round(radius = unit(0.2, "snpc")),
        legend.key = element_rect_round(radius = unit(0.4, "snpc")),
        panel.background = element_rect_round(radius = unit(1, "cm"), fill = "grey97",color = "grey78"),
        strip.background = element_rect_round(radius = unit(8, "pt")),
        plot.background  = element_rect_round(fill = "grey97")
      )
    
  }else{
  p<-reef.df%>%
    mutate(Indicator=recode(Indicator, 
                            Coral.cover="Coral cover",
                            Macroalgae="Macroalgae cover",
                            Performance="Recovery performance",
                            Juvenile="Juvenile abundance",
                            Composition="Community composition"),
           Indicator=str_wrap(Indicator,width = 8,whitespace_only = T))%>%
    filter(Depth %in% c(NA,"deep slope"),
           Reference=="Baseline")%>%
    ggplot(aes(group=Indicator))+
    geom_point(aes(x=Year, y=Median, color=Indicator), size=3)+
    geom_line(aes(x=Year, y=Median, color=Indicator), linetype="dashed")+
    geom_hline(aes(yintercept=0.5), linetype="dashed")+
    # scale_shape_manual(values=c(15,16,17,22,25))+
    # scale_shape_manual(values = unique(reef.df$Indicator)) +
    # scale_color_class()+
    ylim(0,1)+
    # guides(color = FALSE)+
    theme_bw()+
    labs(shape=NULL)+
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.key.height=unit(1, "cm"),
          panel.grid = element_blank()
          )
  
  
  }
  
  
  # fig<-plot_ly(reef.df, 
  #         x = ~Year,
  #         y = ~Score,
  #         color =  ~Indicator,
  #         type = 'scatter', 
  #         mode = 'lines+markers',
  #         marker= list(
  #           size = 15,
  #           line= list(
  #             width=1
  #           )) 
  # )%>%
  #   layout(shapes=list(
  #     list(
  #       type = "line",
  #       x0 = 0,
  #       x1 = 1,
  #       xref = "paper",
  #       y0 = 0.5,
  #       y1 = 0.5,
  #       line = list(color = "black")
  #     )
  #   ))
  # 
  # fig<- fig %>%
  #   add_segments(
  #     x = 0,
  #     xend=1,
  #     y = 0.5,
  #     yend=0.5
  #   )
  # 
  # # Plot an additional line on the chart for the monthly mean
  # figure = ct.chart.line(
  #   monthly_mean,
  #   fig=figure,
  #   error_y=monthly_std,
  #   scatter_kwargs={
  #     'name':'Monthly'
  #   }
  # )
  # 
  # 
  # ggplotly(p)
  p
}