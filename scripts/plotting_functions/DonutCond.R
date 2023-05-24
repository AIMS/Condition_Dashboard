# load library
library(ggplot2)
library(sf)

P.Cond<-function(dat){
  
  dat<-dat%>%
    st_drop_geometry()%>%
    group_by(Class)%>%
    summarise(n=n())%>%
    arrange(by=Class)%>%
    mutate(freq=round(n / sum(n), 1),
           ymax=cumsum(freq), # Compute the cumulative percentages (top of each rectangle)
           ymin=c(0, head(ymax, n=-1)),# Compute the bottom of each rectangle
           labelPosition=((ymax + ymin) / 2),# Compute label position
           lab.pos = ymax-.5*freq,
           label= paste0(Class, "\n n= ", n),
           Class=factor(Class, levels= c("Critical", "WarningII","WarningI", "Watch", "Good"))
    )
    
  
  dat = r %>% 
    st_drop_geometry()%>%
    # filter(Surveyed)%>%
    group_by(Class)%>%
    count() %>% 
    ungroup()%>% 
    arrange(desc(Class)) %>%
    mutate(percentage = round(n/sum(n),4)*100,
           lab.pos = cumsum(percentage)-.5*percentage,
           label= paste0(percentage, "\n n= ", n),
  Class=factor(Class, levels= c("Critical", "WarningII","WarningI", "Watch", "Good")))%>%
    filter(!is.na(Class))
  # arrange(Class)
  
  cond.palette = c("#EF3A14" ,"#F3841C", "#EFC214",'#8ace7e', "#309143")
  
  # Make the plot
  ggplot(dat, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Class)) +
    geom_rect() +
    # geom_text( x=2, aes(y=labelPosition, label=label, color=Class), size=6) + # x here controls label position (inner / outer)
    scale_fill_manual(values=cond.palette) +
    scale_color_manual(values=cond.palette) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")
  
  p<-ggplot(data = dat, 
         aes(x = 2, y = percentage, fill = Class))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 200) +
    # geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
    theme_void() +
    scale_fill_manual(values = cond.palette)+
    xlim(.2,2.5)
  
  p<-ggplotly(p)
  
}
# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

