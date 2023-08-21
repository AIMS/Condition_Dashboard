

scale_fill_class <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#EF3A14" ,"#F3841C", "#EFC214",'#8ace7e', "#309143","#808080"), 
                      c("Critical", "Warning II","Warning I", "Watch", "Good", "Insuficient data")),
    drop=F,
    ...
  )
}


P.Cond<-function(i.df.r, y, s){
  
 require(ggplot2)
  if(s=="All"){s=c("Inshore","Offshore")}
  dat = i.df.r%>%
    filter(Year==y, Shelf %in% s, Reference=="Baseline"
           )%>%
    Cond.Class()%>%
    group_by(Class)%>%
    count() %>% 
    ungroup()%>% 
    arrange(desc(Class)) %>%
    mutate(percentage = round(n/sum(n),digits=2)*100,
           lab.pos = cumsum(percentage)-.5*percentage,
           label= paste0(percentage, "\n n= ", n))%>%
    filter(!is.na(Class))
  
  # Make the plot
  p<-ggplot(data = dat, 
         aes(x = 2, y = percentage, fill = Class))+
    geom_bar(stat = "identity")+
    coord_polar("y", start = 200) +
    geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "white") +
    theme_void() +
    scale_fill_class()+
    xlim(.2,2.5)+
    theme(legend.position = "bottom",
                       legend.title = element_blank())+
    labs(title="Proportion of reefs per classification")
  
  p
  
}


