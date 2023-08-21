
comp_plot<-function(comp, i.df.r, y, s){
  if(s=="All"){s=c("Inshore","Offshore")}
  comp.d<-comp%>%
    filter(k==6,REPORT_YEAR==y, REEF %in% (i.df.r%>%filter(Year==y, Indicator=="Community.composition", Reference=="Baseline",Median<0.5, Shelf %in% s)%>%pull(Name)))%>%
    group_by(COMP_2021_DESCRIPTION)%>%
    summarise(diff=median(meanDiff), se=ifelse(is.na(sd(meanDiff)/sqrt(length(meanDiff))), 0,sd(meanDiff)/sqrt(length(meanDiff)))) %>%
    mutate(taxa=factor(COMP_2021_DESCRIPTION, levels=COMP_2021_DESCRIPTION[order(diff)]),
           c=case_when(diff<0 ~ "Loss",
                       .default="Gain"),
           lower=case_when(c=="Loss" ~ diff+se,
                           .default=diff-se),
           upper=case_when(c=="Loss" ~ diff-se,
                           .default=diff+se)
    )%>%
    arrange(-abs(diff))%>%
    head()%>%
    droplevels()
  
  nreefs=length(i.df.r%>%filter(Year==y, Indicator=="Community.composition", Reference=="Baseline",Median<0.5, Shelf %in% s)%>%pull(Name))
  
  if (nreefs==0){
    dat=data.frame(x=0.5,y=0.5, lab="No\nsignficant changes\nobserved in the\n community composition")
    comp.d=ggplot(dat, aes(x,y,label=lab))+geom_text(size=10, color="grey55")+
      theme_void()+
      theme(
        legend.background = element_rect_round(radius = unit(0.2, "snpc")),
        legend.key = element_rect_round(radius = unit(0.4, "snpc")),
        panel.background = element_rect_round(radius = unit(1, "cm"), fill = "grey97",color = "grey78"),
        strip.background = element_rect_round(radius = unit(8, "pt")),
        plot.background  = element_rect_round(fill = "grey97")
      )
  }else{
    comp.d<-comp.d%>%
      ggplot()+
      geom_bar(aes(x=taxa, y=diff*100, fill=c), stat="identity")+
      geom_errorbar(aes(x=taxa,ymin=lower*100, ymax=upper*100), width=0.2)+
      scale_x_discrete(labels = function(x) 
        stringr::str_wrap(x, width = 10))+
      coord_flip()+
      labs(x="Top taxa that changed", y=sprintf("Change in cover from %s reefs\nwhere composition has changed\n(%%, mean +/- se)", nreefs))+
      scale_fill_manual(values=c("blue","red"))+
      theme_bw()+
      guides(fill="none")
  }
  comp.d
  
}
