server <- function(session, input, output) {
  set.seed(122)
  observe({
    cat("Selection:", input$value_select, "\n")

    if (input$region_select=="reef"){
      r<-get_reef(input$value_select)
    }else{
      r<-get_reefs(selVal = input$value_select)
    }
    cat(input$value_select)
    
    if(is.null(input$value_select)){
      i.df=NULL
      dat=NULL
    }else{

      i.df<-scores%>%filter(
        Name==input$value_select)
      i.df.r<-scores%>%filter(
        Name %in% r$Name)
      
      if (input$region_select=="reef"){
        r.shelf=i.df%>%select(Shelf)%>%unique()%>%pull(Shelf)
        updateTextInput(session = session, inputId = "shelf",value = r.shelf)
      }
      i.df<-i.df%>%filter(Shelf %in% input$shelf)
      
      if(input$shelf=="All"){s=c("Inshore","Offshore")}else{s=input$shelf}
      i.df.r<-i.df.r%>%filter(Shelf %in% s)
      
      
      dat<-i.df%>%filter(
        Year==input$report_year)
      if (dim(dat)[1]>0){
        dat<-dat%>%
          mutate(Classification=case_when(
            Lower > 0.5 ~ "Above",
            Upper < 0.5 ~ "Below",
            is.na(Lower) ~ NA,
            .default="Within"))
      }
      
      
      if(dim(i.df)[1]==0){
        dat.temp<-data.frame()
      }else{
       
        if (input$region_select == "reef"){
          dat.temp<-i.df%>%
            mutate(Classification=case_when(
              Lower > 0.5 ~ "Above",
              Upper < 0.5 ~ "Below",
              is.na(Lower) ~ NA,
              .default="Within"))
          
        }else{
          dat.temp<-i.df%>%
            filter(Shelf==input$shelf)%>%
            mutate(Classification=case_when(
              Lower > 0.5 ~ "Above",
              Upper < 0.5 ~ "Below",
              is.na(Lower) ~ NA,
              .default="Within"))
        }
 
      }
      
      this.region<-regions%>%
        filter(Name==input$value_select)%>%
        st_make_valid()%>%
        st_transform(4326)
      
      
    }
    
    
    
    output$reactive_condition=renderText({
      if (dim(dat)[1]==0){
        add.txt= ";display:inline-block;border:none;border-radius:16px;color:black;padding:10px 20px;display: nline-block;margin:4px 2px"
        tc<-paste0("<button class='pill-btn' style='background-color:","#C8C8C8", add.txt, "'>", "No data", "</button>")
        
      }else{
        p<-data.frame(Class=c("Insufficient data","Critical", "Warning II","Warning I", "Watch", "Good"),
                      col=c("#F0F0F0","#EF3A14" ,"#F3841C", "#EFC214",'#8ace7e', "#309143"))
        tc<-dat%>%
          filter(Reference=="Baseline")%>%
          ungroup%>%
          Cond.Class()%>%
          select(Class)%>%
          left_join(p)
        '<a style="text-decoration:none;cursor:default;color:#FFFFFF;"'
        add.txt= ";display:inline-block;border:none;border-radius:16px;color:black;padding:10px 20px;display: nline-block;margin:4px 2px"
        tc<-paste0("<button class='pill-btn' style='background-color:",tc$col, add.txt, "'>", tc$Class, "</button>")
      }
    })
    
    
    output$clean_region_reactive=renderText({
      if(dim(i.df)[1]==0){
        name_text=paste0('<b>',input$region_select,': </b>',input$value_select)
      }else{
        name_text<-dat%>%
          select(Level,Name)%>%
          unique()%>%
          mutate(Level=case_when(
            Level=="GBRMPA.MA" ~ "Management Area",
            .default=Level),
            Name=case_when(
              Level=="Management Area" ~ str_remove_all(Name, " Management Area"),
              .default = Name),
            clean_name=paste0('<b>',Level,': </b>',Name))%>%
          ungroup()%>%
          select(clean_name)%>%
          unique()%>%as.character()
        name_text
      }
    })
    output$clean_date_reactive=renderText({
      year_text<-paste('<b>Report Year:</b>',input$report_year)
    })
    output$region.map<-renderLeaflet({
      map.reefs(sf.frame = i.df.r, reef=r, b=this.region, y=input$report_year)
    })
    output$region.radial.plot<-renderPlot({
      if (input$region_select!="reef"){
        ref="Baseline"
        radial.plot.summary(dat = dat,ref = ref)
      }else{
        ref=ifelse(input$reference,"Baseline","Critical")
        radial.plot.summary(dat = dat,ref = ref)
      }
    })
    output$details<-renderPlot({
      if(input$detail=="Trends"){p=ind.temp.summary(dat.temp)}
      if(input$detail=="Composition"){p=comp_plot(comp, i.df.r, y=input$report_year, s=input$shelf)}
      if(input$detail=="Proportions"){p=P.Cond(i.df.r, y=input$report_year, s=input$shelf)}
      p
    })

    output$synopsis<-renderText({
      if(input$region_select=="reef"){
        sum.reef.tx(i.df, input$report_year)[[1]]
      }else{
        sum.tx.r(i.df, input$report_year)[[1]]
      }
      
    })
    output$synopsis.note<-renderText({
      if(input$region_select=="reef"){
        sum.reef.tx(i.df, input$report_year)[[2]]
      }else{
        sum.tx.r(i.df, input$report_year)[[2]]
      }
    })
    
  })
  
  # update region selections
  observeEvent(input$region_select, {
    # if (input$region_select=="GBR") {
    #   updatePickerInput(session = session, inputId = "region_select", 
    #                     choices = "GBR", selected = "GBR")
    # }
    
    if (input$region_select =="GBR") {
      updatePickerInput(session = session, inputId = "value_select", 
                        choices = "GBRMP", 
                        selected = "GBRMP")
      
    }
    
    if (input$region_select =="GBRMPA.MA") {
      this.names=as.character(
        (regions%>%
           filter(Region==input$region_select)%>%
           select(Name)%>%
           st_drop_geometry())$Name
      )
      updatePickerInput(session = session, inputId = "value_select", 
                        choices = this.names, 
                        selected = "Townsville/Whitsunday Management")
      
    }
    
    if (input$region_select =="TUMRA") {
      this.names=as.character(
        (regions%>%
           filter(Region==input$region_select)%>%
           select(Name)%>%
           st_drop_geometry())$Name%>%
          unique
      )
      updatePickerInput(session = session, inputId = "value_select", 
                        choices = this.names, 
                        selected = "Girringun")
      
    }
    
    if (input$region_select =="ZONE") {
      this.names=as.character(
        (regions%>%
           filter(Region==input$region_select)%>%
           select(Name)%>%
           st_drop_geometry())$Name
      )
      updatePickerInput(session = session, inputId = "value_select", 
                        choices = this.names, 
                        selected = "Central")
      
    }
    
    if (input$region_select =="NRM") {
      this.names=as.character(
        (regions%>%
           filter(Region==input$region_select)%>%
           select(Name)%>%
           st_drop_geometry())$Name
      )
      updatePickerInput(session = session, inputId = "value_select", 
                        choices = this.names, 
                        selected = "Fitzroy")
      
    }
    
    
    if (input$region_select=="reef") {
      this.names=as.character(
        (reefs%>%
           select(Name)%>%
           st_drop_geometry())$Name
      )
      updatePickerInput(session = session, inputId = "value_select",
                        choices =this.names,
                        selected = "Pandora (deep slope)")
      
    }
    
  }, ignoreInit = TRUE)
  
  
}
