



### SHINY UI ###
ui <- bootstrapPage(
  # tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Reef Condition</a>'), id="nav",
             windowTitle = "Reef Condition",
             
             tabPanel("Overview",
                      div(class="outer",
                          tags$head(includeCSS("styles.css"),## Added to define the HTML styles
                                    includeScript(path = "app.js")),##added for the collapsible nature to work
                          leafletOutput("region.map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        # span(tags$i(h6("Estimated condition of coral reef habitats based monitored reefs in the Great Barrier Reef.")), style="color:#045a8d"),
                                        fluidRow(
                                          column(width=6,offset = 1,fluidRow(
                                            h6(htmlOutput("clean_region_reactive"), align = "left"),
                                            h6(htmlOutput("clean_date_reactive"), align = "left")
                                          )),
                                          column(width=2,h6(htmlOutput("reactive_condition"), align = "left",.noWS = "outside"))
                                        ),
                                        conditionalPanel('input.region_select=="reef"',
                                                         prettyToggle(
                                                           inputId = "reference",
                                                           label_on = "Baseline Reference",
                                                           label_off = "Critical Reference",
                                                           value=T,
                                                           animation="jelly"
                                                         )
                                        ),
                                        plotOutput("region.radial.plot", height="350px", width="100%"),
                                        # span(tags$i(h6("Error bars represent the 90% credible intervals")), style="color:#045a8d"),
                                        sliderInput("report_year", "Select Report year",
                                                    min=2007, max=2023, value=2022),
                                        # animate=animationOptions(interval = 3000, loop = FALSE)),
                                        
                                        
                                        
                                        
                                        conditionalPanel('input.region_select!="reef"',
                                                         awesomeRadio(inputId = "shelf",
                                                                              label= "Shelf",
                                                                              choices = c("All","Inshore","Offshore"),
                                                                              selected = "Inshore",
                                                                      checkbox = FALSE,
                                                                              inline = T)
                                        ),
                                        awesomeRadio(inputId = "detail",
                                                     label= "Detail plots",
                                                     choices = c("Trends","Composition","Proportions"),
                                                     selected = "Trends",
                                                     checkbox = FALSE,
                                                     inline = T),
                                        pickerInput("region_select", "Region:",   
                                                    choices = c("GBR","GBRMPA.MA", "ZONE", "NRM", "TUMRA", "reef"),
                                                    selected = "ZONE",
                                                    multiple = FALSE),
                                        
                                        pickerInput("value_select", "Name:",
                                                    choices = c("Central",
                                                                "Southern",
                                                                "Northern" ),
                                                    # choices = "GBRMPA",
                                                    selected = "Central",
                                                    options = list(`actions-box` = TRUE), 
                                                    # `none-selected-text` = "Townsville/Whitsunday Management Area"),
                                                    multiple = FALSE)
                          ),
                          absolutePanel(id = "detail.plot", class = "panel panel-default",
                                        top = 75, left = 500, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        HTML('<button data-toggle="collapse" data-target="#demo">Details</button>'),
                                        tags$div(id = 'demo',  class="collapse",
                                                 plotOutput("details",height="250px", width="100%")
                                        )),
                          
                          absolutePanel(id = "synopsis", class = "panel panel-default",
                                        top = 75, left = 950, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        # HTML('<button data-toggle="collapse" data-target="#demo">Synopsis</button>'),
                                        # tags$div(id = 'synopsis',  class="collapse",
                                        h3("Synopsis", style="color:#7393B3;font-weight: bold;text-align:center"),
                                        h6(htmlOutput("synopsis"), align = "left"),
                                        span(tags$i(h6(htmlOutput("synopsis.note")), style="color:#71797E;font-size:0.3em;text-align:center"))
                                        
                          )
                          
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                          #               tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          # 
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                          #               actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                          #                            onclick = sprintf("window.open('%s')", 
                          #                                              "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          # 
                          
                      )
             )
             # ),
             # tabPanel("Reef Explorer",
             #          div(class="outer",
             #              tags$head(includeCSS("scripts/app/styles.css"),## Added to define the HTML styles
             #                        includeScript(path = "scripts/app/app.js")),##added for the collapsible nature to work
             #              leafletOutput("region.map", width="100%", height="100%"),
             #              
             #              absolutePanel(id = "controls", class = "panel panel-default",
             #                            top = 75, left = 55, width = 400, fixed=TRUE,
             #                            draggable = TRUE, height = "auto",
             #                            
             #                            sliderInput("report_year", label = h5("Select Report year"),
             #                                        min=2000, max=2021, value=2021),
             #                                        # animate=animationOptions(interval = 3000, loop = FALSE)),
             #                            
             #                            pickerInput("region_select", "Level:",   
             #                                        choices = c("GBR","GBRMPA.MA", "NRM", "TUMRA", "Reef"),
             #                                        selected = "GBRMPA.MA",
             #                                        multiple = FALSE),
             #                            
             #                            pickerInput("value_select", "Name:",
             #                                        choices = c("Mackay/Capricorn Management Area",
             #                                                    "Townsville/Whitsunday Management Area",
             #                                                    "Cairns/Cooktown Management Area",
             #                                                    "Far Northern Management Area" ),
             #                                        # choices = "GBRMPA",
             #                                        selected = "Townsville/Whitsunday Management Area",
             #                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
             #                                        multiple = FALSE)
             #              )
             #          )
             # )
  )
)

# 
# tabPanel("Region plots",
#          
#          sidebarLayout(
#            sidebarPanel(
#              
#              span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
#              span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
#              
#              pickerInput("level_select", "Level:",   
#                          choices = c("Global", "Continent", "Country", "US state"), 
#                          selected = c("Country"),
#                          multiple = FALSE),
#              
#              pickerInput("region_select", "Country/Region:",   
#                          choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
#                          options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
#                          selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
#                          multiple = TRUE), 
#              
#              pickerInput("outcome_select", "Outcome:",   
#                          choices = c("Deaths per million", "Cases per million", "Cases (total)", "Deaths (total)"), 
#                          selected = c("Deaths per million"),
#                          multiple = FALSE),
#              
#              pickerInput("start_date", "Plotting start date:",   
#                          choices = c("Date", "Week of 100th confirmed case", "Week of 10th death"), 
#                          options = list(`actions-box` = TRUE),
#                          selected = "Date",
#                          multiple = FALSE), 
#              
#              sliderInput("minimum_date",
#                          "Minimum date:",
#                          min = as.Date(cv_min_date,"%Y-%m-%d"),
#                          max = as.Date(current_date,"%Y-%m-%d"),
#                          value=as.Date(cv_min_date),
#                          timeFormat="%d %b"),
#              
#              "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
#            ),
#            
#            mainPanel(
#              tabsetPanel(
#                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
#                tabPanel("New", plotlyOutput("country_plot")),
#                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
#              )
#            )
#          )
# ),
# 
# tabPanel("SARS mapper",
#          div(class="outer",
#              tags$head(includeCSS("styles.css")),
#              leafletOutput("sars_map", width="100%", height="100%"),
#              
#              absolutePanel(id = "controls", class = "panel panel-default",
#                            top = 75, left = 55, width = 250, fixed=TRUE,
#                            draggable = TRUE, height = "auto",
#                            
#                            h3(textOutput("sars_reactive_case_count"), align = "right"),
#                            h4(textOutput("sars_reactive_death_count"), align = "right"),
#                            h6(textOutput("sars_clean_date_reactive"), align = "right"),
#                            h6(textOutput("sars_reactive_country_count"), align = "right"),
#                            plotOutput("sars_epi_curve", height="130px", width="100%"),
#                            plotOutput("sars_cumulative_plot", height="130px", width="100%"),
#                            span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%"),#tags$br(),
#                            span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
#                            
#                            sliderTextInput("sars_plot_date",
#                                            label = h5("Select mapping date"),
#                                            choices = format(unique(sars_cases$date), "%d %b %y"),
#                                            selected = format(sars_max_date, "%d %b %y"),
#                                            grid = FALSE,
#                                            animate=animationOptions(interval = 3000, loop = FALSE))
#              ),
#              
#              absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
#                            tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
#              
#              absolutePanel(id = "logo", class = "card", bottom = 15, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
#                            actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
#                                         onclick = sprintf("window.open('%s')", 
#                                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
#          )
# ),
# 
# tabPanel("Outbreak comparisons",
#          
#          sidebarLayout(
#            sidebarPanel(
#              radioButtons("comparison_metric", h3("Select comparison:"),
#                           c("Cases" = "cases",
#                             "Deaths" = "deaths",
#                             "Countries/regions affected" = "countries")),
#              textOutput("epi_notes_1"),
#              textOutput("epi_notes_2") 
#            ),
#            
#            mainPanel(plotlyOutput("comparison_plot"), width = 6)
#          )
# ),
# 
# tabPanel("Data",
#          numericInput("maxrows", "Rows to show", 25),
#          verbatimTextOutput("rawtable"),
#          downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
#          "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
#                                                             "Johns Hopkins Center for Systems Science and Engineering.")
# ),
# 
# tabPanel("About this site",
#          tags$div(
#            tags$h4("Last update"), 
#            h6(paste0(update)),
#            "This site is updated once daily. There are several other excellent COVID mapping tools available, including those run by", 
#            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "the WHO,"),
#            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University,"),"and",
#            tags$a(href="https://ourworldindata.org/coronavirus-data-explorer?zoomToSelection=true&time=2020-03-01..latest&country=IND~USA~GBR~CAN~DEU~FRA&region=World&casesMetric=true&interval=smoothed&perCapita=true&smoothing=7&pickerMetric=total_cases&pickerSort=desc", "Our World in Data."),
#            "Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
#            
#            tags$br(),tags$br(),tags$h4("Background"), 
#            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
#            These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
#            The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
#            This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
#            tags$br(),tags$br(),
#            "In isolation, these headlines can be hard to interpret. 
#            How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
#            This site is updated daily based on data published by Johns Hopkins University. 
#            By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
#            tags$br(),tags$br(),
#            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
#            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
#            tags$br(),tags$br(),tags$h4("Code"),
#            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
#            tags$br(),tags$br(),tags$h4("Sources"),
#            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
#            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
#            tags$b("US state-level case data: "), tags$a(href="https://github.com/nytimes/covid-19-data", "New York Times github page,"),
#            tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
#            tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
#            tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
#            tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
#            tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
#            substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
#            The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
#            tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
#            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),
#            tags$br(),tags$br(),tags$h4("Authors"),
#            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
#            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
#            tags$br(),tags$br(),tags$h4("Contact"),
#            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
#            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
#          )
