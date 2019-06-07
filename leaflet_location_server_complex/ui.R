#library(shinythemes)
#https://bootswatch.com/yeti/

ui <- navbarPage(title = "GozdIS",
                   # div(
                   #          div(
                   #            id = "logo-id",
                   #            img(src = "logo.png")
                   #          ),
                   #          "GozdIS"
                   #        )
                 #,
                 id="navbar", collapsible = T, fluid = T, #theme = shinytheme("simplex"),
                 theme = "styles.css",
                 #shinythemes::themeSelector(),
                 tabPanel("Explore",
                          div(class="outer",
                            fluidRow(
                              #div(class="leaflet_explore",
                              column(width= 12, offset = 0,
                                     leafletOutput("mymap", width="100%", height = "85vh"),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                                     draggable = TRUE, top = 25, left = 65, right = NULL, bottom = "auto",
                                                     width = 270, height = "auto",
                                                     h2("Data explorer"),
                                                     checkboxInput("big_squares", "choose square", value = FALSE),
                                                     selectInput("slct_location", "Select Location", multiple = TRUE,
                                                                 choices = loc_geom_all$common_name),
                                                     selectInput("slct_datatype", "Select Datatype", multiple = TRUE,
                                                                 choices = c("photo","samples","sensors","survey")),
                                                     actionButton("infoBtn", "Fetch info")
                                       
                                     )
                              )
                            #)
                            ),
                            
                            fluidRow(
                              div(class="filters",
                                  uiOutput("filters_wellpanel")
                              )
                            ),
                            
                            # fluidRow(
                            #   div(class="filters",
                            #       uiOutput("test")
                            #   )
                            # ),
                            
                            fluidRow(
                              div(class="avail_table",
                                  #h2("Data availability information"),
                                      DT::dataTableOutput("info_table")
                                  )
                              )
                          
                  )
                 ),
                 navbarMenu("alphanum data",
                            tabPanel("Get data"
                                     
                            ),
                            tabPanel("Aggregation"
                                     
                            )
                          ),
                 tabPanel("images"
                          
                          ),
                 navbarMenu("Geospatial",
                            tabPanel("shapefiles",
                              leafletOutput("mymap2", width="100%", height = "93vh")
                            )
                          )
)
