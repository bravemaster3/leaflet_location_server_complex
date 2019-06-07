
server <- function(input,output, session){

  output$mymap2 <- output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("OpenStreetMap", group = "Topo") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
      addProviderTiles("Stamen.TerrainBackground", group = "Relief")%>%
      #addMouseCoordinates(style = "basic") %>%
      addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                     circleMarkerOptions = F, polygonOptions = F)
    #check first if there is any photo, sensors, samples... at all before adding them to the map to avoir any eror
     m <- m%>% 
      markers_simplifier(map = m, sp_df = loc_geom_all, group_name = "All Locations", fill_color = "#A5DF00")
     
     #groups_string <- as.vector("All Locations")
     groups_string <- as.vector(NULL)
     if (!is.null(loc_geom_photo)){
       m <- m%>%
         markers_simplifier(map = m, sp_df = loc_geom_photo, group_name = "Photo", fill_color = "#FFBF00")
       groups_string <- c(groups_string,"Photo")
       }
     if (!is.null(loc_geom_samples)){
       m <- m%>%
         markers_simplifier(map = m, sp_df = loc_geom_samples, group_name = "Samples", fill_color = "#00BFFF")
       groups_string <- c(groups_string,"Samples")
       }
     if (!is.null(loc_geom_sensors)){
       m <- m%>%
         markers_simplifier(map = m, sp_df = loc_geom_sensors, group_name = "Sensors", fill_color = "#DF0101")
       groups_string <- c(groups_string,"Sensors")
       }
     if (!is.null(loc_geom_survey)){
       m <- m%>%
        markers_simplifier(map = m, sp_df = loc_geom_survey, group_name = "Survey", fill_color = "#FFFFFF")
       groups_string <- c(groups_string,"Survey")
       }
     
     #test_list <- c("Sensors", "Photo","Samples","Survey")
     m <- m%>%
       #addPolygons()%>%
        addLayersControl(
        baseGroups = c("OpenStreetMap","ESRI Aerial","Relief"),
        overlayGroups = c("All Locations", groups_string),
        options = layersControlOptions(collapsed = T)
      ) %>%
       hideGroup(groups_string)
       #setView(lng = coordinates_spliter("Gis vrt")[1], lat=coordinates_spliter("Gis vrt")[2], zoom=8)
    m
  })
  #Zoom to location on location selection
  # observeEvent(input$slct_location, {
  #   long_lat_slctd <- coordinates_spliter(input$slct_location)
  #   proxy <- leafletProxy("mymap")
  #   proxy %>% setView(input$mymap, lng = long_lat_slctd[1], lat=long_lat_slctd[2], zoom=9)
  # },ignoreInit = T)
  
  # output$info_table <- DT::renderDataTable({
  #   DT::datatable(availability_table("013733E045998N", "sensors"))
  #   })
  
  #mymap_copy <-leafletProxy("mymap")
  observeEvent(input$big_squares, {
    #long_lat_slctd <- coordinates_spliter(input$slct_location)

    if(input$big_squares){
      big_squares <- readOGR(paste(path_to_space,"shapefiles/big_squares/grid_4degrees_wgs84.shp", sep="/"))
      big_squares <- spTransform(big_squares, CRS("+init=epsg:4326"))
      #proxy <- leafletProxy("mymap")
      leafletProxy("mymap") %>% addPolygons(data = big_squares, stroke = TRUE, fillColor = "blues", fill = TRUE, layerId = unique(6400)) #, popup = ~name
    }
    else{
      leafletProxy("mymap") %>% addPolygons(data = big_squares, stroke = TRUE, fillColor = "blues", fill = TRUE, layerId = unique(6400))

    }

  },ignoreInit = T)

  #test <- c("Krucmanove konte", "Gropajski bori")
  #slcted_datatype <- c("sensors")
  observeEvent(input$infoBtn, {
    #std_name <- lookup_locations$location_names[which(lookup_locations$common_name %in% test)]
    std_name <- lookup_locations$location_names[which(lookup_locations$common_name %in% input$slct_location)]
    slcted_datatype <- input$slct_datatype
    
    #we'll first create the dataframe, then use it to generate the output table
    #looping through the list of locations, apply the function and return a binded dataframe
    df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("location_names","device","list_param","list_param_long","date_min", "date_max","step","datatype"))
    for (i in std_name){
      for (j in slcted_datatype){
        new <- availability_table_simpl(i, j)
        if (nrow(new) != 0) df <- rbind(df, new)
      }
    }
    #test <- as.data.frame(c("location_names","device","list_param","date_min", "date_max","step"),c(NA,NA,NA,NA,NA,NA))
    
    #################
    ####
    #min(as.yearmon(c("2018-01","2009-02")))
    ####
    ################
    
    #df <- availability_table_simpl(std_name, slcted_datatype)
    #df$year_month <- paste(year(df$year_month),sprintf("%02d",month(df$year_month)), sep="-")
    
    #let's get the starting date and ending dates from the table before transforming the format of the date
    #initializing start and end dates so that when the button is clicked without any selection, we don't get an error
    start_date <- end_date <- Sys.Date()
    try({
        start_date <- paste(year(min(df$date_min)),sprintf("%02d",month(min(df$date_min))),"01", sep="-");
        end_date <- paste(year(min(df$date_max)),sprintf("%02d",month(min(df$date_max))),"01", sep="-")
    }, silent = TRUE)
    
    #formating the year-month columns to a proper format for display since the yearmon object doesn't display in DT rendertable...
    df$date_min <- paste(year(df$date_min),sprintf("%02d",month(df$date_min)), sep="-")
    df$date_max <- paste(year(df$date_max),sprintf("%02d",month(df$date_max)), sep="-")
    df <- merge(df,lookup_locations,by="location_names")
    #df$year_month <- as.yearmon(df$year_month)
    
    #Now let's put together all parameters and collect a unique list of them
    all_param <- paste(df$list_param,sep=",")
    unique_param <- unique(unlist(strsplit(all_param, ", ")))
    
    all_param_long <- paste(df$list_param_long,sep=",")
    unique_param_long <- unique(unlist(strsplit(all_param_long, ", ")))
    
    # output$filter_param <- renderText({
    #   unique_param
    # })
    
    # output$filter_param <- renderText({
    #   unique_param
    # })
    
    # output$test <- renderUI({
    #   p(list_param_long)
    # })
    # 
    output$filters_wellpanel <- renderUI({
      tabsetPanel(
        tabPanel("Parameters",
                 wellPanel(style = "background: white", 
                        tags$head(tags$style(HTML(".multicol{font-size:12px;
                                                  height:auto;
                                                  -webkit-column-count: 7;
                                                  -moz-column-count: 7;
                                                  column-count: 7;
                                                  }"))),
                        tags$div(align = "left", 
                                 class = "multicol",
                        checkboxGroupInput(
                          inputId = "filter_param_check",
                          #paste0("checkboxfood_", i),
                          label = NULL,
                          #choices = c(sort(unique_param)),
                          inline = TRUE,
                          choiceNames = as.character(unique_param_long), ### for some reason it likes characters, not factors with extra levels
                          choiceValues = as.character(unique_param)
                          #selected = selected_ids
                        )
                      )
                      )
          ),
        tabPanel("date/time",
                 
                   fluidPage(style = "background: white",
                             fluidRow(
                             column(2,                          
                                    dateInput(inputId='datestart_filter', width="100%",label = 'Choose start date:', value = start_date), #Sys.Date()),
                                    dateInput(inputId='dateend_filter', width="100%",label = 'Choose end date:', value = end_date) #Sys.Date())
                                    ),
                             column(3,                          
                                    timeInput("timestart_filter", "Choose start time", value =  strptime("00:00:00", "%T")),
                                    timeInput("timeend_filter", "Choose end time", value =  strptime("00:00:00", "%T"))
                             )
                             )
                          )
                           
                 )
    )
    })
    
    # df_infotable <- df
    # df_infotable$
    
    #rendering df to the info_table already defined in ui.R
    output$info_table <- DT::renderDataTable({
      #df[,(!names(df)=="list_param_long")] this is aimed at removing the long names of parameters from the table
      DT::datatable(df[,(!names(df)=="list_param_long")], style ='bootstrap', class = "compact", filter="top")%>%
        formatStyle(colnames(df[,(!names(df)=="list_param_long")])[1:ncol(df[,(!names(df)=="list_param_long")])],  color = 'black', backgroundColor = 'white', fontWeight = 'normal')
    })
  }, ignoreInit = F)
}