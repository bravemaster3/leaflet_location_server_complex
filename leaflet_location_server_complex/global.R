
#loading all required libraries
x <- c("shiny","shinyTime","leaflet","dplyr","tidyr","stringr","sf","markdown","zoo","data.table","DT","leaflet.extras", "rgdal", "sp")

#"leaflet.extras","tidyr","shinyTime"
lapply(x, require, character.only = TRUE)

#Here is the root path to location folders. This with the previous metadata are the paths to specify
path_to_space <- "/home/shiny_data/location_complex/geospatial"
path_to_locations <- "/home/shiny_data/location_complex/measurements"

lookup_locations <- read.table(paste(path_to_locations,"metadata.csv", sep="/"), header = TRUE, sep=",", encoding = "UTF-8")
#names(lookup_locations) <- c("location_names", "common_name")
lookup_locations$location_names <- as.character(lookup_locations$location_names)
lookup_locations$common_name <- as.character(lookup_locations$common_name)

#Function for retrieving all the location folders
list.dirs.depth.n <- function(p, n) {#Just specify the number of levels you want to go down in the hierarchy frim the root_path
  res <- list.dirs(p, recursive = FALSE)
  if (n > 1) {
    add <- list.dirs.depth.n(res, n-1)
    #c(add)
  } else {
    res
  }
}

# list.dirs.depth.n(root_path, n = 4)
#reading the directories to use it in the loc_geom_creator function
#location_dirs <- list.dirs(path = path_to_locations, full.names = FALSE, recursive = FALSE)
location_dirs <- list.dirs.depth.n(path_to_locations,3)
#location_names <- location_dirs

#The following function takes in a list of locations (obtained from a list.dirs()) and creates a geometry dataframe used in leaflets in server.R


loc_geom_creator <- function(dirs){
  
  #getting only the location names from the full path
  location_names <- str_sub(dirs,-16)
  #getting from standard location names, longitude, latitude and their signs from N/S or E/W. "T" in names is for test only
  loc_long <- substr(location_names,1,7)
  loc_long_sign <- substr(location_names,8,8)
  loc_lat <- substr(location_names,9,15)
  loc_lat_sign <- substr(location_names,16,16)
  
  #This line will create a dataframe from the previous information
  loc_geom <- data.frame(location_names, loc_long,loc_long_sign, loc_lat, loc_lat_sign)
  names(loc_geom) <- c("location_names", "longitude", "long_sign", "latitude", "lat_sign")
  
  #View(loc_geom)
  loc_geom$location_names <- as.character(levels(loc_geom$location_names))
  
  #let's create a dataframe to hold datatypes to be merged later on with the spatial dataframe
  datatype_perlocation <- data.frame(location_names)
  names(datatype_perlocation) <- "location_names"
  datatype_perlocation$datatype <- NA
  
  #This loop will create a comma separated column for datatype
  for (i in dirs){
    location_name <- str_sub(i,-16) #This will retrieve only the location name from a path of location_dirs
    #each_location_path <- paste(path_to_locations, i, sep="/")
    #here we read folder names in each location folder
    datatype <- list.dirs(path = i, full.names = FALSE, recursive = FALSE)
    datatype <- paste0(datatype, collapse = ",")
    datatype_perlocation$datatype[which(datatype_perlocation$location_names==location_name)] <- datatype
  }
  #loc_geom$common_name <- NA
  #loc_geom$common_name[which(lookup_locations$std_name %in% loc_geom$location_names)] <- lookup_locations$name[which(lookup_locations$std_name %in% loc_geom$location_names)]
 
  #The following is for merging loc_geom (table with geometry) with lookup locations (lookup to link standard names with common names and vice versa) and datatype list in each folder (created in th previous loop)
  loc_geom <- merge(x=loc_geom, y=lookup_locations, by="location_names")
  loc_geom <- merge(x=loc_geom, y=datatype_perlocation, by="location_names")
  #str(loc_geom)
  #The following will get us the real coordinate values, because till now it was just a string without comma. the 0.0005 is to have poitns at the center of the square
  loc_geom$longitude <- (as.numeric(levels(loc_geom$longitude))[loc_geom$longitude]/10000)+0.00025
  loc_geom$latitude <- (as.numeric(levels(loc_geom$latitude))[loc_geom$latitude]/10000)+0.00025
  
#!#!##The following 2 lines, remove later on the "T" which is only location for test purpose also assumed East here.
  loc_geom$longitude[which(loc_geom$long_sign %in% c("E","T"))] <- loc_geom$longitude[which(loc_geom$long_sign %in% c("E","T"))]
  loc_geom$longitude[which(loc_geom$long_sign == "W")] <- -loc_geom$longitude[which(loc_geom$long_sign == "W")]
  loc_geom$latitude[which(loc_geom$lat_sign %in% c("N","T"))] <- loc_geom$latitude[which(loc_geom$lat_sign %in% c("N","T"))]
  loc_geom$latitude[which(loc_geom$lat_sign == "S")] <- -loc_geom$latitude[which(loc_geom$lat_sign == "S")]
  
  #The following is to create a geometry column from longitude latitude, needed in the leaflet map
  loc_geom$geom <- NULL
  loc_geom$geom <- st_as_sf(loc_geom, coords = c("longitude", "latitude"), crs = 4326)
  return(loc_geom)
}


#loc_geom_creator(list_pertype_test)

#the following function is a generic function that will check the existence of the folder of the specified datatype, select the list of locations and run the log_geom creator() function on that list
loc_geom_pertype <- function(datatype){
  #str_sub(location_dirs,-16)
  #location_names <- location_dirs #list.dirs(path = path_to_locations, full.names = FALSE, recursive = FALSE)
  # print(location_names)
  list_pertype <- list()
  for (i in location_dirs){
    #print(paste(path_to_locations, i, datatype, sep = "/"))
    if(dir.exists(paste(i, datatype, sep = "/"))==TRUE) list_pertype <- unlist(append(list_pertype,i))
    }
  #list_pertype = unlist(list_pertype)
  #print(list_pertype)
  # list.dirs(path=)
  if (length(list_pertype)>0) loc_geom_creator(list_pertype)
  #print(list_pertype)
}

#list_pertype_test <- loc_geom_pertype("sensors")

#addcircle markers simplifier
#This is just a part of the leaflet renderer in server.R, and it has been put separately, in order to use less lines in the server.R
markers_simplifier <- function(map, sp_df, group_name, fill_color, ...) {
  addCircleMarkers(map, data=sp_df, lng = ~longitude, lat = ~latitude, group=group_name, radius = 4, opacity=1, fill=TRUE, color="black",
                 stroke=TRUE, fillOpacity = 1, weight=1, fillColor = fill_color,
                 clusterOptions = markerClusterOptions(),
                 popup = paste0(
                   "Location: <b>", sp_df$common_name, "</b><br>",
                   "Long: <b>", sp_df$longitude, "</b><br>",
                   "Lat: <b>", sp_df$latitude, "</b><br>",
                   "Datatypes: <b>", sp_df$datatype, "</b><br>"
                 ))
}

#The following function will split any standard location and return long, lat
coordinates_spliter <- function(location_name){
  #coordinates_spliter("Gropajski bori") #for testing
  std_name <- as.character(loc_geom_all$location_names[which(loc_geom_all$common_name == location_name)])
  long <- as.numeric(substr(std_name,1,7))/10000+0.00025
  long_sign <- substr(std_name,8,8)
  if (long_sign =="W"){
    long <- -long
  }
  # else if (long_sign %in% c("E","T")){
  #   long <- -long
  # }
  # 
  lat <- as.numeric(substr(std_name,9,15))/10000+0.00025
  lat_sign <- substr(std_name,16,16)
  if (lat_sign =="S"){
    lat <- -lat
  }
  # else if (lat_sign %in% c("N","T")){
  #   lat <- -lat
  # }
  return(c(long, lat))
}

#Data availability function
availability_table <- function(location_name, datatype="sensors"){
  root_path_loc_datype <- paste(path_to_locations, location_name, datatype, sep="/")
  #print(root_path_loc_datype)
  # list_files <- list.files(path = root_path_loc_datype,
  #                             pattern = glob2rx(paste("*", timestep, "*.csv", sep="")),
  #                             full.names = TRUE, recursive = TRUE)
  # 
  list_files <- list.files(path = root_path_loc_datype,
                           pattern = glob2rx("*.csv"),
                           full.names = TRUE, recursive = TRUE)
  #The number 57 represents the last character 
  list_files_names <- substr(list_files, 57, 1000)
  
  
  split <- strsplit(list_files_names, split="_")
  unlist_split <- unlist(split)
  df <- data.frame(matrix(unlist_split, nrow=length(split), byrow=T))
  #df$file_name <- paste(df[1:6], sep="_")
  df$name <- paste(df$X1, df$X2, df$X3, df$X4, df$X5, df$X6, sep="_")
  df$device <- paste(df$X1,df$X2,sep="_")
  df$step <- df$X3
  df$year_month <- as.yearmon(paste(df$X4,df$X5,sep="-"), format="%Y-%m")
  df[1:5] <- NULL
  names(df)[1] <- "location_names"
  df$location_names <- substr(df$location_names, 1,16)
  
  #This loop reads the first line of each file and inserts it into the param column of df
  for(i in 1:nrow(df)){
    headers <-readLines(paste(root_path_loc_datype,format(df$year_month[i],"%Y"),
                              format(df$year_month[i],"%m"), df[i,"name"], sep="/"), n=1)
    
    # headers <- readLines(paste("D:/shiny_server/location/013733E045998N/sensors",format(df$year_month[i],"%Y"), 
    #                            format(df$year_month[i],"%m"), df[i,"name"], sep="/"), n=1)
    headers <- noquote(headers)
    df$param[i] <- headers
  }
  #getting unique values of all parameters
  list_param <- unique(unlist(strsplit(as.character(df$param), ",")))
  
  #removing special characters (\" in this case)
  list_param <- gsub("[[:punct:]]", " ", list_param)
  #creating the columns from unique values in a loop
  for (i in 1:length(list_param)){
    col_name <- list_param[i]
    df[[col_name]] <- as.yearmon(NA)
    #names(df)[colnames(df)=="new"] <- i
    #df[,as.character(substitute(i))] <- NA
  }
  #New loop to fill each of the parameters columns with dates
  #nrow(df)
  for(i in 1:nrow(df)){
    for (j in 7:ncol(df)){
      #print(gsub("[[:punct:]]", " ",unlist(str_split(df$param[i], ","))))
      if (colnames(df)[j] %in% unlist(str_split(df$param[i], ","))){ #if(colnames(df)[j] %in% gsub("[[:punct:]]", " ",unlist(str_split(df$param[i], ",")))){
        df[i,j] <- df$year_month[i]
      }
      df[,j] <- as.yearmon(df[,j])
    }
  }
  #Creating a factor with order on the "step column" 
  df$step <- factor(df$step, order=TRUE, levels=c("1s","1","5","10","30","HOUR","DAY","MONTH"))
  
  df <- data.table(df)
  df <- df[, .SD[which.min(step)], by=c("device","year_month")]
  df <- df[,c("location_names","param","year_month")]
  df$year_month <- as.yearmon(df$year_month)
  return(as.data.frame(df))
  #print(unique(list_param))
}

#The simplified function is for retrieving only the information we want on the "explore" tab of the shiny app
#the function will be used for each combination of selected locations and datatypes, to get a table of availability

availability_table_simpl <- function(location_name, datatype){
  
  # location_name = "0165120E0464965N"
  # location_name = "013938E046367N"
  # datatype="sensors"
  location_dir <- grep(location_name, location_dirs, value=TRUE)
  
  #global parameter list file with description
  all_parameters <- read.table(paste(path_to_locations,"metadata_par.csv", sep="/"), header=TRUE, sep=";")
  
  #concatenate locationame and datatype to get complete path
  root_path_loc_datype <- paste(location_dir, datatype, sep="/")
  
  #check if the directory exists. Later, if not (ELSE, see later), we will return an empty table to avoid error
  if(dir.exists(root_path_loc_datype)==TRUE){
    #retrieving a list of all files in the directory location/datatype
    list_files <- list.files(path = root_path_loc_datype,
                             pattern = glob2rx("*.*"),
                             full.names = FALSE, recursive = TRUE)
#!#!##The number 57 represents the last character. So, we extract from character 57 to the end (which the file name).
    # file_name_start <- nchar(location_dir)+nchar(datatype)+11 #10 is for "/ before datatype, /YYYY/mm/"
    # 
    # list_files_names <- substr(list_files, file_name_start, 1000)
    
    list_files_names <- substr(list_files, 9, 1000) #9 is because 8 is the length of YYYY/mm/
    split <- strsplit(list_files_names, split="_") #this will split each part into a list
    unlist_split <- unlist(split) #flattens the list
    df <- data.frame(matrix(unlist_split, nrow=length(split), byrow=T)) #This is then converted into a dataframe
    #df$file_name <- paste(df[1:6], sep="_")
    df$name <- paste(df$X1, df$X2, df$X3, df$X4, df$X5, df$X6, sep="_") #Recreates the name from the different parts
    df$device <- paste(df$X1,df$X2,sep="_") #retrieves the device name only, which is the first 2 parts of thename
    df$step <- df$X3 #retrieves the step (1s, 1, 5, 10, 30, DAY, ...)
    df$year_month <- as.yearmon(paste(df$X4,df$X5,sep="-"), format="%Y-%m") #recreating year-month
    df[1:5] <- NULL #deleting all the columns we will not need anymore
    names(df)[1] <- "location_names" #renames the former "X6" column to location_names
    df$location_names <- substr(df$location_names, 1,16) #removes the extension "*.csv", extracts only the location names
    df$datatype <- datatype #to keep in the table the datatype
    #This loop reads the first line of each file and inserts it into the param column of df
    if (datatype %in% c("sensors", "samples", "survey")){#do the following only for csv files (alphanumeric)
      for(i in 1:nrow(df)){
        headers <-noquote(readLines(paste(root_path_loc_datype,format(df$year_month[i],"%Y"),
                                  format(df$year_month[i],"%m"), df[i,"name"], sep="/"), n=1)) #reads headers of each file and removes quotes
        #params
        df$param[i] <- headers #we add the list of headers to one single column, and later we split them
      }
      
      df$list_param <- NA
      df$list_param_long <- NA
      df$date_min <- as.yearmon(NA)
      df$date_max <- as.yearmon(NA)
      for (i in unique(df$device)){
        # i <- "045_IoTm"

        #let's get the equivalent long names of the files. This will require reading txt files (one per device) in each location, joining with the global parameters file
        #param_curr_loc_copy <- param_curr_loc <- read.table(paste(path_to_locations,location_name,paste(i,".txt",sep=""),sep="/"), stringsAsFactors = FALSE,header=TRUE, sep=",") #we created 2 variables here because we want to be able to use the copy next
        param_curr_loc_copy <- param_curr_loc <- read.table(paste(location_dir,paste(i,".txt",sep=""),sep="/"), stringsAsFactors = FALSE,header=TRUE, sep=",") #we created 2 variables here because we want to be able to use the copy next
        
        param_curr_loc_copy$parameter <- gsub('[.]',",", param_curr_loc_copy$parameter) #This replaces dots by comma for splitting in the next line. dots are not working as separator
        param_curr_loc$code <- separate(param_curr_loc_copy,parameter,into=paste0("code", 1:3),sep=",")[,1] #the aim here is to retrieve the first part of parameter name, which is equivalent to code column in the marameter list (i.e without description and agregation)
        param_curr_loc <- merge(param_curr_loc,all_parameters,by.x="code",by.y="code",all.x = TRUE,ALL.y=FALSE) #here now we join the metadata_par (or parameter_all) to the metadata of this specific current location, in order to have the possibility to write the complete name with units
        #creating the new column with the long name to use in the UI in parallel with the short names
        param_curr_loc$long_name <- paste(param_curr_loc$english.name, " - ",param_curr_loc$english.description, " - ", param_curr_loc$english.aggregation, sep="") #" - ", param_curr_loc$unit,      unit has been removed because otherwise it will create a longer list than the short parameter names    
        #These 2 lines will get the unique list of parameters for each device, and remove special characters (//")
        list_param <- unique(unlist(strsplit(as.character(df$param[which(df$device==i)]), ",")))
        list_param <- gsub('"', " ", list_param)
        
        list_param <- trimws(list_param, "b") #the trimws function removes additional white space from each parameter name. "b" is to remove both on the left and right
        #list_param_copy <- list_param
        #list_param_copy <- trimws(list_param_copy, "b")
        retrieval_param <- function(indiv_param){ #This function will take a value, match it with a column (col1), and return the equivalent from col2
          if (indiv_param %in% param_curr_loc$parameter)
            return(param_curr_loc$long_name[which(param_curr_loc$parameter==indiv_param)])
          else return(as.character(indiv_param))
        }
        
        list_param_long <- unlist(lapply(list_param,FUN=retrieval_param)) #this line applies the previous function to the list of parameters, in order to get their equivalent long names

        #################################################
        
        #list_param <- list_param[!list_param %in% "date"]
        
        #gsub("month,","", "this, month, is good")
        
        list_param <- str_c(list_param, collapse = ", ")
        list_param_long <- str_c(list_param_long, collapse = ", ")
        # list_param <- gsub("time,", "", list_param)
        # list_param <- gsub("month,", "", list_param)
        # list_param <- gsub("date,", "", list_param)
        # list_param <- gsub(",month", "", list_param)
        df$list_param[which(df$device==i)] <- list_param
        df$list_param_long[which(df$device==i)] <- list_param_long
        df$date_min[which(df$device==i)] <- as.yearmon(min(df$year_month[which(df$device==i)]))
        df$date_max[which(df$device==i)] <- as.yearmon(max(df$year_month[which(df$device==i)]))
      }
      
    
      #Creating a factor with order on the "step column" 
      df$step <- factor(df$step, order=TRUE, levels=c("1s","1","5","10","30","HOUR","DAY","MONTH"))
      
      df <- data.table(df)
      df <- df[, .SD[which.min(step)], by=c("device","year_month")]
      #df <- df[,c("location_names","device","param","list_param","year_month","date_min", "date_max")]
      df <- df[,c("location_names","device","list_param","list_param_long","date_min", "date_max","step","datatype")]
      
      #removing duplicated rows
      df <- df %>% distinct()
      #df$year_month <- as.yearmon(df$year_month)
      df$date_min <- as.yearmon(df$date_min)
      df$date_max <- as.yearmon(df$date_max)
    }
    else { #this is for photos only
      df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("location_names","device","list_param","list_param_long","date_min", "date_max","step","datatype"))
    }
  }
  else{
    #if the folder does not exist (example of different datatype, or no file with that name)
    df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("location_names","device","list_param","list_param_long","date_min", "date_max","step","datatype"))
  }
  return(as.data.frame(df))
  #print(unique(list_param))
}

#something <- availability_table_simpl("0165120E0464965N", "sensors")

#####################################################
#Applying function to obtain some global variables needed both in server.R and ui.R
loc_geom_all <- loc_geom_creator(location_dirs)
loc_geom_photo <- loc_geom_pertype("photo")
loc_geom_samples <- loc_geom_pertype("samples")
loc_geom_sensors <- loc_geom_pertype("sensors")
loc_geom_survey <- loc_geom_pertype("survey")



#View(availability_table_simpl("0137330E0459980N","sensors"))


