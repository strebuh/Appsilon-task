# Data overview
library(data.table)
library(stringi)

# =====================================================================================================================================
# data <- fread("data/ships.csv")
# data <- fread(cmd = 'unzip -p ships_04112020.zip')
data <- fread(unzip("data/ships_04112020.zip"))


dim(data)
# [1] 3 102 887      20

names(data)
# [1] "LAT"         "LON"         "SPEED"       "COURSE"      "HEADING"     "DESTINATION" "FLAG"        "LENGTH"      "SHIPNAME"    "SHIPTYPE"    "SHIP_ID"    
# [12] "WIDTH"       "DWT"         "DATETIME"    "PORT"        "date"        "week_nb"     "ship_type"   "port"        "is_parked"  

head(data)
#         LAT      LON SPEED COURSE HEADING DESTINATION FLAG LENGTH SHIPNAME SHIPTYPE SHIP_ID WIDTH  DWT             DATETIME   PORT       date week_nb ship_type
# 1: 54.77127 18.99692    99    200     196      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:29:01Z gdansk 2016-12-19      51     Cargo
# 2: 54.76542 18.99361   100    200     196      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:31:02Z gdansk 2016-12-19      51     Cargo
# 3: 54.76007 18.99059   102    196     196      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:33:02Z gdansk 2016-12-19      51     Cargo
# 4: 54.75468 18.98753   102    198     196      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:35:02Z gdansk 2016-12-19      51     Cargo
# 5: 54.74926 18.98447   102    196     195      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:37:02Z gdansk 2016-12-19      51     Cargo
# 6: 54.74385 18.98150   102    198     196      GDANSK   MT    100   KAROLI        7    2764    14 5727 2016-12-19T11:39:01Z gdansk 2016-12-19      51     Cargo
# port is_parked
# 1: GdaÅ„sk         0
# 2: GdaÅ„sk         0
# 3: GdaÅ„sk         0
# 4: GdaÅ„sk         0
# 5: GdaÅ„sk         0
# 6: GdaÅ„sk         0

uniqueN(data$ship_type)
# [1] 9

unique(data$ship_type)
# [1] "Cargo"        "Tanker"       "Unspecified"  "Tug"          "Fishing"      "Passenger"    "Pleasure"     "Navigation"   "High Special"

uniqueN(data$SHIP_ID)
# [1] 1210

# User can select a vessel type (Ship_type) from the dropdown field
# User can select a vessel from a dropdown field (available vessels should correspond to the selected type). Dropdown fields should be created as a Shiny module
# For the vessel selected, find the observation when it sailed the longest distance between two consecutive observations. If there is a situation when a vessel moves exactly the same amount of meters, please select the most recent.  
# Display that on the map - show two points, the beginning and the end of the movement. The map should be created using the leaflet library. Changing type and vessel name should re-render the map and the note.
# Provide a short note saying how much the ship sailed - distance should be provided in meters.

# =====================================================================================================================================

saveRDS(unique(data$ship_type), "data/vessel_types.RDS")
vessel_types <- readRDS("data/vessel_types.RDS")

vessels <- lapply(unique(data$ship_type), function(x){
  unique(data[ship_type == x,]$SHIPNAME)
})
names(vessels) <- unique(data$ship_type)

# =====================================================================================================================================
# Create supplemental id
data[,id:=paste0(SHIPNAME, "_", ship_type, "_", FLAG, "_", LENGTH)]

# Does number of names match number of IDs for each type
data[,.(n_names=uniqueN(SHIPNAME),
        n_ship_id=uniqueN(SHIP_ID),
        n_id=uniqueN(id)), ship_type]
#       ship_type n_names n_ship_id n_id
# 1:        Cargo     381       379  382
# 2:       Tanker     175       175  175
# 3:  Unspecified      39        34   39
# 4:          Tug     266       284  273
# 5:      Fishing     180       179  181
# 6:    Passenger      65        65   66
# 7:     Pleasure      86        85   87
# 8:   Navigation       7         7    7
# 9: High Special       3         3    3


# # Transform datetime
# data[,DATETIME:=strptime(stri_sub(stri_replace_all_regex(DATETIME, "T"," "), 1, -2), "%Y-%m-%d %H:%M:%S")]
# setorder(data, ship_type, SHIPNAME, FLAG, LENGTH, DATETIME)
# 
# distances <- raster::pointDistance(data[, c("LON", "LAT")],
#                                    data[, .(LON=data.table::shift(LON, type="lead"), LAT=data.table::shift(LAT, type="lead"))],
#                                    lonlat=TRUE,
#                                    allpairs=FALSE)
# data[,DIST_M:=distances]
# data[nrow(data), DIST_M:=0]
# summary(data$DIST_M)
# # Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# # 0.0       0.0       0.0     245.8       1.6 1111269.9 
# 
# max_dist = data[which.max(DIST_M),]
# which_max_dist <- which(data$ship_type==max_dist$ship_type & data$SHIPNAME==max_dist$SHIPNAME & data$DATETIME==max_dist$DATETIME)
# data[c(which_max_dist-1, which_max_dist, which_max_dist+1),]

# odleglosci musze liczyc w grupach zeby wyszly ok

data2 <- data[, .(LAT=LAT,
                  LON=LON,
                  DATETIME=DATETIME,
                  LAT2=data.table::shift(LAT, type="lead"),
                  LON2=data.table::shift(LON, type="lead"),
                  DATETIME2=data.table::shift(DATETIME, type="lead")),
               by=c("ship_type", "SHIPNAME", "FLAG", "LENGTH")]
distances <- raster::pointDistance(data2[, c("LON", "LAT")],
                                   data2[, c("LON2", "LAT2")],
                                   lonlat=TRUE,
                                   allpairs=FALSE)
data2[,DIST_M:=distances]

# data3 <- data[, .(LAT=LAT,
#                   LON=LON,
#                   DATETIME=DATETIME,
#                   LAT2=data.table::shift(LAT, type="lead"),
#                   LON2=data.table::shift(LON, type="lead"),
#                   DATETIME2=data.table::shift(DATETIME, type="lead")),
#               by=c("id")]
# distances <- raster::pointDistance(data3[, c("LON", "LAT")],
#                                    data3[, c("LON2", "LAT2")],
#                                    lonlat=TRUE,
#                                    allpairs=FALSE)
# data3[,DIST_M:=distances]

# Check the viability
max_dist = data2[which.max(DIST_M),]
which_max_dist <- which(data2$ship_type==max_dist$ship_type & data2$SHIPNAME==max_dist$SHIPNAME & data2$DATETIME==max_dist$DATETIME)
data2[c(which_max_dist-1, which_max_dist, which_max_dist+1),]
setorder(data2, ship_type, SHIPNAME, FLAG, LENGTH,  DIST_M, DATETIME)

# Save the data
data_final <- data2[,.SD[.N], by=.(ship_type, SHIPNAME, FLAG, LENGTH)]
saveRDS(data_final, "data/prep_data.RDS")

data_final[,id:=paste0(SHIPNAME, "_", ship_type, "_", FLAG, "_", LENGTH)]

data_final[,TIME_LEN:=difftime(DATETIME2, DATETIME, units = "auto")]
data_final[,TIME_UNIT:=units(TIME_LEN)]



paste("1", data_final[1212,]$TIME_LEN, data_final[1212,]$TIME_UNIT)

paste("1", difftime(data_final[1212,]$DATETIME2, data_final[1212,]$DATETIME), data_final[1212,]$TIME_UNIT)

head(data_final)
str(data_final)

# =====================================================================================================================================
library(leaflet)
# mapa
which_max_dist <- which(vessel_data$DIST_M == max(vessel_data$DIST_M, na.rm=T))

leaflet(data = data3[1023,]) %>% 
  addTiles() %>% addMarkers(~LON, ~LAT, label = ~paste(as.character(round(DIST_M, 2)), "meters")) %>% 
  addMarkers(~LON2, ~LAT2)


