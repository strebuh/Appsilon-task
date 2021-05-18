# Data overview
library(data.table)
library(stringi)

# =====================================================================================================================================
# data <- fread("data/ships.csv")
# data <- fread(cmd = 'unzip -p ships_04112020.zip')
raw_data <- fread(unzip("data/ships_04112020.zip"))


dim(raw_data)
# [1] 3 102 887      20

names(raw_data)
# [1] "LAT"         "LON"         "SPEED"       "COURSE"      "HEADING"     "DESTINATION" "FLAG"        "LENGTH"      "SHIPNAME"    "SHIPTYPE"    "SHIP_ID"    
# [12] "WIDTH"       "DWT"         "DATETIME"    "PORT"        "date"        "week_nb"     "ship_type"   "port"        "is_parked"  

head(raw_data)
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

uniqueN(raw_data$ship_type)
# [1] 9

unique(raw_data$ship_type)
# [1] "Cargo"        "Tanker"       "Unspecified"  "Tug"          "Fishing"      "Passenger"    "Pleasure"     "Navigation"   "High Special"

uniqueN(raw_data$SHIP_ID)
# [1] 1210

# User can select a vessel type (Ship_type) from the dropdown field
# User can select a vessel from a dropdown field (available vessels should correspond to the selected type). Dropdown fields should be created as a Shiny module
# For the vessel selected, find the observation when it sailed the longest distance between two consecutive observations. If there is a situation when a vessel moves exactly the same amount of meters, please select the most recent.  
# Display that on the map - show two points, the beginning and the end of the movement. The map should be created using the leaflet library. Changing type and vessel name should re-render the map and the note.
# Provide a short note saying how much the ship sailed - distance should be provided in meters.

# Does number of names match number of IDs for each type
raw_data[,.(n_names=uniqueN(SHIPNAME),
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


# =====================================================================================================================================

saveRDS(unique(raw_data$ship_type), "data/vessel_types.RDS")
vessel_types <- readRDS("data/vessel_types.RDS")

vessels <- lapply(unique(raw_data$ship_type), function(x){
  unique(raw_data[ship_type == x,]$SHIPNAME)
})
names(vessels) <- unique(raw_data$ship_type)

# =====================================================================================================================================
raw_data <- fread(unzip("data/ships_04112020.zip"), encoding = "UTF-8")
str(raw_data)
raw_data <- unique(raw_data)

# Correct timestamp format
if(class(raw_data$DATETIME)[1]=="character"){
  start <- Sys.time()
  raw_data[,DATETIME:=strptime(stri_sub(stri_replace_all_regex(DATETIME, "T"," "), 1, -2), "%Y-%m-%d %H:%M:%S")]
  message(paste("Time stamp correction took ", Sys.time() - start))
}

# Create supplemental id
raw_data[,id:=paste0(SHIPNAME, "_", ship_type, "_", FLAG, "_", LENGTH)]

# Order the data by the ship and timestamp
setorder(raw_data, id, DATETIME) # ship_type, SHIPNAME, FLAG, LENGTH

data2 <- raw_data[, .(SHIP_ID=SHIP_ID,
                      LAT=LAT,
                      LON=LON,
                      SPEED=SPEED,
                      DWT=DWT,
                      LENGTH=LENGTH,
                      WIDTH=WIDTH,
                      DESTINATION=DESTINATION,
                      DATETIME=DATETIME,
                      LAT2=data.table::shift(LAT, type="lead"),
                      LON2=data.table::shift(LON, type="lead"),
                      DATETIME2=data.table::shift(DATETIME, type="lead")),
                  by=c("id")][,c("SHIPNAME", "ship_type", "FLAG", "LENGTH") := tstrsplit(id, "_", fixed=TRUE)]

distances <- raster::pointDistance(data2[, c("LON", "LAT")],
                                   data2[, c("LON2", "LAT2")],
                                   lonlat=TRUE,
                                   allpairs=FALSE)
data2[,DIST_M:=distances]
data2 <- unique(data2)


# Prepare data agregates

# ship type level
data_types <- data2[, .(MED_LENGTH=median(as.numeric(LENGTH), na.rm=TRUE),
                       MED_WIDTH=median(as.numeric(WIDTH), na.rm=TRUE),
                       MED_DWT=median(as.numeric(DWT), na.rm=TRUE), 
                       AVG_LENGTH=round(mean(LENGTH, na.rm=TRUE), 2),
                       AVG_WIDTH=round(mean(WIDTH, na.rm=TRUE), 2),
                       AVG_DWT=round(mean(DWT, na.rm=TRUE), 2),
                       AVG_SPEED=round(mean(SPEED, na.rm=TRUE), 2),
                       MAX_SPEED=round(max(SPEED, na.rm=TRUE), 2),
                       AVG_STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T)/uniqueN(id), 2)), 
                   by="ship_type"]
saveRDS(data_types, "data/data_types.RDS")

# country level
data_country <- data2[, .(MED_LENGTH=median(as.numeric(LENGTH), na.rm=TRUE),
                         MED_WIDTH=median(as.numeric(WIDTH), na.rm=TRUE),
                         MED_DWT=median(as.numeric(DWT), na.rm=TRUE), 
                         AVG_LENGTH=round(mean(LENGTH, na.rm=TRUE), 2),
                         AVG_WIDTH=round(mean(WIDTH, na.rm=TRUE), 2),
                         AVG_DWT=round(mean(DWT, na.rm=TRUE), 2),
                         AVG_SPEED=round(mean(SPEED, na.rm=TRUE), 2),
                         MAX_SPEED=round(max(SPEED, na.rm=TRUE), 2),
                         AVG_STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T)/uniqueN(id), 2)), 
                     by="FLAG"]
saveRDS(data_country, "data/data_country.RDS")


data_ships <- data2[, .(SHIP_ID=unique(SHIP_ID),
                        WIDTH=unique(WIDTH),
                        DWT=unique(DWT),
                        AVG_SPEED=round(mean(SPEED, na.rm=TRUE),2),
                        MAX_SPEED=round(max(SPEED, na.rm=TRUE),2),
                        TOT_DIST=round(sum(DIST_M, na.rm = T), 2),
                        STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T), 2)),
                    by=c("id")][,c("SHIPNAME", "ship_type", "FLAG", "LENGTH") := tstrsplit(id, "_", fixed=TRUE)]
columns <- names(data_ships)[c(1:2,9:12,3:8)]
data_ships <- data_ships[,..columns]
saveRDS(data_ships, "data/data_ships.RDS")

data_ships[,.N, .(SHIPNAME, ship_type)][N>1,]$SHIPNAME
data_ships[SHIPNAME %in% data_ships[,.N, .(SHIPNAME, ship_type)][N>1,]$SHIPNAME,]

# # Check the viability of the data
# max_dist = data2[which.max(DIST_M),]
# which_max_dist <- which(data2$ship_type==max_dist$ship_type & data2$SHIPNAME==max_dist$SHIPNAME & data2$DATETIME==max_dist$DATETIME)
# data2[c(which_max_dist-1, which_max_dist, which_max_dist+1),]

# test na jak input jest wektorem 

# Order according to the length, and secondary to the time
# setorder(data2, ship_type, SHIPNAME, FLAG, LENGTH,  DIST_M, DATETIME)
setorder(data2, id, DIST_M, DATETIME, na.last = F)

data2[id=="KAROLI_Cargo_MT_100",]
data2[id=="REDUT_Tug_RU_28",]
data2[id==data2$id[1],]

# Select records with longest distance and latest
data_final <- data2[,.SD[.N], by=.(ship_type, SHIPNAME, FLAG, LENGTH)]

summary(data_final$DIST_M, na.rm=T)
# data_final[,id:=paste0(SHIPNAME, "_", ship_type, "_", FLAG, "_", LENGTH)]
data_final[,TIME_LEN:=difftime(DATETIME2, DATETIME, units = "auto")]
data_final[,TIME_UNIT:=units(TIME_LEN)]

# Save the data
saveRDS(data_final, "data/prep_data.RDS")



# =====================================================================================================================================
library(leaflet)
# mapa
which_max_dist <- which(vessel_data$DIST_M == max(vessel_data$DIST_M, na.rm=T))

leaflet(data = data3[1023,]) %>% 
  addTiles() %>% addMarkers(~LON, ~LAT, label = ~paste(as.character(round(DIST_M, 2)), "meters")) %>% 
  addMarkers(~LON2, ~LAT2)


