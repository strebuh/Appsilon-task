library(data.table)
library(stringi)

# =====================================================================================================================================
raw_data <- fread(unzip("data/ships_04112020.zip"), encoding = "UTF-8")
raw_data <- unique(raw_data)

dim(raw_data)
names(raw_data)
head(raw_data)
uniqueN(raw_data$ship_type)
unique(raw_data$ship_type)
uniqueN(raw_data$SHIP_ID)

# Does number of names match number of IDs for each type
raw_data[,.(n_names=uniqueN(SHIPNAME),
        n_ship_id=uniqueN(SHIP_ID),
        n_id=uniqueN(id)), ship_type]

# =====================================================================================================================================
# Ship typed for the app
saveRDS(unique(raw_data$ship_type), "data/vessel_types.RDS")

# =====================================================================================================================================

# Correct timestamp format
if(class(raw_data$DATETIME)[1]=="character"){
  start <- Sys.time()
  raw_data[,DATETIME:=strptime(stri_sub(stri_replace_all_regex(DATETIME, "T"," "), 1, -2), "%Y-%m-%d %H:%M:%S")]
  message(paste("Time stamp correction took ", Sys.time() - start))
}


# Order the data by the ship and timestamp
setorder(raw_data, SHIP_ID, DATETIME) # ship_type, SHIPNAME, FLAG, LENGTH


# Data for calculating distances
data_dist <- raw_data[, .(LAT=LAT,
                          LON=LON,
                          SPEED=SPEED,
                          SHIPNAME=SHIPNAME,
                          ship_type=ship_type,
                          FLAG=FLAG,
                          DWT=DWT,
                          LENGTH=LENGTH,
                          WIDTH=WIDTH,
                          DATETIME=DATETIME,
                          LAT2=data.table::shift(LAT, type="lead"),
                          LON2=data.table::shift(LON, type="lead"),
                          DATETIME2=data.table::shift(DATETIME, type="lead")),
                      by="SHIP_ID"]
distances <- raster::pointDistance(data_dist[, c("LON", "LAT")],
                                   data_dist[, c("LON2", "LAT2")],
                                   lonlat=TRUE,
                                   allpairs=FALSE)
data_dist[,DIST_M:=distances]
data_dist <- unique(data_dist)


# Agregates
data_by_type <- data_dist[, .(MED_LENGTH=median(as.numeric(LENGTH), na.rm=TRUE),
                               MED_WIDTH=median(as.numeric(WIDTH), na.rm=TRUE),
                               MED_DWT=median(as.numeric(DWT), na.rm=TRUE),
                               AVG_LENGTH=round(mean(LENGTH, na.rm=TRUE), 2),
                               AVG_WIDTH=round(mean(WIDTH, na.rm=TRUE), 2),
                               AVG_DWT=round(mean(DWT, na.rm=TRUE), 2),
                               AVG_SPEED=round(mean(SPEED, na.rm=TRUE), 2),
                               MAX_SPEED=round(max(SPEED, na.rm=TRUE), 2),
                               AVG_STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T)/uniqueN(SHIP_ID), 2)),
                           by="ship_type"]
saveRDS(data_by_type, "data/data_by_type.RDS")

data_by_country <- data_dist[, .(MED_LENGTH=median(as.numeric(LENGTH), na.rm=TRUE),
                              MED_WIDTH=median(as.numeric(WIDTH), na.rm=TRUE),
                              MED_DWT=median(as.numeric(DWT), na.rm=TRUE),
                              AVG_LENGTH=round(mean(LENGTH, na.rm=TRUE), 2),
                              AVG_WIDTH=round(mean(WIDTH, na.rm=TRUE), 2),
                              AVG_DWT=round(mean(DWT, na.rm=TRUE), 2),
                              AVG_SPEED=round(mean(SPEED, na.rm=TRUE), 2),
                              MAX_SPEED=round(max(SPEED, na.rm=TRUE), 2),
                              AVG_STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T)/uniqueN(SHIP_ID), 2)),
                          by="FLAG"]
saveRDS(data_by_country, "data/data_by_country.RDS")


# Order according to the length, and secondary to the time
setorder(data_dist, SHIP_ID, DIST_M, DATETIME, na.last = F)

# Select records with longest distance and latest
data_by_ship <- data_dist[,.SD[.N], by="SHIP_ID"]
data_by_ship[,TIME_LEN:=difftime(DATETIME2, DATETIME, units = "auto")]

# Prapare average values by ship
data_by_ship2 <- data_dist[, .(AVG_SPEED=round(mean(SPEED, na.rm=TRUE),2),
                              MAX_SPEED=round(max(SPEED, na.rm=TRUE),2),
                              TOT_DIST=round(sum(DIST_M, na.rm = T), 2),
                              STAND_TIME=round(sum(ifelse(DIST_M==0, difftime(DATETIME2, DATETIME), 0), na.rm = T), 2)),
                          by="SHIP_ID"]
data_by_ship <- merge(data_by_ship, data_by_ship2, by="SHIP_ID")

# Check for duplicated
data_by_ship[SHIPNAME %in% data_by_ship[,.N, .(SHIPNAME, ship_type)][N>1,]$SHIPNAME,]

# Save the data
saveRDS(data_by_ship, "data/data_by_ship.RDS")
