library(lubridate)
library(RSQLite)
library(sqldf)
require(dplyr)
library(sp)
library(rgdal)
library(fields)
library(rgeos)
library(maptools)
library(raster)
library(rdist)
library(plyr)
library(arm)


#============================================================================================
#                     READ IN AIRBNB LISTINGS 
#             Output: a5 - remove outliers, calculate price by guests 
#             Q     : Take average price grouped by room & host ids ? 
# ===========================================================================================

#read in data
years <- c("2014","2015","2016","2017")
data <- lapply(years, read_file)
all_data <- rbind(data[[1]], data[[2]], data[[3]], data[[4]])

#create Date column 
all_data$new_date <- as.Date(all_data$date, "%m/%d/%y", tz = "GMT")
all_data$year <- year(all_data$new_date)
all_data$month <- month(all_data$new_date)

#drop date column in text format and borough
drops <- c("date", "borough", "last_modified")
all_data <- all_data[, !(names(all_data) %in% drops)]
names(all_data)[names(all_data) =="new_date"] <- "date"

#select distinct listing and GIS coordinates
coords_a <- sqldf('select distinct room_id, host_id, latitude, longitude
                  from all_data 
                  order by room_id, host_id, latitude, longitude')
coords_a$rownum <- ave(coords_a$host_id, coords_a$room_id, FUN = seq_along)
coords_b <- sqldf('select * from coords_a where rownum = 1')

a <- sqldf('select all_data.*, coords_b.latitude as new_lat, coords_b.longitude as new_long from all_data inner join coords_b on
          all_data.room_id = coords_b.room_id and
          all_data.host_id = coords_b.host_id
          order by host_id, room_id, date')
c4 <- sqldf('select all_data.*, coords_b.latitude as new_lat, coords_b.longitude as new_long from all_data left join coords_b on
          all_data.room_id = coords_b.room_id and
            all_data.host_id = coords_b.host_id
            where coords_b.room_id is NULL and coords_b.host_id is NULL')
for (i in 1:nrow(c4)){
  if(c4$room_id[i] == 2777752){
    c4$host_id[i] <- 1362285
  } else if (c4$room_id[i] == 4410765){
    c4$host_id[i] <- 9795590
  } else if (c4$room_id[i] == 4560053){
    c4$host_id[i] <- 8229
  } else if (c4$room_id[i] == 5584915){
    c4$host_id[i] <- 6964793
  } else if (c4$room_id[i] == 6972426){
    c4$host_id[i] <- 36559227
  } else if (c4$room_id[i] == 8653238){
    c4$host_id[i] <- 45477148
  }
  c4$new_lat <- c4$latitude
  c4$new_long <- c4$longitude
}
a <- rbind(a, c4)
drops <- c("latitude", "longitude")
a <- a[, !(names(a) %in% drops)]
names(a)[names(a) =="new_lat"] <- "latitude"
names(a)[names(a) =="new_long"] <- "longitude"


#distinct rooms
a2 <- sqldf('select distinct host_id, room_id from a')


#distinct rooms with count grouped by room
a3 <- sqldf('select *, count(room_id) as ct from a group by room_id, host_id')
a4 <- sqldf('select a.*, a3.ct from a left join a3 on
            a.room_id = a3.room_id and
            a.host_id = a3.host_id')

#calculate price per guest 
  #filter NAs accommodates and bedrooms 
a5 <- subset(a4, subset = !(is.na(accommodates) & is.na(bedrooms)))

for (row in 1:nrow(a5)){
  if(is.na(a5[row, "accommodates"])){
    a5[row, "accommodates"] <- a5[row, "bedrooms"]
  } 
}

a5$pricePerGuest <- a5$price/a5$accommodates
  #remove outliers ($200 - 99th percentile)
a5 <- subset(a5, subset = pricePerGuest <= 200)
a6 <- sqldf("select distinct room_id, host_id, latitude, longitude from a5")
a7 <- sqldf("select distinct room_id, host_id, latitude, longitude, ct from a5")



#============================================================================================
#                     IMPORT PARCEL ID SHAPEFILE
#             Output: ß - all airbnb in all years matched with parcel ID 
#             
# ===========================================================================================

#import parcel IDs
parcelIDs <- readOGR("/Users/oanhdoan/Google Drive/Thesis/BostonData/Parcels_2016_Data_Full/Parcels_2016_Data_Full.shp")

#reproject Airbnb data

coordinates(a6) <- c("longitude", "latitude")
projection(a6) <- crs(parcelIDs)
airbnb_parcels <- over(a6, parcelIDs)
airbnb_ids_parcels <- cbind(airbnb_parcels, as.data.frame(a6))
airbnb_not_in_parcel <- subset(airbnb_ids_parcels, subset = is.na(PID))
airbnb_in_parcel <- subset(airbnb_ids_parcels, subset = !is.na(PID))
airbnb_not_in_parcel_ids <- airbnb_not_in_parcel[, c("room_id", "host_id", "latitude", "longitude")]

#buffer spatial points not in polygons
  #find centroids of parcels
centroids <- readOGR("/Users/oanhdoan/Google Drive/Thesis/BostonData/parcel_centroid.shp")
centroids_mat <- coordinates(centroids)
centroids_df <- as.data.frame(centroids_mat)
centroids_df$object_id <- centroids@data$OBJECTID
  #distance from each listing to each parcel 
airbnb_not_in_parcel_mat <- as.matrix(airbnb_not_in_parcel_ids[,c("longitude","latitude")])
dist <- cdist(airbnb_not_in_parcel_mat, centroids_mat)
df <- as.data.frame(dist)
colnames(df) <- centroids_df$object_id

  #find the nearest parcel 
for (row in 1:nrow(df)) {
   df$nearest_parcel[row] <- names(df[which.min(df[row,])])
   print(row)
}
write.csv(df, "/Users/oanhdoan/Google Drive/Thesis/BostonData/distance_to_parcel.csv" )
  #merge listing with nearest parcels
airbnb_not_in_parcel_final <- as.data.frame(df[, c("nearest_parcel")])
colnames(airbnb_not_in_parcel_final)[1] <- "OBJECTID"
airbnb_not_in_parcel_final$room_id <- airbnb_not_in_parcel_ids$room_id
airbnb_not_in_parcel_final$host_id <- airbnb_not_in_parcel_ids$host_id
airbnb_not_in_parcel_final$latitude <- airbnb_not_in_parcel_ids$latitude
airbnb_not_in_parcel_final$longitude <- airbnb_not_in_parcel_ids$longitude
  #create buffer dummy
airbnb_not_in_parcel_final$buffer <- 1
airbnb_in_parcel$buffer <- 0
  #save output
write.csv(airbnb_not_in_parcel_final,"/Users/oanhdoan/Google Drive/Thesis/BostonData/buffered_airbnb.csv")
airbnb_not_in_parcel_final <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/buffered_airbnb.csv")
parcels <- parcelIDs@data
airbnb_not_in_parcel_final_2 <- sqldf("select parcels.*, aa.room_id, aa.host_id, aa.latitude, aa.longitude, aa.buffer
                                      from airbnb_not_in_parcel_final as aa left join parcels on aa.OBJECTID = parcels.OBJECTID")
airbnb_not_in_parcel_final_2 <- sqldf("select parcels.*, aa.room_id, aa.host_id, aa.latitude, aa.longitude, aa.buffer 
                                      from airbnb_not_in_parcel_final as aa left join parcels on 
                                      aa.OBJECTID = parcels.OBJECTID")
  #combine buffered and non-buffered datasets
airbnb_parcels_final <- rbind(airbnb_in_parcel, airbnb_not_in_parcel_final_2)
airbnb_final <- sqldf("select aa.OBJECTID, aa.buffer, aaa.* 
                      from a5 as aaa left join airbnb_parcels_final as aa on
                      aaa.room_id = aa.room_id and 
                      aaa.host_id = aa.host_id and 
                      aaa.latitude = aa.latitude and 
                      aaa.longitude = aa.longitude")
airbnb_reg <- airbnb_final


#============================================================================================
#                       DISTANCE TO TRAIN STATION
#            
#             
# ===========================================================================================
  #calculate distance from each airbnb to each subway 
stations <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/subway/subway_stations.csv",header = T, sep =",")
temp_ids <- airbnb_final[, c("longitude", "latitude", "host_id", "room_id")]
temp_stations <- as.matrix(stations[, c("X", "Y")])
temp_dist <- as.data.frame(cdist(as.matrix(temp_ids[,c("longitude", "latitude")]), temp_stations))
  
  #distance to the nearest station
for (i in 1:nrow(temp_dist)){
  airbnb_final$min_station[i] <- min(temp_dist[i,])
}


#============================================================================================
#                     READ IN PROPERTY VALUES 
#             Output: a5 - remove outliers, calculate price by guests 
#             Q     : Take average price grouped by room & host ids ? 
# ===========================================================================================

#Read in property value files 
property_2014 <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/PropertyValue/property-assessment-fy2014_formatted.csv", stringsAsFactors = FALSE)
property_2015 <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/PropertyValue/property-assessment-fy2015_formatted.csv", stringsAsFactors = FALSE)
property_2016 <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/PropertyValue/property-assessment-fy2016_formatted.csv", stringsAsFactors = FALSE)
property_2017 <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/PropertyValue/property-assessment-fy2017_formatted.csv", stringsAsFactors = FALSE)
property_2014$year <- 2014
property_2015$year <- 2015
property_2016$year <- 2016
property_2017$year <- 2017

#extract parcels that are common to all years
common_cols <- intersect(colnames(property_2014), colnames(property_2015))
common_cols <- intersect(common_cols, colnames(property_2016))
common_cols <- intersect(common_cols, colnames(property_2017))

all_property <- rbind(
  subset(property_2014, select = common_cols),
  subset(property_2015, select = common_cols),
  subset(property_2016, select = common_cols),
  subset(property_2017, select = common_cols)
)
all_property <- all_property[, c("PID", "CM_ID", "PTYPE", "LU", "AV_TOTAL", "LAND_SF")]
all_property$AV_TOTAL[all_property$AV_TOTAL == 0] <- NA
all_property$LAND_SF[all_property$LAND_SF == 0] <- NA 
all_property$AV_PER_UNIT_TOTAL <- all_property$AV_TOTAL/all_property$LAND_SF
m1 <- count(unique(all_property$PID))
m2 <- count(unique(parcels_csv$PID))
m3 <- merge(m1, m2)


parcels_csv <- read.csv("/Users/oanhdoan/Google Drive/Thesis/BostonData/Parcels_2016_Data_Full/Parcels_2016_Data_Full.csv")
names(parcels_csv)[names(parcels_csv) =="PID_LONG"] <- "PID"
parcels_csv <- parcels_csv[, c("OBJECTID", "PID", "CM_ID", "PTYPE", "LU","AV_LAND", "AV_BLDG", "AV_TOTAL", "LAND_SF", "LIVING_AREA")]


all_property_objids <- sqldf('select a.*, b.OBJECTID from all_property as a left join parcels_csv as b on
                             a.PID = b.PID')
max(parcels_csv$PID)
m1 <- merge(parcels_csv$PID, all_property$PID)



#extract assessed values and area, replace 0 with NA
all_property$AV_LAND[all_property_objids$AV_LAND == 0] <- NA
all_property$AV_BLDG[all_property_objids$AV_BLDG == 0] <- NA
all_property$AV_TOTAL[all_property_objids$AV_TOTAL == 0] <- NA
all_property$LAND_SF[all_property_objids$LAND_SF == 0] <- NA 
all_property$LIVING_AREA[all_property_objids$LIVING_AREA==0] <- NA

#calculate parcel value per unit of land

final_1 <- sqldf('select a.*, b.PTYPE, b.LU, b.AV_PER_UNIT_TOTAL from airbnb_reg as a left join 
                 all_property as b on a.OBJECTID = b.OBJECTID and a.year = b.year')


















#=================================== FUNCTIONS ==============================================
read_file <- function(year){
  ayear = paste("*",year,"\\.csv$",sep="")
  print(ayear)
  file_list <- list.files(path = "/Users/oanhdoan/Google Drive/Thesis/BostonData/boston",
                          pattern = ayear,
                          all.files = T,
                          full.names = T)
  file_loaded<- lapply(file_list, 
                      FUN = function(i) {
                        read.csv(i, header=TRUE, sep=",")
                      })
  
  file_appended <- do.call("rbind", file_loaded)
  return (file_appended)
}

airbnb_reg$dec <- NA
airbnb_reg$feb <- NA
airbnb_reg$mar <- NA
airbnb_reg$apr <- NA
airbnb_reg$may <- NA
airbnb_reg$jun <- NA
airbnb_reg$jul <- NA
airbnb_reg$aug <- NA
airbnb_reg$sep <- NA
airbnb_reg$oct <- NA
airbnb_reg$nov <- NA


for (i in 1:nrow(airbnb_reg)){
  if(airbnb_reg$month[i] == 12){
    airbnb_reg$dec[i] = 1
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 2){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 1
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 3){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 1
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 4){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 1
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 5){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 1
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 6){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 1
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 7){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 1
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 8){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 1
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 9){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 1
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 10){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 1
    airbnb_reg$nov[i] = 0
  } else if(airbnb_reg$month[i] == 11){
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 1
  } else {
    airbnb_reg$dec[i] = 0
    airbnb_reg$feb[i] = 0
    airbnb_reg$mar[i] = 0
    airbnb_reg$apr[i] = 0
    airbnb_reg$may[i] = 0
    airbnb_reg$jun[i] = 0
    airbnb_reg$jul[i] = 0
    airbnb_reg$aug[i] = 0
    airbnb_reg$sep[i] = 0
    airbnb_reg$oct[i] = 0 
    airbnb_reg$nov[i] = 0
  }
} 