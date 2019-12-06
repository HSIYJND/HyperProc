#' Combine DEM file with IMU
#'
#' This function combines the imu/frame file with dem to calculate accurate height throughout the flight
#' @param imu processed imu/frame file
#' @param demfilelocation name and location of dem that is within local subfolder
#' @param latMin latitude of min elevation in imu - typically takeoff point
#' @param lonMin longitude of min elevation in imu - typically takeoff point
#' @keywords
#' @importFrom raster extract raster extent crs crop cv rasterize crs<- extent<- nrow<- ncol<-
#' @import rgdal 
#' @export
#' @examples
#' imu_demcombine()

imu_demcombine<-function(imu=imu.framematch,demfilelocation,latMin=latMinIMU,lonMin=lonMinIMU){

#don't need to do this again
imu.framexy <- imu[,c("Lon","Lat")]
imu.framesp <- SpatialPointsDataFrame(coords=imu.framexy,data=imu,proj4string = CRS("+init=epsg:32615"))
# imu.framesp <- spTransform(imu.framesp,"+init=epsg:32615")

dem1m <- readGDAL(paste0(demfilelocation))
dem1m <- spTransform(dem1m,"+init=epsg:32615")
dem_rel <- crop(dem1m,extent(imu.framesp)+40)
class(dem_rel)
dem_sf <- st_as_sf(dem_rel)
# st_write(dem_sf,dsn=paste0(LocalSource,"dem_BBFAB.shp"),layer="dem_BBFAB.shp",driver="ESRI Shapefile",delete_layer=TRUE)
rm(dem1m)
# dem_rel <- readOGR(paste0(LocalSource,"dem_BBFAB.shp"))
# dem_rel <- spTransform(dem_rel,"+init=epsg:32615")

rast <- raster()
extent(rast) <- extent(dem_rel) 
ncol(rast) <- round((extent(dem_rel)@xmax-extent(dem_rel)@xmin)/2)
nrow(rast) <- round((extent(dem_rel)@ymax-extent(dem_rel)@ymin)/2)

# And then ... rasterize it! This creates a grid version 
# of your points using the cells of rast, values from the IP field:
dem_rast <- rasterize(dem_rel, rast, dem_rel$band1, fun=mean) 
crs(dem_rast)<-crs(dem_rel)
minAlt_dem_atminIMU <- raster::extract(dem_rast,SpatialPoints(cbind(lonMin,latMin)),buffer=2,fun=mean,na.rm=T)
assign("minAlt_dem_atminIMU", minAlt_dem_atminIMU, envir = .GlobalEnv)

return(dem_rast)

}
