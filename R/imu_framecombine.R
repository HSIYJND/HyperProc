#' IMU frame combine
#'
#' This function combines frame numbers with timestamps and calculates min IMU and location of min IMU to later match with DEM and correct for spatial variation in ground level.
#' @param folderloc where are the frame files located
#' @param imu_filename file name for imu_gps file, including extension .txt
#' @keywords 
#' @export
#' @examples
#' imu_framecombine()

imu_framecombine <- function(folderloc=paste0(RemoteSenDataLoc,FolderLoc),imu_filename="imu_gps.txt"){
  framelist <- list.files(folderloc,pattern = "frameIndex")
  framematch <- rbindlist(lapply(framelist,function(x){fi<-fread(paste0(folderloc,x))}))

#coords.epsg = "32615" is utm 15n which is what I want here. "4326" is lat lon

imu <- read.delim(paste0(folderloc,imu_filename))
# if(length(is.na(imu$Alt)>0)){imu <- imu[!is.na(imu$Alt),]}
# imuxy <- imu[,c("Lon","Lat")]
# imusp <- SpatialPointsDataFrame(coords=imuxy,data=imu,proj4string = CRS("+init=epsg:4326"))
# imusp <- spTransform(imusp,"+init=epsg:32615")
# imu <- as.data.frame(imusp[,!names(imusp) %in% c("Lon","Lat")])
# overallIMUmin <- min(imu$Alt)
# latMinIMU <- imu$Lat[imu$Alt==min(imu$Alt)]
# lonMinIMU <- imu$Lon[imu$Alt==min(imu$Alt)]
#
# assign("overallIMUmin", overallIMUmin, envir = .GlobalEnv)
# assign("latMinIMU", latMinIMU, envir = .GlobalEnv)
# assign("lonMinIMU", lonMinIMU, envir = .GlobalEnv)


setDT(imu,key="Timestamp")
setDT(framematch,key="Timestamp")
imu.framematch <- imu[framematch,roll=T]

return(imu.framematch)

}
