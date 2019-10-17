#' IMU process
#'
#' This function calculates the corrections for imagery location given the roll, yaw, pitch, and elevation
#' @keywords 
#' @export
#' @examples



imu_proc <- function(imu.datafile,FOVAngle = 15.9619,GroundLevel=overallIMUmin,minAlt_dem_atminIMU,degree=TRUE,coords.epsg,dem_rast,YawCorrFactor=0,PitchCorrFactor=0,RollCorrFactor=0){
  
 tmp <- imu.datafile
 # tmp<-imu.framematch
 sp.tmpfordem <- SpatialPointsDataFrame(coords=tmp[,c("Lon","Lat")],data=tmp,proj4string = CRS(paste0("+init=epsg:",coords.epsg)))
sp.tmpTRfordem <- spTransform(sp.tmpfordem,"+init=epsg:32615")
imuTR <- as.data.frame(sp.tmpTRfordem[,!(names(sp.tmpTRfordem)%in%c("Lon","Lat"))])

 dem_alt <- raster::extract(dem_rast,imuTR[,c("Lon","Lat")],df=T)
 dem_alt$Frame. <- imuTR$Frame.
 dem_alt$DEMAlt <- dem_alt$layer
 tmp <- merge(imuTR,dem_alt,by="Frame.")
 # tmp <- merge(imu.framematch,dem_alt,by="Frame.")
 tmp$AdjGroundLevel <- GroundLevel+(tmp$DEMAlt-minAlt_dem_atminIMU)
 tmp$HeightAboveGround <- tmp$Alt-tmp$AdjGroundLevel
 tmp$HeightAboveGroundOLD <- tmp$Alt-GroundLevel
 
    if(degree==TRUE){
    tmp$FOVAngle <- degtorad(FOVAngle)
    tmp$Roll <- degtorad(tmp$Roll)
    tmp$Pitch <- degtorad(tmp$Pitch)
    tmp$Yaw <- degtorad(tmp$Yaw)
    }
 
 #at each point, this is the FOV
  tmp$UncorrectedFOVmeters <- 2*tmp$HeightAboveGround*tan(tmp$FOVAngle/2)
  
  ############## CORRECTIONS/OFFSETS

  tmp$Roll <- tmp$Roll + RollCorrFactor
  tmp$Pitch <- tmp$Pitch + PitchCorrFactor
  tmp$Yaw <- tmp$Yaw + YawCorrFactor
  
  tmp$RollCorrFactor <-  RollCorrFactor
  tmp$PitchCorrFactor <-  PitchCorrFactor
  tmp$YawCorrFactor <-  YawCorrFactor
  
  # TESTING (ignore)
  # tmp<-data.frame(nrow=1)
  # tmp$Roll <- degtorad(5)
  # tmp$Pitch <- degtorad(5)
  # tmp$Yaw <- degtorad(45)
  # tmp$HeightAboveGround<-50
  # tmp$FOVAngle <- degtorad(15.9619)


  
  #ROLL -- Negative sign added 1/3/2019 in order to account for the reversal of what happens to the drone and which way the sensor points!
   tmp$Rolloffset <- -tmp$HeightAboveGround*tan(tmp$Roll) #parallel to the frame's long direction (as specified by yaw), therefore:
   tmp$RollYComponent <- -tmp$Rolloffset*sin(tmp$Yaw) ###(FOR NOW REMOVING THIS NEGATIVE SIGN TO SEE HOW THINGS LOOK!!!!!)#### THAT WAS WRONG PUTTING IT BACK IN
   tmp$RollXComponent <- tmp$Rolloffset*cos(tmp$Yaw)
   
  #PITCH
  tmp$Pitchoffset <- tmp$HeightAboveGround*tan(tmp$Pitch) # orthogonal to the frame's long direction (as specified by the yaw), therefore:
  tmp$PitchYComponent <-tmp$Pitchoffset*cos(tmp$Yaw)
  tmp$PitchXComponent <-tmp$Pitchoffset*sin(tmp$Yaw)
  
  #TOTAL
  tmp$TotMidPointCorrectionY <- tmp$RollYComponent+tmp$PitchYComponent
  tmp$TotMidPointCorrectionX <- tmp$RollXComponent+tmp$PitchXComponent

  
  #Change in FOV length due to ROLL. Potential to add PITCH to this to make it even more accurate. If pitch is not 0, the distance away will be even farther.
  #The new distance will be the base of two right triangles (apex of each is the FOV angle + or - the Roll angle)
  
  #triangle 1
  Tri1ApexAngle <- 0.5*tmp$FOVAngle + tmp$Roll
  Tri1Base <- tmp$HeightAboveGround*tan(Tri1ApexAngle)
  #triangle 2
  Tri2ApexAngle <- 0.5*tmp$FOVAngle - tmp$Roll
  Tri2Base <- tmp$HeightAboveGround*tan(Tri2ApexAngle)

#if roll=0, these will be identical

#totalbase = FOV on the ground
tmp$RollCorrectedFOVmeters <- Tri1Base + Tri2Base

#Yaw offset -- used in finding the endpoints -- left side will use -x and +y, right side will use +x and -y (with +yaw) -- Or vice versa -- check on this later -- I believe I swapped this around in the later code.
tmp$YawYOffset<-0.5*tmp$RollCorrectedFOVmeters*sin(tmp$Yaw)
tmp$YawXOffset<- -0.5*tmp$RollCorrectedFOVmeters*cos(tmp$Yaw) #this is negative of y
  
sp.tmp <- SpatialPointsDataFrame(coords=tmp[,c("Lon","Lat")],data=tmp,proj4string = CRS("+init=epsg:32615"))

sp.out <- sp.tmp[,!(names(sp.tmp)%in%c("Lon","Lat"))]

out <- data.frame(sp.out)

return(out)


}
